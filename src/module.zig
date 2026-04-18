const std = @import("std");
const frontend = @import("frontend/index.zig");
const Statement = frontend.Statement;
const Tree = frontend.Tree;
const Lexer = frontend.Lexer;
const Parser = frontend.Parser;
const Token = frontend.Token;

const backend = @import("backend/index.zig");
const Bytecode = backend.Bytecode;
const Compiler = backend.Compiler;
const CompilerErrors = backend.CompilerErrors;
const suggest = backend.suggest;

const Io = std.Io;

/// Group of files
/// Manages all allocations and clears all with deinit
pub const Module = struct {
    pub const Timings = struct {
        resolve_includes_ns: u64 = 0,
        parse_ns: u64 = 0,
        compile_ns: u64 = 0,
        total_ns: u64 = 0,
        file_count: usize = 0,
        source_bytes: usize = 0,
    };

    arena: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    errors: CompilerErrors,
    entry: *File,
    io: Io,
    dir: Io.Dir,
    dir_path: []const u8,
    includes: std.array_hash_map.String(*File),
    timings: Timings = .{},

    pub fn init(allocator: std.mem.Allocator, io: Io, entry_path: []const u8) !*Module {
        const mod = try allocator.create(Module);
        const path = std.fs.path.basename(entry_path);
        const dir_name = std.fs.path.dirname(entry_path) orelse return error.InvalidArgument;
        mod.* = .{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .io = io,
            .dir = try Io.Dir.openDirAbsolute(io, dir_name, .{}),
            .dir_path = try allocator.dupe(u8, dir_name),
            .entry = undefined,
            .includes = std.array_hash_map.String(*File).empty,
            .errors = CompilerErrors.init(allocator),
        };
        // Can't add entry until includes is initialized
        mod.entry = try mod.addFileAtPath(path);
        return mod;
    }

    /// Used for initializing with source directly rather than a file path
    pub fn initEmpty(allocator: std.mem.Allocator, io: Io) !*Module {
        const mod = try allocator.create(Module);
        mod.* = .{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .io = io,
            .dir = undefined,
            .dir_path = "",
            .entry = undefined,
            .includes = std.array_hash_map.String(*File).empty,
            .errors = CompilerErrors.init(allocator),
        };
        return mod;
    }

    /// Path must be relative to the Module root directory
    pub fn addFileAtPath(self: *Module, path: []const u8) !*File {
        if (self.includes.get(path)) |existing| return existing;
        const file = try File.create(self, path);
        try self.includes.putNoClobber(self.allocator, file.path, file);
        return file;
    }

    fn nowNs(self: *Module) i96 {
        return std.Io.Timestamp.now(self.io, .awake).nanoseconds;
    }

    pub fn generateBytecode(self: *Module, allocator: std.mem.Allocator) !Bytecode {
        const total_start = self.nowNs();
        var phase_start = total_start;
        // Capture whatever phases completed if we error out partway through.
        errdefer self.timings.total_ns = @intCast(self.nowNs() - total_start);

        // Phase 1: Resolve all includes and load sources
        try self.resolveIncludes();
        var now = self.nowNs();
        self.timings.resolve_includes_ns = @intCast(now - phase_start);
        phase_start = now;

        // Populate file/source counts now that all sources are loaded.
        self.timings.file_count = self.includes.count();
        var bytes: usize = 0;
        var src_it = self.includes.iterator();
        while (src_it.next()) |kvp| {
            if (kvp.value_ptr.*.source) |s| bytes += s.len;
        }
        self.timings.source_bytes = bytes;

        // Phase 2: Build parse trees for all files
        var it = self.includes.iterator();
        while (it.next()) |kvp| {
            try kvp.value_ptr.*.buildTree();
        }
        now = self.nowNs();
        self.timings.parse_ns = @intCast(now - phase_start);
        phase_start = now;

        // Phase 3: Compile
        var compiler = try Compiler.init(allocator, self);
        defer compiler.deinit();

        compiler.compile() catch |e| {
            return e;
        };
        const result = try compiler.bytecode();
        now = self.nowNs();
        self.timings.compile_ns = @intCast(now - phase_start);
        self.timings.total_ns = @intCast(now - total_start);
        return result;
    }

    const IncludeDirective = struct { path: []const u8, token: Token };

    /// Resolve all include directives transitively by scanning tokens.
    /// Loads source for each discovered file. After this completes,
    /// all files are in self.includes with their sources loaded.
    /// Detects circular includes and emits a compile error pointing at
    /// the offending include directive.
    pub fn resolveIncludes(self: *Module) !void {
        const alloc = self.arena.allocator();
        try self.entry.loadSource();
        var chain: std.ArrayList([]const u8) = .empty;
        defer chain.deinit(alloc);
        var visited = std.StringHashMap(void).init(alloc);
        defer visited.deinit();
        try self.resolveIncludesRec(self.entry, &chain, &visited);
    }

    fn resolveIncludesRec(
        self: *Module,
        file: *File,
        chain: *std.ArrayList([]const u8),
        visited: *std.StringHashMap(void),
    ) !void {
        if (visited.contains(file.path)) return;

        const alloc = self.arena.allocator();
        try chain.append(alloc, file.path);
        defer _ = chain.pop();

        const includes = try self.scanForIncludes(file);
        for (includes) |inc| {
            const resolved = try self.resolveIncludePath(file, inc.path);
            var is_cycle = false;
            for (chain.items) |p| {
                if (std.mem.eql(u8, p, resolved)) {
                    is_cycle = true;
                    break;
                }
            }
            if (is_cycle) {
                try self.addCycleError(chain.items, resolved, inc.token, file.path);
                // Still register the file so later lookups don't double-fail.
                _ = try self.addFileAtPath(resolved);
                continue;
            }
            const child = try self.addFileAtPath(resolved);
            child.loadSource() catch |e| switch (e) {
                error.FileNotFound => {
                    try self.addMissingIncludeError(file, inc, resolved);
                    continue;
                },
                else => return e,
            };
            try self.resolveIncludesRec(child, chain, visited);
        }
        try visited.put(file.path, {});
    }

    fn addCycleError(
        self: *Module,
        chain: []const []const u8,
        repeated: []const u8,
        token: Token,
        including_path: []const u8,
    ) !void {
        // Find the first occurrence of the repeated path in the chain
        // so we render just the cycle, not the prefix leading into it.
        var start: usize = 0;
        for (chain, 0..) |p, i| {
            if (std.mem.eql(u8, p, repeated)) {
                start = i;
                break;
            }
        }

        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(self.allocator);
        for (chain[start..]) |p| {
            try buf.appendSlice(self.allocator, std.fs.path.basename(p));
            try buf.appendSlice(self.allocator, " -> ");
        }
        try buf.appendSlice(self.allocator, std.fs.path.basename(repeated));
        const cycle_str = try buf.toOwnedSlice(self.allocator);
        defer self.allocator.free(cycle_str);

        const note = try self.allocator.dupe(
            u8,
            "remove one of the include directives, or break the cycle by moving shared content into a third file",
        );
        try self.errors.addWithHelp(
            including_path,
            "Circular include: {s}",
            token,
            .err,
            .{cycle_str},
            null,
            note,
        );
    }

    fn addMissingIncludeError(
        self: *Module,
        parent_file: *File,
        inc: IncludeDirective,
        resolved_path: []const u8,
    ) !void {
        const suggestion = try self.suggestIncludeFile(inc.path, resolved_path);

        const note = try self.allocator.dupe(
            u8,
            "include paths are resolved relative to the file containing the directive",
        );
        try self.errors.addWithHelp(
            parent_file.path,
            "Include file '{s}' not found",
            inc.token,
            .err,
            .{std.fs.path.basename(resolved_path)},
            suggestion,
            note,
        );
    }

    /// List sibling .topi files in the directory of the missing include
    /// and return a did-you-mean suggestion if one is close enough.
    fn suggestIncludeFile(self: *Module, raw_path: []const u8, resolved_path: []const u8) !?[]const u8 {
        const alloc = self.arena.allocator();
        const basename = std.fs.path.basename(resolved_path);
        const dirname = std.fs.path.dirname(resolved_path);

        var dir = if (dirname) |dn|
            self.dir.openDir(self.io, dn, .{ .iterate = true }) catch return null
        else
            self.dir.openDir(self.io, ".", .{ .iterate = true }) catch return null;
        defer dir.close(self.io);

        var names: std.ArrayList([]const u8) = .empty;
        defer {
            for (names.items) |n| alloc.free(n);
            names.deinit(alloc);
        }

        var iter = dir.iterate();
        while (iter.next(self.io) catch return null) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.name, ".topi")) continue;
            try names.append(alloc, try alloc.dupe(u8, entry.name));
        }

        const match = try suggest.closest(self.allocator, basename, names.items) orelse return null;
        defer self.allocator.free(match);

        // Reconstruct the suggestion using the user's original directory prefix
        const raw_dir = std.fs.path.dirname(raw_path);
        if (raw_dir) |rd| {
            if (rd.len > 0) {
                return try std.fmt.allocPrint(self.allocator, "did you mean '{s}/{s}'?", .{ rd, match });
            }
        }
        return try std.fmt.allocPrint(self.allocator, "did you mean '{s}'?", .{match});
    }

    /// Lightweight lexer scan that finds all include "path" patterns
    /// without doing a full parse. Returns the raw path text alongside
    /// the include directive's token so diagnostics can point at the line.
    fn scanForIncludes(self: *Module, file: *File) ![]IncludeDirective {
        const source = file.source orelse return &.{};
        var lexer = Lexer.init(source, 0);
        var out: std.ArrayList(IncludeDirective) = .empty;
        const alloc = self.arena.allocator();

        while (true) {
            const tok = lexer.next(0);
            if (tok.token_type == .eof) break;
            if (tok.token_type == .include) {
                const next_tok = lexer.next(0);
                if (next_tok.token_type == .string) {
                    try out.append(alloc, .{
                        .path = source[next_tok.start..next_tok.end],
                        .token = tok,
                    });
                } else {
                    try self.errors.addSpan(
                        file.path,
                        "'include' must be followed by a file path string",
                        tok,
                        next_tok,
                        .err,
                        .{},
                    );
                }
            }
        }
        return try out.toOwnedSlice(alloc);
    }

    /// Resolve an include path relative to the including file's directory,
    /// then normalize to be relative to the module root directory.
    pub fn resolveIncludePath(self: *Module, including_file: *File, raw_path: []const u8) ![]const u8 {
        const alloc = self.arena.allocator();

        if (self.dir_path.len > 0) {
            const current_file_dir = try std.fs.path.resolve(alloc, &.{ self.dir_path, including_file.dir_name });
            defer alloc.free(current_file_dir);

            const full_path = try std.fs.path.resolve(alloc, &.{ current_file_dir, raw_path });
            defer alloc.free(full_path);

            return std.fs.path.relative(alloc, self.dir_path, null, self.dir_path, full_path) catch
                try alloc.dupe(u8, full_path);
        } else {
            // initEmpty case (tests): resolve relative to dir_name directly
            const current_file_dir = including_file.dir_name;
            const full_path = try std.fs.path.resolve(alloc, &.{ current_file_dir, raw_path });
            defer alloc.free(full_path);

            return try alloc.dupe(u8, full_path);
        }
    }

    pub fn writeErrors(self: *Module, writer: *std.Io.Writer) !void {
        while (self.errors.list.pop()) |err| {
            // free since we're removing from list
            defer self.allocator.free(err.fmt);
            defer if (err.suggestion) |s| self.allocator.free(s);
            defer if (err.note) |n| self.allocator.free(n);
            const file = self.includes.get(err.file_path).?;
            try err.write(file.source.?, writer);
        }
        try writer.flush();
    }

    pub fn deinit(self: *Module) void {
        self.errors.deinit();
        self.includes.deinit(self.allocator);
        if (self.dir_path.len > 0) self.allocator.free(self.dir_path);
        var arena = self.arena;
        arena.deinit();
        self.allocator.destroy(self);
    }
};

pub const File = struct {
    path: []const u8,
    name: []const u8,
    dir_name: []const u8,
    module: *Module,

    source: ?[]const u8 = null,
    tree: ?Tree = null,
    loc: ?[]const u8 = null,

    /// Path should be relative to the module root directory
    pub fn create(module: *Module, path: []const u8) !*File {
        const allocator = module.arena.allocator();
        const file = try allocator.create(File);
        file.* = .{
            .path = try allocator.dupe(u8, path),
            .name = std.fs.path.basename(path),
            .dir_name = std.fs.path.dirname(path) orelse ".",
            .module = module,
        };
        return file;
    }

    pub fn loadSource(self: *File) !void {
        if (self.source != null) return;
        const io = self.module.io;
        var file = try self.module.dir.openFile(io, self.path, .{});
        defer file.close(io);

        const stat = try file.stat(io);
        const file_size = stat.size;
        var buf: [1024]u8 = undefined;
        var reader = file.reader(io, &buf);
        const read = &reader.interface;
        self.source = try read.readAlloc(self.module.arena.allocator(), file_size);
    }

    pub fn buildTree(self: *File) !void {
        if (self.tree != null) return;
        const source = self.source orelse {
            // Missing file — already reported during include resolution.
            self.tree = Tree{ .root = &.{} };
            return;
        };
        var lexer = Lexer.init(source, 0);
        const alloc = self.module.arena.allocator();
        const file_index = self.module.includes.getIndex(self.path) orelse 0;

        var parser = Parser{
            .current_token = lexer.next(file_index),
            .peek_token = lexer.next(file_index),
            .allocator = alloc,
            .lexer = &lexer,
            .file = self,
            .file_index = file_index,
        };

        var nodes: std.ArrayList(Statement) = .empty;
        errdefer nodes.deinit(alloc);

        while (!parser.currentIs(.eof)) {
            if (parser.statement()) |stmt| {
                try nodes.append(alloc, stmt);
                // Drain comments skipped during peek advancement
                for (parser.pending_comments.items) |comment| {
                    try nodes.append(alloc, comment);
                }
                parser.pending_comments.items.len = 0;
                parser.next();
            } else |err| switch (err) {
                error.ParserError => parser.synchronize(),
                else => return err,
            }
        }
        // Drain any trailing comments
        for (parser.pending_comments.items) |comment| {
            try nodes.append(alloc, comment);
        }

        self.tree = Tree{
            .root = try nodes.toOwnedSlice(alloc),
        };

        if (parser.had_error) return error.ParserError;
    }

    pub fn unloadTree(self: *File) void {
        if (self.tree) |t| t.deinit();
    }
};
