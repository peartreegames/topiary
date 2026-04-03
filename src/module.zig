const std = @import("std");
const frontend = @import("frontend/index.zig");
const Statement = frontend.Statement;
const Tree = frontend.Tree;
const Lexer = frontend.Lexer;
const Parser = frontend.Parser;

const backend = @import("backend/index.zig");
const Bytecode = backend.Bytecode;
const Compiler = backend.Compiler;
const CompilerErrors = backend.CompilerErrors;

const fs = std.fs;

/// Group of files
/// Manages all allocations and clears all with deinit
pub const Module = struct {
    arena: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    errors: CompilerErrors,
    entry: *File,
    dir: fs.Dir,
    dir_path: []const u8,
    includes: std.StringArrayHashMap(*File),

    pub fn init(allocator: std.mem.Allocator, entry_path: []const u8) !*Module {
        const mod = try allocator.create(Module);
        const path = std.fs.path.basename(entry_path);
        const dir_name = std.fs.path.dirname(entry_path) orelse return error.InvalidArgument;
        mod.* = .{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .dir = try std.fs.openDirAbsolute(dir_name, .{}),
            .dir_path = try allocator.dupe(u8, dir_name),
            .entry = undefined,
            .includes = std.StringArrayHashMap(*File).init(allocator),
            .errors = CompilerErrors.init(allocator),
        };
        // Can't add entry until includes is initialized
        mod.entry = try mod.addFileAtPath(path);
        return mod;
    }

    /// Used for initializing with source directly rather than a file path
    pub fn initEmpty(allocator: std.mem.Allocator) !*Module {
        const mod = try allocator.create(Module);
        mod.* = .{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .dir = undefined,
            .dir_path = "",
            .entry = undefined,
            .includes = std.StringArrayHashMap(*File).init(allocator),
            .errors = CompilerErrors.init(allocator),
        };
        return mod;
    }

    /// Path must be relative to the Module root directory
    pub fn addFileAtPath(self: *Module, path: []const u8) !*File {
        if (self.includes.get(path)) |existing| return existing;
        const file = try File.create(self, path);
        try self.includes.putNoClobber(file.path, file);
        return file;
    }

    pub fn generateBytecode(self: *Module, allocator: std.mem.Allocator) !Bytecode {
        // Phase 1: Resolve all includes and load sources
        try self.resolveIncludes();

        // Phase 2: Build parse trees for all files
        var it = self.includes.iterator();
        while (it.next()) |kvp| {
            try kvp.value_ptr.*.buildTree();
        }

        // Phase 3: Compile
        var compiler = try Compiler.init(allocator, self);
        defer compiler.deinit();

        compiler.compile() catch |e| {
            return e;
        };
        return try compiler.bytecode();
    }

    /// Resolve all include directives transitively by scanning tokens.
    /// Loads source for each discovered file. After this completes,
    /// all files are in self.includes with their sources loaded.
    pub fn resolveIncludes(self: *Module) !void {
        var i: usize = 0;
        while (i < self.includes.count()) : (i += 1) {
            const file = self.includes.values()[i];
            try file.loadSource();
            const paths = try self.scanForIncludes(file);
            for (paths) |raw_path| {
                const resolved = try self.resolveIncludePath(file, raw_path);
                _ = try self.addFileAtPath(resolved);
            }
        }
    }

    /// Lightweight lexer scan that finds all include "path" patterns
    /// without doing a full parse.
    fn scanForIncludes(self: *Module, file: *File) ![][]const u8 {
        const source = file.source orelse return &.{};
        var lexer = Lexer.init(source, 0);
        var paths: std.ArrayList([]const u8) = .empty;
        const alloc = self.arena.allocator();

        while (true) {
            const tok = lexer.next(0);
            if (tok.token_type == .eof) break;
            if (tok.token_type == .include) {
                const next_tok = lexer.next(0);
                if (next_tok.token_type == .string) {
                    try paths.append(alloc, source[next_tok.start..next_tok.end]);
                }
            }
        }
        return try paths.toOwnedSlice(alloc);
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

            return std.fs.path.relative(alloc, self.dir_path, full_path) catch
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
            const file = self.includes.get(err.file_path).?;
            try err.write(file.source.?, writer);
        }
        try writer.flush();
    }

    pub fn deinit(self: *Module) void {
        self.errors.deinit();
        self.includes.deinit();
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
            .name = fs.path.basename(path),
            .dir_name = fs.path.dirname(path) orelse ".",
            .module = module,
        };
        return file;
    }

    pub fn loadSource(self: *File) !void {
        if (self.source != null) return;
        var file = try self.module.dir.openFile(self.path, .{});
        defer file.close();

        const stat = try file.stat();
        const file_size = stat.size;
        var buf: [1024]u8 = undefined;
        var reader = file.reader(&buf);
        const read = &reader.interface;
        self.source = try read.readAlloc(self.module.arena.allocator(), file_size);
    }

    pub fn buildTree(self: *File) !void {
        if (self.tree != null) return;
        const source = self.source orelse return error.ParserError;
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
