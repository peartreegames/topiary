const Tree = @import("ast.zig").Tree;
const Statement = @import("ast.zig").Statement;
const Bytecode = @import("bytecode.zig").Bytecode;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const CompilerErrors = @import("compiler-error.zig").CompilerErrors;
const Compiler = @import("compiler.zig").Compiler;
const std = @import("std");
const fs = std.fs;

/// Group of files
pub const Module = struct {
    allocator: std.mem.Allocator,
    entry: *File,
    use_loc: bool = false,
    includes: std.StringArrayHashMap(*File),

    pub fn init(allocator: std.mem.Allocator, entry_path: []const u8) !*Module {
        var mod = try allocator.create(Module);
        mod.* = .{
            .allocator = allocator,
            .entry = undefined,
            .includes = std.StringArrayHashMap(*File).init(allocator),
        };
        const file = try File.create(allocator, entry_path, mod);
        mod.entry = file;
        try mod.includes.putNoClobber(file.path, file);
        return mod;
    }

    /// Used for initialzing with source direstly rather than a file path
    pub fn create(allocator: std.mem.Allocator) Module {
        return .{
            .allocator = allocator,
            .entry = undefined,
            .includes = std.StringArrayHashMap(*File).init(allocator),
        };
    }

    pub fn generateBytecode(self: *Module) !Bytecode {
        try self.entry.loadSource();
        self.entry.buildTree() catch |err| {
            try self.writeErrors(std.io.getStdErr().writer());
            return err;
        };

        var compiler = try Compiler.init(self.allocator);
        compiler.use_loc = self.use_loc;
        defer compiler.deinit();

        compiler.compile(self) catch |e| {
            try self.writeErrors(std.io.getStdErr().writer());
            return e;
        };
        return try compiler.bytecode();
    }

    pub fn writeErrors(self: *Module, writer: anytype) !void {
        var it = self.includes.iterator();
        while (it.next()) |kvp| {
            var file = kvp.value_ptr.*;
            try file.errors.write(file.source, writer);
        }
    }

    pub fn deinit(self: *Module) void {
        var it = self.includes.iterator();
        const maybe_first = it.next();
        // TODO Refactor this.
        // The file.tree arean allocator deinits all other trees
        // that have been included, so we only deinit the first tree
        if (maybe_first) |first| {
            first.value_ptr.*.destroy();
            self.allocator.free(first.key_ptr.*);
        }

        while (it.next()) |kvp| {
            kvp.value_ptr.*.tree_loaded = false;
            kvp.value_ptr.*.destroy();
            self.allocator.free(kvp.key_ptr.*);
        }
        self.includes.deinit();
    }
};

pub const File = struct {
    path: []const u8,
    name: []const u8,
    dir_name: []const u8,
    dir: fs.Dir,
    module: ?*Module = null,
    errors: CompilerErrors,

    source: []const u8 = undefined,
    source_loaded: bool = false,
    tree: Tree = undefined,
    tree_loaded: bool = false,
    loc: []const u8 = undefined,
    loc_loaded: bool = false,

    allocator: std.mem.Allocator,

    pub fn create(allocator: std.mem.Allocator, path: []const u8, module: ?*Module) !*File {
        const file = try allocator.create(File);
        file.* = .{
            .allocator = allocator,
            .path = path,
            .name = fs.path.basename(path),
            .dir_name = fs.path.dirname(path) orelse "",
            .dir = try fs.openDirAbsolute(fs.path.dirname(path) orelse path, .{}),
            .module = module,
            .errors = CompilerErrors.init(allocator),
        };
        return file;
    }

    pub fn destroy(self: *File) void {
        if (self.source_loaded) self.allocator.free(self.source);
        if (self.tree_loaded) self.tree.deinit();
        if (self.loc_loaded) self.allocator.free(self.loc);
        self.errors.deinit();
        self.allocator.destroy(self);
    }

    pub fn loadSource(self: *File) !void {
        if (self.source_loaded) return;
        const file = try fs.openFileAbsolute(self.path, .{});
        defer file.close();

        const stat = try file.stat();
        const file_size = stat.size;
        const source = try self.allocator.alloc(u8, file_size);
        try file.reader().readNoEof(source);
        self.source = source;
        self.source_loaded = true;
    }

    pub fn unloadSource(self: *File) void {
        if (!self.source_loaded) return;
        self.allocator.free(self.source);
        self.source_loaded = false;
    }

    pub fn loadLoc(self: *File) !void {
        if (self.loc_loaded) return;
        var csv_path = try self.allocator.alloc(u8, self.path.len + 4);
        defer self.allocator.free(csv_path);
        @memcpy(csv_path[0..self.path.len], self.path);
        @memcpy(csv_path[self.path.len..], ".csv");
        defer self.allocator.free(csv_path);
        const file = fs.openFileAbsolute(csv_path, .{}) catch return error.FileNotFound;
        defer file.close();

        const stat = try file.stat();
        const file_size = stat.size;
        const source = try self.allocator.alloc(u8, file_size);
        try file.reader().readNoEof(source);
        self.loc = source;
        self.loc_loaded = true;
    }

    pub fn unloadLoc(self: *File) void {
        if (!self.loc_loaded) return;
        self.allocator.free(self.loc);
        self.loc_loaded = false;
        self.loc = undefined;
    }

    pub fn buildTree(self: *File) !void {
        if (self.tree_loaded) return;
        if (!self.source_loaded) return error.ParserError;
        var lexer = Lexer.init(self.source, 0);
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        errdefer arena.deinit();

        var parser = Parser{
            .current_token = lexer.next(),
            .peek_token = lexer.next(),
            .arena = arena.state,
            .allocator = arena.allocator(),
            .lexer = &lexer,
            .file = self,
        };

        var nodes = std.ArrayList(Statement).init(parser.allocator);
        errdefer nodes.deinit();

        while (!parser.currentIs(.eof)) : (parser.next()) {
            try nodes.append(try parser.statement());
        }

        self.tree = Tree{
            .root = try nodes.toOwnedSlice(),
            .arena = arena.state,
            .allocator = self.allocator,
            .source = self.source,
        };
        self.tree_loaded = true;
    }
};
