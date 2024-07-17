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
/// Manages all allocations and clears all with deinit
pub const Module = struct {
    arena: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    entry: *File,
    use_loc: bool = false,
    includes: std.StringArrayHashMap(*File),
    allow_includes: bool = true,

    pub fn init(allocator: std.mem.Allocator, entry_path: []const u8) !*Module {
        const mod = try initEmpty(allocator);
        mod.entry = try mod.addFileAtPath(entry_path);
        return mod;
    }

    /// Used for initialzing with source directly rather than a file path
    pub fn initEmpty(allocator: std.mem.Allocator) !*Module {
        const mod = try allocator.create(Module);
        mod.* = .{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .entry = undefined,
            .includes = undefined,
        };
        mod.includes = std.StringArrayHashMap(*File).init(mod.arena.allocator());
        return mod;
    }

    pub fn addFileAtPath(self: *Module, path: []const u8) !*File {
        const file = try File.create(self, path);
        try self.includes.putNoClobber(file.path, file);
        return file;
    }

    pub fn generateBytecode(self: *Module, allocator: std.mem.Allocator) !Bytecode {
        try self.entry.loadSource();
        self.entry.buildTree() catch |err| {
            try self.writeErrors(std.io.getStdErr().writer());
            return err;
        };

        var compiler = try Compiler.init(allocator, self);
        compiler.use_loc = self.use_loc;
        defer compiler.deinit();

        compiler.compile() catch |e| {
            try self.writeErrors(std.io.getStdErr().writer());
            return e;
        };
        return try compiler.bytecode();
    }

    pub fn writeErrors(self: *Module, writer: anytype) !void {
        var it = self.includes.iterator();
        while (it.next()) |kvp| {
            var file = kvp.value_ptr.*;
            if (file.source_loaded) try file.errors.write(file.path, file.source, writer);
        }
    }

    pub fn deinit(self: *Module) void {
        var it = self.includes.iterator();
        while (it.next()) |kvp| {
            kvp.value_ptr.*.errors.deinit();
        }
        var arena = self.arena;
        arena.deinit();
        self.allocator.destroy(self);
    }
};

pub const File = struct {
    path: []const u8,
    name: []const u8,
    dir_name: []const u8,
    dir: fs.Dir,
    module: *Module,
    errors: CompilerErrors,

    source: []const u8 = undefined,
    source_loaded: bool = false,
    tree: Tree = undefined,
    tree_loaded: bool = false,
    loc: []const u8 = undefined,
    loc_loaded: bool = false,

    pub fn create(module: *Module, path: []const u8) !*File {
        const allocator = module.arena.allocator();
        const file = try allocator.create(File);
        file.* = .{
            .path = try allocator.dupe(u8, path),
            .name = fs.path.basename(path),
            .dir_name = fs.path.dirname(path) orelse "",
            .dir = try fs.openDirAbsolute(fs.path.dirname(path) orelse path, .{}),
            .module = module,
            .errors = CompilerErrors.init(module.arena.allocator()),
        };
        return file;
    }

    pub fn loadSource(self: *File) !void {
        if (self.source_loaded) return;
        const file = try fs.openFileAbsolute(self.path, .{});
        defer file.close();

        const stat = try file.stat();
        const file_size = stat.size;
        const source = try self.module.arena.allocator().alloc(u8, file_size);
        try file.reader().readNoEof(source);
        self.source = source;
        self.source_loaded = true;
    }

    pub fn unloadSource(self: *File) void {
        if (self.source_loaded) self.module.arena.allocator().free(self.source);
        self.source_loaded = false;
    }

    pub fn loadLoc(self: *File) !void {
        if (self.loc_loaded) return;
        const allocator = self.module.arena.allocator();
        var csv_path = try allocator.alloc(u8, self.path.len + 4);
        defer allocator.free(csv_path);
        @memcpy(csv_path[0..self.path.len], self.path);
        @memcpy(csv_path[self.path.len..], ".csv");
        defer allocator.free(csv_path);
        const file = fs.openFileAbsolute(csv_path, .{}) catch return error.FileNotFound;
        defer file.close();

        const stat = try file.stat();
        const file_size = stat.size;
        const source = try allocator.alloc(u8, file_size);
        try file.reader().readNoEof(source);
        self.loc = source;
        self.loc_loaded = true;
    }

    pub fn unloadLoc(self: *File) void {
        if (self.loc_loaded) self.module.arena.allocator().free(self.loc);
        self.loc_loaded = false;
    }

    pub fn buildTree(self: *File) !void {
        if (self.tree_loaded) return;
        if (!self.source_loaded) return error.ParserError;
        var lexer = Lexer.init(self.source, 0);
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

        var nodes = std.ArrayList(Statement).init(alloc);
        errdefer nodes.deinit();

        while (!parser.currentIs(.eof)) : (parser.next()) {
            try nodes.append(try parser.statement());
        }

        self.tree = Tree{
            .root = try nodes.toOwnedSlice(),
        };
        self.tree_loaded = true;
    }

    pub fn unloadTree(self: *File) void {
        if (self.tree_loaded) self.tree.deinit();
        self.tree_loaded = false;
    }
};
