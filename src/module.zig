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
    includes: std.StringArrayHashMap(*File),

    pub fn init(allocator: std.mem.Allocator, entry_path: []const u8) !*Module {
        const mod = try allocator.create(Module);
        const path = std.fs.path.basename(entry_path);
        const dir_name = std.fs.path.dirname(entry_path) orelse return error.InvalidArgument;
        mod.* = .{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .dir = try std.fs.openDirAbsolute(dir_name, .{}),
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
        var it = self.includes.iterator();
        while (it.next()) |kvp| {
            const file = kvp.value_ptr.*;
            try file.loadSource();
            try file.buildTree();
        }

        var compiler = try Compiler.init(allocator, self);
        defer compiler.deinit();

        compiler.compile() catch |e| {
            return e;
        };
        return try compiler.bytecode();
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

        while (!parser.currentIs(.eof)) : (parser.next()) {
            try nodes.append(alloc, try parser.statement());
        }

        self.tree = Tree{
            .root = try nodes.toOwnedSlice(alloc),
        };
    }

    pub fn unloadTree(self: *File) void {
        if (self.tree) |t| t.deinit();
    }
};
