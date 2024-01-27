const Tree = @import("ast.zig").Tree;
const Statement = @import("ast.zig").Statement;
const Bytecode = @import("bytecode.zig").Bytecode;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const CompilerErrors = @import("compiler-error.zig").CompilerErrors;
const std = @import("std");
const fs = std.fs;

/// Group of files
pub const Module = struct {
    allocator: std.mem.Allocator,
    entry: *File,
    includes: std.StringArrayHashMap(*File),

    pub fn buildTree(self: *Module) !void {
        try self.entry.loadSource(self.allocator);
        try self.entry.buildTree(self.allocator);
    }

    pub fn writeErrors(self: *Module, writer: anytype) !void {
        var it = self.includes.iterator();
        while (it.next()) |kvp| {
            var file = kvp.value_ptr.*;
            try file.errors.write(file.source, writer);
        }
    }

    pub fn deinit(self: *Module) void {
        self.entry.deinit(self.allocator);
        var it = self.includes.iterator();
        while (it.next()) |kvp| {
            kvp.value_ptr.*.deinit(self.allocator);
            kvp.value_ptr.*.unloadSource(self.allocator);
            self.allocator.free(kvp.key_ptr.*);
        }
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

    tree: ?Tree = null,
    bytecode: ?Bytecode = null,

    pub fn create(path: []const u8, module: *Module) !File {
        std.log.warn("File Path:: {s}", .{path});
        return .{
            .path = path,
            .name = fs.path.basename(path),
            .dir_name = fs.path.dirname(path) orelse "",
            .dir = try fs.openDirAbsolute(fs.path.dirname(path) orelse path, .{}),
            .module = module,
            .errors = CompilerErrors.init(module.allocator),
        };
    }

    pub fn deinit(self: *File, allocator: std.mem.Allocator) void {
        if (self.tree) |tree| tree.deinit();
        if (self.bytecode) |bytecode| bytecode.free(allocator);
        self.errors.deinit();
    }

    pub fn loadSource(self: *File, allocator: std.mem.Allocator) !void {
        const file = try fs.openFileAbsolute(self.path, .{});
        defer file.close();

        const stat = try file.stat();
        const file_size = stat.size;
        const source = try allocator.alloc(u8, file_size);
        try file.reader().readNoEof(source);
    }

    pub fn unloadSource(self: *File, allocator: std.mem.Allocator) void {
        allocator.free(self.source);
    }

    pub fn buildTree(self: *File, allocator: std.mem.Allocator) !void {
        var lexer = Lexer.init(self.source);
        var arena = std.heap.ArenaAllocator.init(allocator);
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
            .allocator = allocator,
            .source = self.source,
        };
    }
};
