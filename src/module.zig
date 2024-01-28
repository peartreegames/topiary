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

    pub fn create(allocator: std.mem.Allocator) Module {
        return .{
            .allocator = allocator,
            .entry = undefined,
            .includes = std.StringArrayHashMap(*File).init(allocator),
        };
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
        while (it.next()) |kvp| {
            kvp.value_ptr.*.deinit(self.allocator);
            kvp.value_ptr.*.unloadSource(self.allocator);
            self.allocator.free(kvp.key_ptr.*);
            self.allocator.destroy(kvp.value_ptr.*);
        }
        self.includes.deinit();
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

    pub fn create(path: []const u8, module: *Module) !File {
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
        _ = allocator;
        if (self.tree_loaded) self.tree.deinit();
        self.errors.deinit();
    }

    pub fn loadSource(self: *File, allocator: std.mem.Allocator) !void {
        if (self.source_loaded) return;
        const file = try fs.openFileAbsolute(self.path, .{});
        defer file.close();

        const stat = try file.stat();
        const file_size = stat.size;
        const source = try allocator.alloc(u8, file_size);
        try file.reader().readNoEof(source);
        self.source = source;
        self.source_loaded = true;
    }

    pub fn unloadSource(self: *File, allocator: std.mem.Allocator) void {
        if (!self.source_loaded) return;
        allocator.free(self.source);
        self.source_loaded = false;
    }

    pub fn buildTree(self: *File, allocator: std.mem.Allocator) !void {
        if (self.tree_loaded) return;
        if (!self.source_loaded) return error.ParserError;
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
        self.tree_loaded = true;
    }
};
