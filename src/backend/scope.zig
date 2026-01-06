const std = @import("std");
const C = @import("../utils/index.zig").C;

pub const Symbol = struct {
    index: C.GLOBAL,
    name: []const u8,
    tag: Scope.Tag,
    is_mutable: bool,
    is_extern: bool,
};

pub const Scope = struct {
    allocator: std.mem.Allocator,
    parent: ?*Scope,
    tag: Tag,

    count: u32 = 0,
    symbols: std.StringArrayHashMapUnmanaged(*Symbol),

    pub const Tag = union(enum(u4)) {
        global,
        local,
        function,
        upvalue,
    };

    pub fn create(allocator: std.mem.Allocator, parent: ?*Scope, tag: Tag) !*Scope {
        const scope = try allocator.create(Scope);
        scope.* = .{
            .allocator = allocator,
            .parent = parent,
            .symbols = .empty,
            .tag = tag,
        };
        return scope;
    }

    pub fn destroy(self: *Scope) void {
        for (self.symbols.values()) |s| {
            self.allocator.free(s.name);
            self.allocator.destroy(s);
        }
        self.symbols.deinit(self.allocator);
        self.allocator.destroy(self);
    }

    pub fn define(self: *Scope, name: []const u8, is_mutable: bool, is_extern: bool) !*Symbol {
        if (self.symbols.contains(name)) return error.SymbolAlreadyDeclared;
        const symbol = try self.allocator.create(Symbol);
        if (is_extern and self.parent != null) {
            return error.ExternError;
        }
        const name_copy = try self.allocator.dupe(u8, name);
        symbol.* = .{
            .name = name_copy,
            .index = self.count,
            .tag = self.tag,
            .is_mutable = is_mutable,
            .is_extern = is_extern,
        };
        self.count += 1;
        try self.symbols.putNoClobber(self.allocator, name_copy, symbol);
        return symbol;
    }

    pub fn defineUpvalue(self: *Scope, original: *Symbol) !*Symbol {
        const symbol = try self.allocator.create(Symbol);
        const name = try self.allocator.dupe(u8, original.name);
        symbol.* = .{
            .name = name,
            .index = original.index,
            .tag = .upvalue,
            .is_mutable = original.is_mutable,
            .is_extern = original.is_extern,
        };
        try self.symbols.putNoClobber(self.allocator, symbol.name, symbol);
        return symbol;
    }

    pub fn resolve(self: *Scope, name: []const u8) !?*Symbol {
        const symbol = self.symbols.get(name);
        if (symbol) |s| return s;

        if (self.parent) |p| {
            const s = (try p.resolve(name)) orelse return null;
            if (s.tag == .global) return s;

            if (self.tag == .function) {
                return try self.defineUpvalue(s);
            }
            return s;
        }
        return null;
    }

    pub fn print(self: *Scope, writer: *std.Io.Writer) void {
        writer.print("==SCOPE==\n", .{});
        for (self.symbols.keys()) |k| {
            writer.print("{s}\n", .{k});
        }
    }
};
