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
    free_symbols: std.ArrayList(*Symbol),

    pub const Tag = union(enum(u4)) {
        builtin,
        constant,
        global,
        local,
        free,
    };

    pub fn create(allocator: std.mem.Allocator, parent: ?*Scope, tag: Tag) !*Scope {
        const scope = try allocator.create(Scope);
        scope.* = .{
            .allocator = allocator,
            .parent = parent,
            .symbols = .empty,
            .free_symbols = .empty,
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
        self.free_symbols.deinit(self.allocator);
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

    pub fn defineFunction(self: *Scope, name: []const u8) !*Symbol {
        const symbol = try self.allocator.create(Symbol);
        const name_copy = try self.allocator.dupe(u8, name);
        symbol.* = .{
            .name = name_copy,
            .index = 0,
            .tag = .function,
            .is_mutable = false,
            .is_extern = false,
        };
        try self.symbols.putNoClobber(self.allocator, name_copy, symbol);
        return symbol;
    }

    pub fn defineFree(self: *Scope, original: *Symbol) !*Symbol {
        const index = @as(u32, @intCast(self.free_symbols.items.len));
        try self.free_symbols.append(self.allocator, original);

        const symbol = try self.allocator.create(Symbol);
        const name = try self.allocator.dupe(u8, original.name);
        symbol.* = .{
            .name = name,
            .index = index,
            .tag = .free,
            .is_mutable = original.is_mutable,
            .is_extern = original.is_extern,
        };
        try self.symbols.putNoClobber(self.allocator, symbol.name, symbol);
        return symbol;
    }

    pub fn resolve(self: *Scope, name: []const u8) !?*Symbol {
        var symbol = self.symbols.get(name);
        if (symbol) |s| return s;
        if (self.parent) |p| {
            symbol = try p.resolve(name);
            if (symbol == null) return null;
            if (symbol) |s| {
                if (s.tag == .global or self.tag == .local) return s;
                const free = try self.defineFree(s);
                return free;
            }
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
