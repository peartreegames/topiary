const std = @import("std");
const ast = @import("./ast.zig");
const values = @import("./values.zig");
const DebugToken = @import("./debug.zig").DebugToken;

pub const Symbol = struct {
    index: u16,
    name: []const u8,
    tag: Scope.Tag,
    is_mutable: bool,
    is_extern: bool,
};

pub const Scope = struct {
    allocator: std.mem.Allocator,
    parent: ?*Scope,
    tag: Tag,

    count: u16 = 0,
    symbols: std.StringArrayHashMap(*Symbol),
    free_symbols: std.ArrayList(*Symbol),

    pub const Tag = union(enum(u4)) {
        global,
        builtin,
        closure,
        function,
        free,
        local,
    };

    pub fn create(allocator: std.mem.Allocator, parent: ?*Scope, tag: Tag) !*Scope {
        var scope = try allocator.create(Scope);
        scope.* = .{
            .allocator = allocator,
            .parent = parent,
            .symbols = std.StringArrayHashMap(*Symbol).init(allocator),
            .free_symbols = std.ArrayList(*Symbol).init(allocator),
            .tag = tag,
        };
        return scope;
    }

    pub fn destroy(self: *Scope) void {
        for (self.symbols.values()) |s| {
            self.allocator.destroy(s);
        }
        self.symbols.deinit();
        self.free_symbols.deinit();
        self.allocator.destroy(self);
    }

    pub fn define(self: *Scope, name: []const u8, is_mutable: bool, is_extern: bool) !*Symbol {
        std.log.warn("DEFINE: {s} -- {}", .{ name, is_extern });
        const symbol = try self.allocator.create(Symbol);
        if (is_extern and self.parent != null) {
            return error.ExternError;
        }

        symbol.* = .{
            .name = name,
            .index = self.count,
            .tag = self.tag,
            .is_mutable = is_mutable,
            .is_extern = is_extern,
        };
        self.count += 1;
        try self.symbols.putNoClobber(name, symbol);
        return symbol;
    }

    pub fn defineFunction(self: *Scope, name: []const u8) !*Symbol {
        const symbol = try self.allocator.create(Symbol);
        symbol.* = .{
            .name = name,
            .index = 0,
            .tag = .function,
            .is_mutable = false,
            .is_extern = false,
        };
        try self.symbols.putNoClobber(name, symbol);
        return symbol;
    }

    pub fn defineFree(self: *Scope, original: *Symbol) !*Symbol {
        const index = @intCast(u16, self.free_symbols.items.len);
        try self.free_symbols.append(original);

        const symbol = try self.allocator.create(Symbol);
        symbol.* = .{
            .name = original.name,
            .index = index,
            .tag = .free,
            .is_mutable = original.is_mutable,
            .is_extern = original.is_extern,
        };
        try self.symbols.putNoClobber(symbol.name, symbol);
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
                var free = try self.defineFree(s);
                return free;
            }
        }
        return null;
    }
};
