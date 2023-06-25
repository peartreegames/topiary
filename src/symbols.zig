const std = @import("std");
const Scope = @import("./scope.zig").Scope;

pub const Symbol = struct {
    scope: ?*Scope,
    index: u16,
    name: []const u8,
};

pub const SymbolTable = struct {
    store: std.StringArrayHashMap(*Symbol),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) SymbolTable {
        return .{
            .store = std.StringArrayHashMap(*Symbol).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *SymbolTable) void {
        for (self.store.values()) |symbol| {
            self.allocator.destroy(symbol);
        }
        self.store.deinit();
    }

    pub fn define(self: *SymbolTable, name: []const u8) !*Symbol {
        const symbol = try self.allocator.create(Symbol);
        symbol.* = .{
            .name = name,
            .index = @intCast(u16, self.store.count()),
            .scope = null,
        };
        try self.store.putNoClobber(name, symbol);
        return symbol;
    }

    pub fn resolve(self: *SymbolTable, name: []const u8) ?*Symbol {
        return self.store.get(name);
    }
};
