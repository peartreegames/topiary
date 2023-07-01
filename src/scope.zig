const std = @import("std");
const ast = @import("./ast.zig");
const values = @import("./values.zig");
const DebugToken = @import("./debug.zig").DebugToken;

pub const Symbol = struct {
    scope: *Scope,
    index: u16,
    name: []const u8,
};

pub const Scope = struct {
    allocator: std.mem.Allocator,
    parent: ?*Scope,
    tag: Tag,

    debug_tokens: DebugToken.List,
    symbols: std.StringArrayHashMap(*Symbol),
    instructions: std.ArrayList(u8),

    pub const Tag = union(enum(u4)) {
        global,
        function,
        module,
    };

    pub fn create(allocator: std.mem.Allocator, parent: ?*Scope, tag: Tag) !*Scope {
        var scope = try allocator.create(Scope);
        scope.* = .{
            .allocator = allocator,
            .parent = parent,
            .symbols = std.StringArrayHashMap(*Symbol).init(allocator),
            .tag = tag,
            .instructions = std.ArrayList(u8).init(allocator),
            .debug_tokens = DebugToken.List.init(allocator),
        };
        return scope;
    }

    pub fn destroy(self: *Scope) void {
        self.instructions.deinit();
        self.debug_tokens.deinit();
        for (self.symbols.values()) |s| {
            self.allocator.destroy(s);
        }
        self.symbols.deinit();
        self.allocator.destroy(self);
    }

    pub fn define(self: *Scope, name: []const u8) !*Symbol {
        const symbol = try self.allocator.create(Symbol);
        const index = @intCast(u16, self.symbols.count());
        symbol.* = .{
            .name = name,
            .index = index,
            .scope = self,
        };
        try self.symbols.putNoClobber(name, symbol);
        return symbol;
    }

    pub fn resolve(self: *Scope, name: []const u8) ?*Symbol {
        var symbol = self.symbols.get(name);
        if (symbol) |s| return s;
        return self.parent.?.resolve(name) orelse null;
    }
};
