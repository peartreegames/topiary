const std = @import("std");
const ast = @import("./ast.zig");
const values = @import("./values.zig");

pub const Scope = struct {
    arena: std.heap.ArenaAllocator.State,
    allocator: std.mem.Allocator,
    parent: ?*Scope,
    variables: std.StringHashMap(values.Value),

    pub fn init(allocator: std.mem.Allocator, parent: ?*Scope) !Scope {
        var arena = std.heap.ArenaAllocator.init(allocator);
        return .{
            .allocator = allocator,
            .arena = arena.state,
            .parent = parent,
            .variables = std.StringHashMap(values.Value).init(allocator),
        };
    }

    pub fn deinit(self: *Scope) void {
        self.arena.deinit();
        self.variables.deinit();
        self.* = undefined;
    }

    pub fn declare(self: *Scope, statement: *ast.Statement.StatementValue, value: values.Value) !void {
        var allocator = self.arena.allocator();
        var decl = statement.declaration;
        value.is_mutable = decl.is_mutable;
        try self.variables.putNoClobber(
            try allocator.dupe(u8, decl.name),
            value,
        );
    }

    pub fn get(self: *Scope, name: []const u8) !?*values.Value {
        if (try self.variables.contains(name)) {
            return try self.variables.get(name);
        }
        if (self.parent != null) return self.parent.get(name);
        return null;
    }
};
