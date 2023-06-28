const std = @import("std");
const ast = @import("./ast.zig");
const values = @import("./values.zig");
const SymbolTable = @import("./symbols.zig").SymbolTable;
const DebugToken = @import("./debug.zig").DebugToken;

pub const Scope = struct {
    allocator: std.mem.Allocator,
    parent: ?*Scope,
    symbols: SymbolTable,
    tag: Tag,

    debug_tokens: DebugToken.List,
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
            .symbols = SymbolTable.init(allocator),
            .tag = tag,
            .instructions = std.ArrayList(u8).init(allocator),
            .debug_tokens = DebugToken.List.init(allocator),
        };
        return scope;
    }

    pub fn destroy(self: *Scope) void {
        self.instructions.deinit();
        self.debug_tokens.deinit();
        self.symbols.deinit();
        self.allocator.destroy(self);
    }
};
