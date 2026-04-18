const std = @import("std");
const utils = @import("../utils/index.zig");
const C = utils.C;
const UUID = utils.UUID;

pub const VarType = union(enum) {
    unknown,
    number,
    boolean,
    nil,
    string,
    list,
    set,
    map,
    instance: []const u8,
};

pub const Symbol = struct {
    index: C.GLOBAL,
    name: []const u8,
    uuid: UUID.ID = UUID.Empty,
    tag: Scope.Tag,
    is_mutable: bool,
    var_type: VarType = .unknown,
};

pub const Scope = struct {
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
        // Local scopes share the same stack frame as their parent, so they
        // must start indexing after the parent's variables to avoid conflicts.
        const initial_count: u32 = if (tag == .local)
            if (parent) |p| p.count else 0
        else
            0;
        scope.* = .{
            .parent = parent,
            .symbols = .empty,
            .tag = tag,
            .count = initial_count,
        };
        return scope;
    }

    pub fn destroy(self: *Scope, allocator: std.mem.Allocator) void {
        for (self.symbols.values()) |s| {
            allocator.free(s.name);
            allocator.destroy(s);
        }
        self.symbols.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn define(self: *Scope, allocator: std.mem.Allocator, name: []const u8, is_mutable: bool) !*Symbol {
        if (self.symbols.contains(name)) return error.SymbolAlreadyDeclared;
        // Reserve capacity so the final put cannot fail after we allocate.
        try self.symbols.ensureUnusedCapacity(allocator, 1);
        const symbol = try allocator.create(Symbol);
        errdefer allocator.destroy(symbol);
        const name_copy = try allocator.dupe(u8, name);
        symbol.* = .{
            .name = name_copy,
            .index = self.count,
            .tag = self.tag,
            .is_mutable = is_mutable,
        };
        self.count += 1;
        self.symbols.putAssumeCapacityNoClobber(name_copy, symbol);
        return symbol;
    }

    pub fn defineUpvalue(self: *Scope, allocator: std.mem.Allocator, original: *Symbol) !*Symbol {
        try self.symbols.ensureUnusedCapacity(allocator, 1);
        const symbol = try allocator.create(Symbol);
        errdefer allocator.destroy(symbol);
        const name = try allocator.dupe(u8, original.name);
        symbol.* = .{
            .name = name,
            .index = original.index,
            .tag = .upvalue,
            .is_mutable = original.is_mutable,
            .var_type = original.var_type,
        };
        self.symbols.putAssumeCapacityNoClobber(symbol.name, symbol);
        return symbol;
    }

    /// Look for `name` in any enclosing scope (strictly NOT the current one)
    /// without defining upvalues. Used for shadow-warning diagnostics.
    pub fn resolveOuter(self: *Scope, name: []const u8) ?*Symbol {
        var current: ?*Scope = self.parent;
        while (current) |s| : (current = s.parent) {
            if (s.symbols.get(name)) |sym| return sym;
        }
        return null;
    }

    pub fn resolve(self: *Scope, allocator: std.mem.Allocator, name: []const u8) !?*Symbol {
        const symbol = self.symbols.get(name);
        if (symbol) |s| return s;

        if (self.parent) |p| {
            const s = (try p.resolve(allocator, name)) orelse return null;
            if (s.tag == .global) return s;

            if (self.tag == .function) {
                return try self.defineUpvalue(allocator, s);
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
