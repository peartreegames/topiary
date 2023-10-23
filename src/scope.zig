const std = @import("std");
const ast = @import("./ast.zig");
const values = @import("./values.zig");
const DebugToken = @import("./debug.zig").DebugToken;
const OpCode = @import("./opcode.zig").OpCode;

pub const Symbol = struct {
    index: OpCode.Size(.get_global),
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
    symbols: std.StringArrayHashMap(*Symbol),
    free_symbols: std.ArrayList(*Symbol),
    offset: OpCode.Size(.get_global),

    pub const Tag = union(enum(u4)) {
        global,
        builtin,
        closure,
        function,
        free,
        local,
    };

    pub fn create(allocator: std.mem.Allocator, parent: ?*Scope, tag: Tag, offset: u32) !*Scope {
        var scope = try allocator.create(Scope);
        scope.* = .{
            .allocator = allocator,
            .parent = parent,
            .symbols = std.StringArrayHashMap(*Symbol).init(allocator),
            .free_symbols = std.ArrayList(*Symbol).init(allocator),
            .tag = tag,
            .offset = offset,
        };
        return scope;
    }

    pub fn destroy(self: *Scope) void {
        for (self.symbols.values()) |s| {
            self.allocator.free(s.name);
            self.allocator.destroy(s);
        }
        self.symbols.deinit();
        self.free_symbols.deinit();
        self.allocator.destroy(self);
    }

    pub fn define(self: *Scope, name: []const u8, is_mutable: bool, is_extern: bool) !*Symbol {
        const symbol = try self.allocator.create(Symbol);
        if (is_extern and self.parent != null) {
            return error.ExternError;
        }
        var name_copy = try self.allocator.dupe(u8, name);
        symbol.* = .{
            .name = name_copy,
            .index = self.count + self.offset,
            .tag = self.tag,
            .is_mutable = is_mutable,
            .is_extern = is_extern,
        };
        self.count += 1;
        try self.symbols.putNoClobber(name_copy, symbol);
        return symbol;
    }

    pub fn defineFunction(self: *Scope, name: []const u8) !*Symbol {
        const symbol = try self.allocator.create(Symbol);
        var name_copy = try self.allocator.dupe(u8, name);
        symbol.* = .{
            .name = name_copy,
            .index = 0,
            .tag = .function,
            .is_mutable = false,
            .is_extern = false,
        };
        try self.symbols.putNoClobber(name_copy, symbol);
        return symbol;
    }

    pub fn defineFree(self: *Scope, original: *Symbol) !*Symbol {
        const index = @as(u32, @intCast(self.free_symbols.items.len));
        try self.free_symbols.append(original);

        const symbol = try self.allocator.create(Symbol);
        var name = try self.allocator.dupe(u8, original.name);
        symbol.* = .{
            .name = name,
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

    pub fn print(self: *Scope, writer: anytype) void {
        writer.print("==SCOPE==\n", .{});
        for (self.symbols.keys()) |k| {
            writer.print("{s}\n", .{k});
        }
    }
};
