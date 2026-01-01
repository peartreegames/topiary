const std = @import("std");
const C = @import("../utils/index.zig").C;

pub const VisitTree = struct {
    allocator: std.mem.Allocator,
    root: *Node,
    current: *Node,
    list: std.ArrayList([]const u8),

    pub const Node = struct {
        parent: ?*Node,
        name: []const u8,
        index: C.GLOBAL,
        children: std.ArrayList(*Node),
        anon_count: usize = 0,

        pub fn create(allocator: std.mem.Allocator, name: []const u8, index: C.GLOBAL, parent: ?*Node) !*Node {
            const node = try allocator.create(Node);
            node.* = .{
                .parent = parent,
                .name = try allocator.dupe(u8, name),
                .index = index,
                .children = .empty,
            };
            return node;
        }

        pub fn destroy(self: *Node, allocator: std.mem.Allocator) void {
            for (self.children.items) |child| {
                child.destroy(allocator);
            }
            self.children.deinit(allocator);
            allocator.free(self.name);
            allocator.destroy(self);
        }

        pub fn reset(self: *Node) void {
            self.anon_count = 0;
            for (self.children.items) |c| c.reset();
        }

        pub fn getChild(self: *const Node, name: []const u8) ?*Node {
            for (self.children.items) |child| {
                if (std.mem.eql(u8, child.name, name)) return child;
            }
            return null;
        }

        pub fn writePath(self: *const Node, alloc: std.mem.Allocator, writer: *std.Io.Writer) !void {
            var node: ?*const Node = self;
            var list: std.ArrayList(*const Node) = .empty;
            defer list.deinit(alloc);
            while (node) |n| {
                try list.append(alloc, node.?);
                node = n.parent;
            }
            std.mem.reverse(*const Node, list.items);
            for (list.items, 0..) |n, i| {
                writer.writeAll(n.name) catch break;
                if (i != list.items.len - 1) writer.writeByte('.') catch break;
            }
        }

        pub fn print(self: *const Node, writer: *std.Io.Writer, depth: usize) void {
            writer.print("\n", .{}) catch unreachable;
            var d: usize = 0;
            while (d < depth) : (d += 1) {
                writer.print("  ", .{}) catch unreachable;
            }
            writer.print("{s}", .{self.name}) catch unreachable;
            for (self.children.items) |c| c.print(writer, depth + 1);
        }
    };

    pub fn init(allocator: std.mem.Allocator) !VisitTree {
        const root = try Node.create(allocator, "__v", 0, null);
        return .{
            .allocator = allocator,
            .root = root,
            .current = root,
            .list = .empty,
        };
    }

    pub fn push(self: *VisitTree, name: []const u8, index: u32) !void {
        const node = try Node.create(self.allocator, name, index, self.current);
        try self.current.children.append(self.allocator, node);
        self.current = node;
    }

    pub fn pop(self: *VisitTree) void {
        if (self.current.parent) |p| {
            self.current = p;
        } else self.current = self.root;
    }

    pub fn deinit(self: *VisitTree) void {
        self.list.deinit(self.allocator);
        self.root.destroy(self.allocator);
    }

    pub fn print(self: *VisitTree, writer: *std.Io.Writer) void {
        self.root.print(writer, 0);
        writer.print("\n", .{}) catch unreachable;
        writer.flush() catch unreachable;
    }

    pub fn reset(self: *VisitTree) void {
        self.root.reset();
    }

    pub fn resolve(self: *VisitTree, name: []const u8) ?*Node {
        var node = self.current;
        if (std.mem.eql(u8, node.name, name)) return node;
        if (node.getChild(name)) |n| return n;
        while (node.parent) |p| {
            node = p;
            if (std.mem.eql(u8, node.name, name)) return node;
        }
        return null;
    }
};
