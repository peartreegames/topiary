const std = @import("std");
const global_size = u32;

pub const VisitTree = struct {
    allocator: std.mem.Allocator,
    root: *Node,
    current: *Node,
    list: std.ArrayList([]const u8),

    pub const Node = struct {
        parent: ?*Node,
        name: []const u8,
        index: global_size,
        children: std.ArrayList(*Node),
        anon_count: usize = 0,

        pub fn create(allocator: std.mem.Allocator, name: []const u8, index: global_size, parent: ?*Node) !*Node {
            var node = try allocator.create(Node);
            node.* = .{
                .parent = parent,
                .name = try allocator.dupe(u8, name),
                .index = index,
                .children = std.ArrayList(*Node).init(allocator),
            };
            return node;
        }

        pub fn destroy(self: *const Node, allocator: std.mem.Allocator) void {
            for (self.children.items) |child| {
                child.destroy(allocator);
            }
            self.children.deinit();
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

        pub fn writePath(self: *const Node, writer: anytype) !void {
            var node: ?*const Node = self;
            var list = std.ArrayList(*const Node).init(std.heap.page_allocator);
            defer list.deinit();
            while (node) |n| {
                try list.append(node.?);
                node = n.parent;
            }
            std.mem.reverse(*const Node, list.items);
            for (list.items) |n| {
                writer.writeAll(n.name) catch break;
                writer.writeByte('.') catch break;
            }
        }

        pub fn print(self: *const Node, writer: anytype, depth: usize) void {
            writer.print("\n", .{});
            var d: usize = 0;
            while (d < depth) : (d += 1) {
                writer.print("  ", .{});
            }
            writer.print("{s}", .{self.name});
            for (self.children.items) |c| c.print(writer, depth + 1);
        }
    };

    pub fn init(allocator: std.mem.Allocator) !VisitTree {
        var root = try Node.create(allocator, "__v", 0, null);
        return .{
            .allocator = allocator,
            .root = root,
            .current = root,
            .list = std.ArrayList([]const u8).init(allocator),
        };
    }

    pub fn push(self: *VisitTree, name: []const u8, index: u32) !void {
        var node = try Node.create(self.allocator, name, index, self.current);
        try self.current.children.append(node);
        self.current = node;
    }

    pub fn pop(self: *VisitTree) void {
        if (self.current.parent) |p| {
            self.current = p;
        } else self.current = self.root;
    }

    pub fn deinit(self: *VisitTree) void {
        self.list.deinit();
        self.root.destroy(self.allocator);
    }

    pub fn print(self: *VisitTree, writer: anytype) void {
        self.root.print(writer, 0);
        writer.print("\n", .{});
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
