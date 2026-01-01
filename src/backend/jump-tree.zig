const std = @import("std");

/// Used to keep track of jump locations by scope name
pub const JumpTree = struct {
    allocator: std.mem.Allocator,
    root: *Node,
    current: *Node,

    pub const Entry = struct {
        node: *Node,
        jump_ip: u32,
    };

    pub const Node = struct {
        parent: ?*Node,
        name: []const u8,
        children: std.ArrayList(*Node),
        dest_ip: u32 = 9999,

        pub fn create(allocator: std.mem.Allocator, name: []const u8, parent: ?*Node) !*Node {
            const node = try allocator.create(Node);
            node.* = .{
                .parent = parent,
                .name = name,
                .children = .empty,
            };
            return node;
        }

        pub fn destroy(self: *Node, allocator: std.mem.Allocator) void {
            for (self.children.items) |child| {
                child.destroy(allocator);
            }
            self.children.deinit(allocator);
            allocator.destroy(self);
        }

        pub fn contains(self: *const Node, name: []const u8) bool {
            for (self.children.items) |child| {
                if (std.mem.eql(u8, child.name, name)) return true;
            }
            return false;
        }

        pub fn getChild(self: *const Node, name: []const u8) !*Node {
            for (self.children.items) |child| {
                if (std.mem.eql(u8, child.name, name)) return child;
            }
            return error.SymbolNotFound;
        }

        pub fn writePath(self: *const Node, allocator: std.mem.Allocator, writer: *std.Io.Writer) !void {
            var node: ?*const Node = self;
            const NodeArray = std.ArrayList(*const Node);
            var list: NodeArray = .empty;
            defer list.deinit(allocator);
            while (node) |n| {
                try list.append(allocator, node.?);
                node = n.parent;
            }
            std.mem.reverse(*const Node, list.items);
            for (list.items, 0..) |n, i| {
                if (i == 0) continue;
                writer.writeAll(n.name) catch break;
                if (i != list.items.len - 1) writer.writeAll(".") catch break;
            }
        }

        pub fn print(self: *const Node, writer: *std.Io.Writer, depth: usize) void {
            var i: usize = depth;
            while (i > 0) : (i -= 1) {
                writer.print("    ", .{});
            }
            writer.print("{s}\n", .{self.name});
            for (self.children.items) |child| {
                child.print(writer, depth + 1);
            }
        }
    };

    pub fn init(allocator: std.mem.Allocator) !JumpTree {
        const root = try JumpTree.Node.create(allocator, "root", null);
        return .{
            .allocator = allocator,
            .root = root,
            .current = root,
        };
    }

    pub fn pop(self: *JumpTree) void {
        if (self.current.parent) |p| {
            self.current = p;
        } else self.current = self.root;
    }

    pub fn deinit(self: *JumpTree) void {
        self.root.destroy(self.allocator);
    }

    pub fn print(self: *JumpTree, writer: *std.Io.Writer) void {
        self.root.print(writer, 0);
    }

    fn get(self: *JumpTree, path: [][]const u8) ?*Node {
        var node = self.root;
        for (path) |name| {
            var found = false;
            for (node.children.items) |child| {
                if (!std.mem.eql(u8, child.name, name)) continue;
                node = child;
                found = true;
                break;
            }
            if (!found) return null;
        }
        return node;
    }
};
