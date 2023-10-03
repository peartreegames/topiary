const std = @import("std");
const OpCode = @import("./opcode.zig").OpCode;

/// Used to keep track of jump locations by scope name
pub const JumpTree = struct {
    allocator: std.mem.Allocator,
    root: *Node,

    pub const Entry = struct {
        node: *Node,
        jump_ip: OpCode.Size(.jump),
    };

    pub const Node = struct {
        parent: ?*Node,
        name: []const u8,
        children: std.ArrayList(*Node),
        ip: OpCode.Size(.jump) = 9999,

        pub fn create(allocator: std.mem.Allocator, name: []const u8, parent: ?*Node) !*Node {
            var node = try allocator.create(Node);
            node.* = .{
                .parent = parent,
                .name = name,
                .children = std.ArrayList(*Node).init(allocator),
            };
            return node;
        }

        pub fn destroy(self: *const Node, allocator: std.mem.Allocator) void {
            for (self.children.items) |child| {
                child.destroy(allocator);
            }
            self.children.deinit();
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
                writer.writeAll(".") catch break;
            }
        }

        pub fn print(self: *const Node, writer: anytype, depth: usize) void {
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

    pub fn init(allocator: std.mem.Allocator) JumpTree {
        return .{
            .allocator = allocator,
            .root = undefined,
        };
    }

    pub fn deinit(self: *JumpTree) void {
        self.root.destroy(self.allocator);
    }

    pub fn print(self: *JumpTree, writer: anytype) void {
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
