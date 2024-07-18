const std = @import("std");
const frontend = @import("frontend/index.zig");
const Statement = frontend.Statement;
const Token = frontend.Token;

const utils = @import("utils/index.zig");
const UUID = utils.UUID;

const module = @import("module.zig");
const Module = module.Module;
const File = module.File;

pub const Locale = struct {
    const Error = error{
        OutOfMemory,
        NoSpaceLeft,
    };

    // This could be refactored to a fmt with an option to add localization ids
    // Would need to modify so every node in the ast writes its output
    pub fn validateFileAtPath(full_path: []const u8, allocator: std.mem.Allocator) ![]const u8 {
        var mod = try Module.init(allocator, full_path);
        mod.allow_includes = false;
        defer mod.deinit();
        try mod.entry.loadSource();
        try mod.entry.buildTree();
        return validateFile(mod.entry, allocator);
    }

    pub fn validateFile(file: *module.File, allocator: std.mem.Allocator) ![]const u8 {
        if (!file.source_loaded) return error.FileSourceNotLoaded;
        if (!file.tree_loaded) return error.FileTreeNotLoaded;
        var buf = std.ArrayList(u8).init(allocator);
        defer buf.deinit();
        try buf.writer().writeAll(file.source);
        var count: usize = 0;

        for (file.tree.root) |stmt| {
            try localizeStatement(stmt, &count, &buf);
        }
        return buf.toOwnedSlice();
    }

    pub fn localizeStatement(stmt: Statement, count: *usize, buf: *std.ArrayList(u8)) !void {
        switch (stmt.type) {
            .block => |b| {
                for (b) |s| try localizeStatement(s, count, buf);
            },
            .bough => |b| {
                for (b.body) |s| try localizeStatement(s, count, buf);
            },
            .choice => |c| {
                try localizeExpr(&stmt, count, buf);
                for (c.body) |s| try localizeStatement(s, count, buf);
            },
            .dialogue => try localizeExpr(&stmt, count, buf),
            .@"for" => |f| {
                for (f.body) |s| try localizeStatement(s, count, buf);
            },
            .fork => |f| {
                for (f.body) |s| try localizeStatement(s, count, buf);
            },
            .@"if" => |i| {
                for (i.then_branch) |s| try localizeStatement(s, count, buf);
                if (i.else_branch) |e| {
                    for (e) |s| try localizeStatement(s, count, buf);
                }
            },
            .@"while" => |w| {
                for (w.body) |s| try localizeStatement(s, count, buf);
            },
            .@"switch" => |s| {
                for (s.prongs) |p| try localizeStatement(p, count, buf);
            },
            .switch_prong => |sp| {
                for (sp.body) |b| try localizeStatement(b, count, buf);
            },
            else => {},
        }
    }

    // very error prone and modifying the source isn't great, but works for now
    fn localizeExpr(stmt: *const Statement, count: *usize, buf: *std.ArrayList(u8)) Error!void {
        const id: UUID.ID = switch (stmt.type) {
            .choice => |c| c.id,
            .dialogue => |d| d.id,
            else => unreachable,
        };
        if (UUID.isEmpty(id) or UUID.isAuto(id)) {
            const token: ?Token = switch (stmt.type) {
                .choice => |c| c.content.token,
                .dialogue => |d| d.content.token,
                else => null,
            };
            if (token == null) return;
            const start = token.?.end + count.* + 1;
            const new_id = UUID.new();
            var tmp: [UUID.Size + 1]u8 = undefined;
            _ = try std.fmt.bufPrint(&tmp, "@{s}", .{new_id});
            // handle malformed ids
            if (buf.items[start] == '@') {
                var end: usize = start;
                while (buf.items[end] != ' ' and buf.items[end] != 0 and buf.items[end] != '\n') : (end += 1) {}
                const len = end - start;
                try buf.replaceRange(start, len, &tmp);
                count.* += UUID.Size + 1 - len;
            } else {
                try buf.insertSlice(start, &tmp);
                count.* += UUID.Size + 1;
            }
        }
    }

    pub fn exportFileAtPath(full_path: []const u8, writer: anytype, allocator: std.mem.Allocator) !void {
        var mod = try Module.init(allocator, full_path);
        mod.allow_includes = false;
        defer mod.deinit();
        try mod.entry.loadSource();
        try mod.entry.buildTree();
        try exportFile(mod.entry, writer);
    }

    // Basic implementation
    // Ideally this would check for existing ids already in the file
    // and only update the raw/base values, rather than replace the entire file
    // base language should be configurable as well
    pub fn exportFile(file: *File, writer: anytype) !void {
        try writer.writeAll("\"id\",\"speaker\",\"raw\",\"en\"\n");
        for (file.tree.root) |stmt| {
            try exportStatement(stmt, writer);
        }
    }

    fn exportStatement(stmt: Statement, writer: anytype) !void {
        switch (stmt.type) {
            .block => |b| {
                for (b) |s| try exportStatement(s, writer);
            },
            .bough => |b| {
                for (b.body) |s| try exportStatement(s, writer);
            },
            .choice => |c| {
                if (UUID.isEmpty(c.id) or UUID.isAuto(c.id)) return error.InvalidLocazationId;
                const str = c.content.type.string;
                try writer.print("\"{s}\",\"CHOICE\",\"{s}\",\"{s}\"\n", .{ &c.id, str.raw, str.value });
                for (c.body) |s| try exportStatement(s, writer);
            },
            .dialogue => |d| {
                if (UUID.isEmpty(d.id) or UUID.isAuto(d.id)) return error.InvalidLocazationId;
                const str = d.content.type.string;
                try writer.print("\"{s}\",\"{s}\",\"{s}\",\"{s}\"\n", .{ &d.id, d.speaker orelse "NONE", str.raw, str.value });
            },
            .@"for" => |f| {
                for (f.body) |s| try exportStatement(s, writer);
            },
            .fork => |f| {
                for (f.body) |s| try exportStatement(s, writer);
            },
            .@"if" => |i| {
                for (i.then_branch) |s| try exportStatement(s, writer);
                if (i.else_branch) |e| {
                    for (e) |s| try exportStatement(s, writer);
                }
            },
            .@"while" => |w| {
                for (w.body) |s| try exportStatement(s, writer);
            },
            .@"switch" => |s| {
                for (s.prongs) |p| try exportStatement(p, writer);
            },
            .switch_prong => |sp| {
                for (sp.body) |b| try exportStatement(b, writer);
            },
            else => {},
        }
    }
};
