const std = @import("std");
const module = @import("module.zig");
const ast = @import("ast.zig");
const UUID = @import("utils/uuid.zig").UUID;
const parser_test = @import("parser.test.zig");
const Token = @import("token.zig").Token;
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

    fn validateFile(file: *module.File, allocator: std.mem.Allocator) ![]const u8 {
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

    pub fn localizeStatement(stmt: ast.Statement, count: *usize, buf: *std.ArrayList(u8)) !void {
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
    fn localizeExpr(stmt: *const ast.Statement, count: *usize, buf: *std.ArrayList(u8)) Error!void {
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

    fn exportStatement(stmt: ast.Statement, writer: anytype) !void {
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

test "Localization Ids" {
    const input =
        \\ const str = "testing"
        \\ var added = "one" + "two"
        \\ const alreadySet = "set"
        \\ === START {
        \\    :Speaker: "Dialogue content without id"
        \\    :Speaker: "More dialogue with id"@JUWH59VY-LRIGSPSB
        \\    fork {
        \\        ~ "Choice without id" => END
        \\    }
        \\    :Speaker: "Interpolated values {if str == "testing" "then value" else "else value"}"
        \\    :Speaker: "Partial id malformed"@1234
        \\    :: "Missing ID"@
        \\ }
    ;

    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const update = try Locale.validateFile(file, std.testing.allocator);
    defer std.testing.allocator.free(update);
    std.log.warn("{s}", .{update});
}
test "Update File Localization Ids" {
    const input =
        \\ const str = "testing"
        \\ var added = "one" + "two"
        \\ const alreadySet = "set"
        \\ === START {
        \\    :Speaker: "Dialogue content withhout id"
        \\    :Speaker: "More dialogue with id"@JUWH59VY-LRIGSPSB
        \\    fork {
        \\        ~ "Choice without id" => END
        \\        ~ "Choice two" {
        \\            :Speaker: "Inner dialogue of choice"    
        \\        }
        \\    }
        \\    :Speaker: "Interpolated values {if str == "testing" "then value" else "else value"}"
        \\    :Speaker: "Partial id malformed"@1234
        \\    :: "Missing ID"@
        \\ }
    ;

    const file_name = "test_locale.topi";
    var file = try std.fs.cwd().createFile(file_name, .{ .read = true });
    defer std.fs.cwd().deleteFile(file_name) catch {};
    try file.writeAll(input);
    const full_path = try std.fs.cwd().realpathAlloc(std.testing.allocator, file_name);
    defer std.testing.allocator.free(full_path);
    const validated = try Locale.validateFileAtPath(full_path, std.testing.allocator);
    defer std.testing.allocator.free(validated);
    try file.seekTo(0);
    try file.writeAll(validated);

    var buf_reader = std.io.bufferedReader(file.reader());
    const reader = buf_reader.reader();

    const out = std.io.getStdOut().writer();

    while (true) {
        reader.streamUntilDelimiter(out, '\n', null) catch break;
    }
}

test "Export Localization CSV Tree" {
    const input =
        \\ const num = 15
        \\ === START {
        \\     :: "A person approaches."@8R955KPX-2WI5R816
        \\     :Stranger: "Hey there."@C5I6VN71-IP0HPJHE
        \\     fork^ {
        \\         ~ "Greet them."@JTCCIIS7-NHTNWTBL {
        \\             :Drew: "Oh, uh, nice to meet you. My name is Drew."@8T8YW3LX-RNGWJE68
        \\             :Drew: "Sorry, I thought you were someone I knew."@8LIQ3QJV-5U3AJJKV
        \\             :Drew: "I'd love to stay and chat, but this is just a short demo."@YPTY00G5-1WX98ONH
        \\         }
        \\         ~ "Say nothing."@AEPZ4SNT-UFN9U9YW {
        \\             :: "The person acts as though they were addressing someone else."@S6MF4G1X-34IOPNOJ
        \\         }
        \\     }
        \\     :: "They walk away... Counting down from {num}"@KPTQNK2P-69OMTGXF
        \\ }
    ;

    const mod = try parser_test.parseSource(input);
    defer mod.deinit();
    const file = mod.entry;
    const writer = std.io.getStdOut().writer();
    try Locale.exportFile(file, writer);
}
