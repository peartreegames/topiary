const std = @import("std");
const frontend = @import("frontend/index.zig");
const Statement = frontend.Statement;
const Token = frontend.Token;

const utils = @import("utils/index.zig");
const UUID = utils.UUID;
const C = utils.C;
const fmt = utils.fmt;

const module = @import("module.zig");
const Module = module.Module;
const File = module.File;

pub const LocaleProvider = struct {
    key: []const u8,
    buffer: []const u8,
    map: std.AutoHashMapUnmanaged(UUID.ID, []const u8),

    pub const IndexEntry = struct {
        id: UUID.ID,
        offset: C.CONSTANT,
        length: u32,

        pub const Size = UUID.Size + @sizeOf(C.CONSTANT) + @sizeOf(u32);
    };

    pub fn init(allocator: std.mem.Allocator, key: []const u8, buffer: []const u8) !*LocaleProvider {
        var fbs = std.io.fixedBufferStream(buffer);
        var reader = fbs.reader();

        const count = try reader.readInt(C.CONSTANT, .little);
        var map: std.AutoHashMapUnmanaged(UUID.ID, []const u8) = .empty;

        const header_size = @sizeOf(C.CONSTANT);
        const table_size = count * IndexEntry.Size;
        const blob_start = header_size + table_size;

        for (0..count) |_| {
            var entry: IndexEntry = undefined;
            _ = try reader.readAll(&entry.id);
            entry.offset = try reader.readInt(C.CONSTANT, .little);
            entry.length = try reader.readInt(u32, .little);

            const str = buffer[blob_start + entry.offset .. blob_start + entry.offset + entry.length];
            try map.put(allocator, entry.id, str);
        }

        const lp = try allocator.create(LocaleProvider);
        lp.* = .{
            .key = try allocator.dupe(u8, key),
            .buffer = buffer,
            .map = map,
        };
        return lp;
    }

    pub fn deinit(self: *LocaleProvider, allocator: std.mem.Allocator) void {
        self.map.deinit(allocator);
        allocator.free(self.key);
        allocator.free(self.buffer);
        allocator.destroy(self);
    }
};

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

    pub fn validateFile(file: *module.File, alloc: std.mem.Allocator) ![]const u8 {
        if (!file.source_loaded) return error.FileSourceNotLoaded;
        if (!file.tree_loaded) return error.FileTreeNotLoaded;
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(alloc);
        try buf.writer(alloc).writeAll(file.source);
        var count: usize = 0;

        for (file.tree.root) |stmt| {
            try localizeStatement(alloc, stmt, &count, &buf);
        }
        return buf.toOwnedSlice(alloc);
    }

    pub fn localizeStatement(alloc: std.mem.Allocator, stmt: Statement, count: *usize, buf: *std.ArrayList(u8)) !void {
        switch (stmt.type) {
            .block => |b| {
                for (b) |s| try localizeStatement(alloc, s, count, buf);
            },
            .bough => |b| {
                for (b.body) |s| try localizeStatement(alloc, s, count, buf);
            },
            .choice => |c| {
                try localizeExpr(alloc, &stmt, count, buf);
                for (c.body) |s| try localizeStatement(alloc, s, count, buf);
            },
            .dialogue => try localizeExpr(alloc, &stmt, count, buf),
            .@"for" => |f| {
                for (f.body) |s| try localizeStatement(alloc, s, count, buf);
            },
            .fork => |f| {
                for (f.body) |s| try localizeStatement(alloc, s, count, buf);
            },
            .@"if" => |i| {
                for (i.then_branch) |s| try localizeStatement(alloc, s, count, buf);
                if (i.else_branch) |e| {
                    for (e) |s| try localizeStatement(alloc, s, count, buf);
                }
            },
            .@"while" => |w| {
                for (w.body) |s| try localizeStatement(alloc, s, count, buf);
            },
            .@"switch" => |s| {
                for (s.prongs) |p| try localizeStatement(alloc, p, count, buf);
            },
            .switch_prong => |sp| {
                for (sp.body) |b| try localizeStatement(alloc, b, count, buf);
            },
            else => {},
        }
    }

    // very error prone and modifying the source isn't great, but works for now
    fn localizeExpr(alloc: std.mem.Allocator, stmt: *const Statement, count: *usize, buf: *std.ArrayList(u8)) Error!void {
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
                try buf.replaceRange(alloc, start, len, &tmp);
                count.* += UUID.Size + 1 - len;
            } else {
                try buf.insertSlice(alloc, start, &tmp);
                count.* += UUID.Size + 1;
            }
        }
    }

    pub fn exportFileAtPath(full_path: []const u8, writer: *std.Io.Writer, allocator: std.mem.Allocator) !void {
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
    pub fn exportFile(file: *File, writer: *std.Io.Writer) !void {
        try writer.writeAll("\"id\",\"speaker\",\"raw\",\"en\"\n");
        for (file.tree.root) |stmt| {
            try exportStatement(stmt, writer);
        }
    }

    fn exportStatement(stmt: Statement, writer: *std.Io.Writer) !void {
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

    pub fn generateAtPath(
        allocator: std.mem.Allocator,
        csv_path: []const u8,
        output_folder: []const u8,
        filter_lang: ?[]const u8,
        dry: bool,
    ) !void {
        const file = try std.fs.cwd().openFile(csv_path, .{});
        const file_name = std.mem.trimEnd(u8, std.fs.path.basename(csv_path), ".topi.csv");
        defer file.close();

        const content = try file.readToEndAlloc(allocator, std.math.maxInt(u32));
        defer allocator.free(content);

        var lines_it = std.mem.tokenizeAny(u8, content, "\n\r");
        const header = lines_it.next() orelse return error.InvalidCsv;

        var header_it = std.mem.tokenizeAny(u8, header, ",");
        var col_idx: usize = 0;
        while (header_it.next()) |h| : (col_idx += 1) {
            const key = std.mem.trim(u8, h, "\" ");
            if (std.mem.eql(u8, key, "id") or std.mem.eql(u8, key, "speaker") or std.mem.eql(u8, key, "raw")) continue;
            if (filter_lang) |f| if (!std.mem.eql(u8, key, f)) continue;

            if (!dry) {
                const out_name = try std.fmt.allocPrint(allocator, "{s}/{s}.{s}.topil", .{ output_folder, file_name, key });
                defer allocator.free(out_name);
                const out_file = try std.fs.cwd().createFile(out_name, .{});
                defer out_file.close();
                var file_buf: [1024]u8 = undefined;
                var file_writer = out_file.writer(&file_buf);
                const writer = &file_writer.interface;
                try generate(allocator, content, col_idx,  writer);
            }
        }
    }

    pub fn generate(allocator: std.mem.Allocator, content: []const u8, lang_col: usize, writer: *std.Io.Writer) !void {
        var lines_it = std.mem.tokenizeAny(u8, content, "\n\r");
        _ = lines_it.next(); // skip header

        var entries: std.ArrayList(LocaleProvider.IndexEntry) = .empty;
        defer entries.deinit(allocator);
        var strings: std.ArrayList(u8) = .empty;
        defer strings.deinit(allocator);

        while (lines_it.next()) |line| {
            var current_cell: usize = 0;
            var id_str: ?[]const u8 = null;
            var target_val: ?[]const u8 = null;

            var i: usize = 0;
            while (i < line.len) {
                const start = i;
                var end = i;
                if (line[i] == '"') {
                    i += 1;
                    while (i < line.len and line[i] != '"') : (i += 1) {}
                    end = i;
                    if (i < line.len) i += 1; // skip closing quote
                } else {
                    while (i < line.len and line[i] != ',') : (i += 1) {}
                    end = i;
                }

                const cell = line[start..end];
                const trimmed = std.mem.trim(u8, cell, "\" ");

                if (current_cell == 0) {
                    if (trimmed.len >= UUID.Size) {
                        id_str = trimmed[0..UUID.Size];
                    }
                }
                if (current_cell == lang_col) {
                    target_val = trimmed;
                }

                // Skip the comma for the next iteration
                if (i < line.len and line[i] == ',') i += 1;
                current_cell += 1;

                if (id_str != null and target_val != null and current_cell > lang_col) break;
            }

            if (id_str == null or target_val == null) continue;

            const offset: u32 = @intCast(strings.items.len);
            try strings.appendSlice(allocator, target_val.?);

            try entries.append(allocator, .{
                .id = UUID.fromString(id_str.?),
                .offset = offset,
                .length = @intCast(target_val.?.len),
            });
        }

        try writer.writeInt(C.CONSTANT, @intCast(entries.items.len), .little);
        for (entries.items) |entry| {
            try writer.writeAll(&entry.id);
            try writer.writeInt(C.CONSTANT, entry.offset, .little);
            try writer.writeInt(u32, entry.length, .little);
        }
        try writer.writeAll(strings.items);
        try writer.flush();
    }
};
