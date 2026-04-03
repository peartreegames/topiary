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

    pub const magic = "TPLC";
    pub const version: u16 = 1;
    pub const header_size = magic.len + @sizeOf(u16) + @sizeOf(C.CONSTANT);

    pub const IndexEntry = struct {
        id: UUID.ID,
        offset: C.CONSTANT,
        length: u32,

        pub const Size = UUID.Size + @sizeOf(C.CONSTANT) + @sizeOf(u32);
    };

    pub fn init(allocator: std.mem.Allocator, key: []const u8, buffer: []const u8) !*LocaleProvider {
        var fbs = std.io.fixedBufferStream(buffer);
        var reader = fbs.reader();

        // Validate magic and version
        var file_magic: [4]u8 = undefined;
        _ = try reader.readAll(&file_magic);
        if (!std.mem.eql(u8, &file_magic, magic)) return error.InvalidLocaleFormat;
        const file_version = try reader.readInt(u16, .little);
        if (file_version != version) return error.UnsupportedLocaleVersion;

        const count = try reader.readInt(C.CONSTANT, .little);
        var map: std.AutoHashMapUnmanaged(UUID.ID, []const u8) = .empty;

        const table_size = count * IndexEntry.Size;
        const blob_start = header_size + table_size;

        for (0..count) |_| {
            var entry: IndexEntry = undefined;
            _ = try reader.readAll(&entry.id);
            entry.offset = try reader.readInt(C.CONSTANT, .little);
            entry.length = try reader.readInt(u32, .little);

            const start = blob_start + entry.offset;
            const end = start + entry.length;
            if (end > buffer.len) return error.CorruptLocaleFile;
            const str = buffer[start..end];
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
    // This could be refactored to a fmt with an option to add localization ids
    // Would need to modify so every node in the ast writes its output
    pub fn validateFileAtPath(full_path: []const u8, allocator: std.mem.Allocator) ![]const u8 {
        var mod = try Module.init(allocator, full_path);
        defer mod.deinit();
        try mod.entry.loadSource();
        try mod.entry.buildTree();
        return validateFile(mod.entry, allocator);
    }

    pub fn validateFile(file: *module.File, alloc: std.mem.Allocator) ![]const u8 {
        const source = file.source orelse return error.FileSourceNotLoaded;
        const tree = file.tree orelse return error.FileTreeNotLoaded;
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(alloc);
        try buf.writer(alloc).writeAll(source);
        var count: usize = 0;

        try walkLocalizable(tree.root, LocalizeContext{ .alloc = alloc, .count = &count, .buf = &buf }, LocalizeContext.onLeaf);
        return buf.toOwnedSlice(alloc);
    }

    fn walkLocalizable(stmts: []const Statement, context: anytype, comptime onLeaf: fn (@TypeOf(context), Statement) @TypeOf(context).Error!void) @TypeOf(context).Error!void {
        for (stmts) |stmt| {
            switch (stmt.type) {
                .block => |b| try walkLocalizable(b, context, onLeaf),
                .bough => |b| try walkLocalizable(b.body, context, onLeaf),
                .choice => |c| {
                    try onLeaf(context, stmt);
                    try walkLocalizable(c.body, context, onLeaf);
                },
                .dialogue => try onLeaf(context, stmt),
                .@"for" => |f| try walkLocalizable(f.body, context, onLeaf),
                .fork => |f| try walkLocalizable(f.body, context, onLeaf),
                .@"if" => |i| {
                    try walkLocalizable(i.then_branch, context, onLeaf);
                    if (i.else_branch) |e| try walkLocalizable(e, context, onLeaf);
                },
                .@"while" => |w| try walkLocalizable(w.body, context, onLeaf),
                .@"switch" => |s| try walkLocalizable(s.prongs, context, onLeaf),
                .switch_prong => |sp| try walkLocalizable(sp.body, context, onLeaf),
                else => {},
            }
        }
    }

    const LocalizeContext = struct {
        alloc: std.mem.Allocator,
        count: *usize,
        buf: *std.ArrayList(u8),

        pub const Error = error{ OutOfMemory, NoSpaceLeft };

        // very error prone and modifying the source isn't great, but works for now
        fn onLeaf(ctx: LocalizeContext, stmt: Statement) @This().Error!void {
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
                const start = token.?.end + ctx.count.* + 1;
                const new_id = UUID.new();
                var tmp: [UUID.Size + 1]u8 = undefined;
                _ = try std.fmt.bufPrint(&tmp, "@{s}", .{new_id});
                // handle malformed ids
                if (ctx.buf.items[start] == '@') {
                    var end: usize = start;
                    while (end < ctx.buf.items.len and ctx.buf.items[end] != ' ' and ctx.buf.items[end] != 0 and ctx.buf.items[end] != '\n' and ctx.buf.items[end] != '#' and ctx.buf.items[end] != '}' and ctx.buf.items[end] != '{' and ctx.buf.items[end] != ')') : (end += 1) {}
                    const len = end - start;
                    try ctx.buf.replaceRange(ctx.alloc, start, len, &tmp);
                    ctx.count.* += UUID.Size + 1 - len;
                } else {
                    try ctx.buf.insertSlice(ctx.alloc, start, &tmp);
                    ctx.count.* += UUID.Size + 1;
                }
            }
        }
    };

    const ExportContext = struct {
        writer: *std.Io.Writer,

        pub const Error = error{WriteFailure};

        fn onLeaf(ctx: ExportContext, stmt: Statement) @This().Error!void {
            switch (stmt.type) {
                .choice => |c| {
                    if (UUID.isEmpty(c.id) or UUID.isAuto(c.id)) return error.WriteFailure;
                    const str = c.content.type.string;
                    ctx.writer.print("\"{s}\",\"CHOICE\",\"{s}\",\"{s}\"\n", .{ &c.id, str.raw, str.value }) catch return error.WriteFailure;
                },
                .dialogue => |d| {
                    if (UUID.isEmpty(d.id) or UUID.isAuto(d.id)) return error.WriteFailure;
                    const str = d.content.type.string;
                    ctx.writer.print("\"{s}\",\"{s}\",\"{s}\",\"{s}\"\n", .{ &d.id, d.speaker orelse "NONE", str.raw, str.value }) catch return error.WriteFailure;
                },
                else => unreachable,
            }
        }
    };

    pub fn exportFileAtPath(full_path: []const u8, writer: *std.Io.Writer, allocator: std.mem.Allocator, base_lang: []const u8) !void {
        var mod = try Module.init(allocator, full_path);
        defer mod.deinit();
        try mod.entry.loadSource();
        try mod.entry.buildTree();
        try exportFile(mod.entry, writer, base_lang);
    }

    // Basic implementation
    // Ideally this would check for existing ids already in the file
    // and only update the raw/base values, rather than replace the entire file
    pub fn exportFile(file: *File, writer: *std.Io.Writer, base_lang: []const u8) !void {
        const tree = file.tree orelse return error.NotInitialized;
        try writer.print("\"id\",\"speaker\",\"raw\",\"{s}\"\n", .{base_lang});
        try walkLocalizable(tree.root, ExportContext{ .writer = writer }, ExportContext.onLeaf);
    }

    fn stripBom(content: []const u8) []const u8 {
        if (content.len >= 3 and content[0] == 0xEF and content[1] == 0xBB and content[2] == 0xBF) {
            return content[3..];
        }
        return content;
    }

    fn nextCsvLine(data: []const u8, pos: *usize) ?[]const u8 {
        if (pos.* >= data.len) return null;
        const start = pos.*;
        var in_quotes = false;
        while (pos.* < data.len) : (pos.* += 1) {
            const ch = data[pos.*];
            if (ch == '"') {
                in_quotes = !in_quotes;
            } else if (!in_quotes and (ch == '\n' or ch == '\r')) {
                const line = data[start..pos.*];
                // Skip \r\n or \n\r pairs
                pos.* += 1;
                if (pos.* < data.len and data[pos.*] != ch and (data[pos.*] == '\n' or data[pos.*] == '\r')) {
                    pos.* += 1;
                }
                return line;
            }
        }
        // Last line without trailing newline
        if (start < data.len) return data[start..];
        return null;
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

        const raw_content = try file.readToEndAlloc(allocator, std.math.maxInt(u32));
        defer allocator.free(raw_content);
        const content = stripBom(raw_content);

        var pos: usize = 0;
        const header = nextCsvLine(content, &pos) orelse return error.InvalidCsv;

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

    pub fn generate(allocator: std.mem.Allocator, raw_content: []const u8, lang_col: usize, writer: *std.Io.Writer) !void {
        const content = stripBom(raw_content);
        var pos: usize = 0;
        _ = nextCsvLine(content, &pos); // skip header

        var entries: std.ArrayList(LocaleProvider.IndexEntry) = .empty;
        defer entries.deinit(allocator);
        var strings: std.ArrayList(u8) = .empty;
        defer strings.deinit(allocator);

        while (nextCsvLine(content, &pos)) |line| {
            var current_cell: usize = 0;
            var id_str: ?[]const u8 = null;
            var target_val: ?[]const u8 = null;

            var i: usize = 0;
            while (i < line.len) {
                const start = i;
                var end = i;
                if (line[i] == '"') {
                    i += 1;
                    while (i < line.len) : (i += 1) {
                        if (line[i] == '"') {
                            // Handle escaped quotes ("") in CSV
                            if (i + 1 < line.len and line[i + 1] == '"') {
                                i += 1; // skip the second quote, loop advances past it
                            } else {
                                break; // closing quote
                            }
                        }
                    }
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
            // Unescape CSV double-quotes ("" → ")
            const tv = target_val.?;
            var j: usize = 0;
            while (j < tv.len) : (j += 1) {
                if (tv[j] == '"' and j + 1 < tv.len and tv[j + 1] == '"') {
                    try strings.append(allocator, '"');
                    j += 1; // skip second quote
                } else {
                    try strings.append(allocator, tv[j]);
                }
            }

            try entries.append(allocator, .{
                .id = UUID.fromString(id_str.?),
                .offset = offset,
                .length = @intCast(strings.items.len - offset),
            });
        }

        try writer.writeAll(LocaleProvider.magic);
        try writer.writeInt(u16, LocaleProvider.version, .little);
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
