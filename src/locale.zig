const std = @import("std");
const frontend = @import("frontend/index.zig");
const Statement = frontend.Statement;
const Token = frontend.Token;

const utils = @import("utils/index.zig");
const UUID = utils.UUID;
const C = utils.C;
const fmt = utils.fmt;

const types = @import("types/index.zig");
const Segment = types.Segment;
const StringData = types.StringData;

const default_io = std.Io.Threaded.global_single_threaded.io();

const module = @import("module.zig");
const Module = module.Module;
const File = module.File;

pub const LocaleProvider = struct {
    key: []const u8,
    buffer: []const u8,
    map: std.AutoHashMapUnmanaged(UUID.ID, StringData),

    pub const magic = "TPLC";
    pub const version: u16 = 2;
    /// magic + version + entry count + string blob length
    pub const header_size = magic.len + @sizeOf(u16) + @sizeOf(C.CONSTANT) + @sizeOf(u32);

    pub const IndexEntry = struct {
        id: UUID.ID,
        str_offset: C.CONSTANT,
        str_length: u32,
        seg_offset: u32,
        seg_count: u8,

        pub const Size = UUID.Size + @sizeOf(C.CONSTANT) + @sizeOf(u32) + @sizeOf(u32) + @sizeOf(u8);
    };

    pub fn init(allocator: std.mem.Allocator, key: []const u8, buffer: []const u8) !*LocaleProvider {
        var reader = std.Io.Reader.fixed(buffer);

        // Validate magic and version
        const file_magic = try reader.takeArray(4);
        if (!std.mem.eql(u8, file_magic, magic)) return error.InvalidLocaleFormat;
        const file_version = try reader.takeVarInt(u16, .little, 2);
        if (file_version != version) return error.UnsupportedLocaleVersion;

        const count = try reader.takeVarInt(C.CONSTANT, .little, @sizeOf(C.CONSTANT));
        const string_blob_size = try reader.takeVarInt(u32, .little, 4);
        var map: std.AutoHashMapUnmanaged(UUID.ID, StringData) = .empty;
        errdefer {
            var it = map.valueIterator();
            while (it.next()) |sd| allocator.free(sd.segments);
            map.deinit(allocator);
        }

        const table_size = std.math.mul(C.CONSTANT, count, IndexEntry.Size) catch return error.CorruptLocaleFile;
        const string_blob_start = header_size + table_size;
        if (string_blob_start > buffer.len) return error.CorruptLocaleFile;
        const segment_blob_start = std.math.add(usize, string_blob_start, string_blob_size) catch return error.CorruptLocaleFile;
        if (segment_blob_start > buffer.len) return error.CorruptLocaleFile;
        const segment_blob = buffer[segment_blob_start..];

        for (0..count) |_| {
            var entry: IndexEntry = undefined;
            entry.id = (try reader.takeArray(UUID.Size)).*;
            entry.str_offset = try reader.takeVarInt(C.CONSTANT, .little, @sizeOf(C.CONSTANT));
            entry.str_length = try reader.takeVarInt(u32, .little, 4);
            entry.seg_offset = try reader.takeVarInt(u32, .little, 4);
            entry.seg_count = try reader.takeByte();

            // Bytes slice into the buffer we keep alive on the provider.
            const str_start = string_blob_start + entry.str_offset;
            const str_end = str_start + entry.str_length;
            if (str_end > segment_blob_start) return error.CorruptLocaleFile;
            const bytes = buffer[str_start..str_end];

            // Decode the segment table from the segment blob. Each entry's
            // segments are owned by the provider's allocator; the bytes
            // themselves stay in the file buffer.
            if (entry.seg_offset > segment_blob.len) return error.CorruptLocaleFile;
            var seg_reader = std.Io.Reader.fixed(segment_blob[entry.seg_offset..]);
            const segments = try allocator.alloc(Segment, entry.seg_count);
            errdefer allocator.free(segments);
            for (segments) |*seg| seg.* = try readSegment(&seg_reader, entry.str_length);

            try map.put(allocator, entry.id, .{ .bytes = bytes, .segments = segments });
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
        var it = self.map.valueIterator();
        while (it.next()) |sd| allocator.free(sd.segments);
        self.map.deinit(allocator);
        allocator.free(self.key);
        allocator.free(self.buffer);
        allocator.destroy(self);
    }
};

/// Wire format for a single segment, used by the .topil v2 segment blob.
/// Mirrors the bytecode encoding in src/types/value.zig (writeSegment).
/// Keeping this self-contained here avoids leaking value.zig's internal
/// helper. Validates literal bounds against the entry's string length.
fn readSegment(reader: *std.Io.Reader, str_length: u32) !Segment {
    const tag: std.meta.Tag(Segment) = @enumFromInt(try reader.takeByte());
    switch (tag) {
        .literal => {
            const start = try reader.takeInt(u16, .little);
            const end = try reader.takeInt(u16, .little);
            if (start > end or end > str_length) return error.CorruptLocaleFile;
            return .{ .literal = .{ .start = start, .end = end } };
        },
        .interp => return .{ .interp = try reader.takeByte() },
    }
}

/// Wire-format bytes for a single segment, appended to the .topil
/// generator's segment blob. Mirrors the readSegment decoder above.
fn appendSegmentBytes(allocator: std.mem.Allocator, buf: *std.ArrayList(u8), seg: Segment) !void {
    try buf.append(allocator, @intFromEnum(@as(std.meta.Tag(Segment), seg)));
    switch (seg) {
        .literal => |r| {
            var le: [4]u8 = undefined;
            std.mem.writeInt(u16, le[0..2], r.start, .little);
            std.mem.writeInt(u16, le[2..4], r.end, .little);
            try buf.appendSlice(allocator, &le);
        },
        .interp => |i| try buf.append(allocator, i),
    }
}

pub const Locale = struct {
    // This could be refactored to a fmt with an option to add localization ids
    // Would need to modify so every node in the ast writes its output
    pub fn checkFileAtPath(full_path: []const u8, allocator: std.mem.Allocator) !usize {
        var mod = try Module.init(allocator, default_io, full_path);
        defer mod.deinit();
        try mod.entry.loadSource();
        try mod.entry.buildTree();
        return checkFile(mod.entry);
    }

    pub fn checkFile(file: *module.File) !usize {
        const tree = file.tree orelse return error.FileTreeNotLoaded;
        var count: usize = 0;
        try walkLocalizable(tree.root, CheckContext{ .count = &count }, CheckContext.onLeaf);
        return count;
    }

    pub fn validateFileAtPath(full_path: []const u8, allocator: std.mem.Allocator) ![]const u8 {
        var mod = try Module.init(allocator, default_io, full_path);
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
        var last_pos: usize = 0;

        try walkLocalizable(tree.root, LocalizeContext{
            .alloc = alloc,
            .source = source,
            .buf = &buf,
            .last_pos = &last_pos,
        }, LocalizeContext.onLeaf);

        try buf.appendSlice(alloc, source[last_pos..]);
        return buf.toOwnedSlice(alloc);
    }

    fn walkLocalizable(stmts: []const Statement, context: anytype, comptime onLeaf: fn (@TypeOf(context), *const Statement) @TypeOf(context).Error!void) @TypeOf(context).Error!void {
        for (stmts) |*stmt| {
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
        source: []const u8,
        buf: *std.ArrayList(u8),
        last_pos: *usize,

        pub const Error = error{ OutOfMemory, NoSpaceLeft };

        fn onLeaf(ctx: LocalizeContext, stmt: *const Statement) @This().Error!void {
            const has_valid_source_id = switch (stmt.type) {
                .choice => |c| c.id_token != null and !UUID.isEmpty(c.id),
                .dialogue => |d| d.id_token != null and !UUID.isEmpty(d.id),
                else => unreachable,
            };
            if (has_valid_source_id) return;

            const id_token: ?Token = switch (stmt.type) {
                .choice => |c| c.id_token,
                .dialogue => |d| d.id_token,
                else => null,
            };

            // Position after the closing " (handles both simple and interpolated strings)
            const after_quote: ?usize = switch (stmt.type) {
                .choice => |c| c.content.token.start + c.content.type.string.raw.len + 1,
                .dialogue => |d| d.content.token.start + d.content.type.string.raw.len + 1,
                else => null,
            };
            if (after_quote == null) return;

            // Copy source up to: the @ char if replacing, or after closing " if inserting
            const copy_end = if (id_token) |idt| idt.start else after_quote.?;
            try ctx.buf.appendSlice(ctx.alloc, ctx.source[ctx.last_pos.*..copy_end]);

            const new_id = UUID.new();
            var tmp: [UUID.Size + 1]u8 = undefined;
            _ = try std.fmt.bufPrint(&tmp, "@{s}", .{new_id});
            try ctx.buf.appendSlice(ctx.alloc, &tmp);

            ctx.last_pos.* = if (id_token) |idt| idt.end else after_quote.?;
        }
    };

    const CheckContext = struct {
        count: *usize,

        pub const Error = error{};

        fn onLeaf(ctx: CheckContext, stmt: *const Statement) @This().Error!void {
            const missing = switch (stmt.type) {
                .choice => |c| c.id_token == null or UUID.isEmpty(c.id),
                .dialogue => |d| d.id_token == null or UUID.isEmpty(d.id),
                else => false,
            };
            if (missing) ctx.count.* += 1;
        }
    };

    const CollectContext = struct {
        map: *std.AutoHashMap(UUID.ID, void),

        pub const Error = error{OutOfMemory};

        fn onLeaf(ctx: CollectContext, stmt: *const Statement) @This().Error!void {
            const id: UUID.ID = switch (stmt.type) {
                .choice => |c| if (c.id_token != null and !UUID.isEmpty(c.id)) c.id else return,
                .dialogue => |d| if (d.id_token != null and !UUID.isEmpty(d.id)) d.id else return,
                else => return,
            };
            try ctx.map.put(id, {});
        }
    };

    fn writeCsvField(writer: *std.Io.Writer, value: []const u8) error{WriteFailure}!void {
        writer.writeByte('"') catch return error.WriteFailure;
        for (value) |byte| {
            if (byte == '"') {
                writer.writeAll("\"\"") catch return error.WriteFailure;
            } else {
                writer.writeByte(byte) catch return error.WriteFailure;
            }
        }
        writer.writeByte('"') catch return error.WriteFailure;
    }

    /// Like `writeCsvField`, but rewrites each `{...}` interpolation
    /// span in the source text to `{... | N}` form so translators can
    /// see which positional index lines up with each named expression.
    /// `\{` escapes are preserved as literal braces (matching the
    /// lexer's escape rule). Repeated bodies share the same index —
    /// e.g. `"hi {bob}, are you {bob}?"` exports as
    /// `"hi {bob | 0}, are you {bob | 0}?"`, so a translator who reuses
    /// `{0}` produces the same value at both spots.
    fn writeRawFieldWithIndices(
        writer: *std.Io.Writer,
        raw: []const u8,
        alloc: std.mem.Allocator,
    ) (error{WriteFailure} || std.mem.Allocator.Error)!void {
        writer.writeByte('"') catch return error.WriteFailure;
        // Body text → first-assigned index. Bodies are normalized by
        // trimming whitespace so `{bob}` and `{ bob }` collapse together.
        var seen: std.StringHashMapUnmanaged(u32) = .empty;
        defer seen.deinit(alloc);
        var next_idx: u32 = 0;

        var i: usize = 0;
        while (i < raw.len) : (i += 1) {
            const c = raw[i];
            if (c == '\\' and i + 1 < raw.len) {
                writer.writeByte(c) catch return error.WriteFailure;
                writer.writeByte(raw[i + 1]) catch return error.WriteFailure;
                i += 1;
                continue;
            }
            if (c == '{') {
                var j = i + 1;
                while (j < raw.len and raw[j] != '}') : (j += 1) {}
                if (j >= raw.len) {
                    for (raw[i..]) |b| {
                        if (b == '"') writer.writeAll("\"\"") catch return error.WriteFailure
                        else writer.writeByte(b) catch return error.WriteFailure;
                    }
                    writer.writeByte('"') catch return error.WriteFailure;
                    return;
                }
                const body = std.mem.trim(u8, raw[i + 1 .. j], " \t");
                const gop = try seen.getOrPut(alloc, body);
                if (!gop.found_existing) {
                    gop.value_ptr.* = next_idx;
                    next_idx += 1;
                }
                writer.writeByte('{') catch return error.WriteFailure;
                for (body) |b| {
                    if (b == '"') writer.writeAll("\"\"") catch return error.WriteFailure
                    else writer.writeByte(b) catch return error.WriteFailure;
                }
                writer.print(" | {d}", .{gop.value_ptr.*}) catch return error.WriteFailure;
                writer.writeByte('}') catch return error.WriteFailure;
                i = j;
                continue;
            }
            if (c == '"') writer.writeAll("\"\"") catch return error.WriteFailure
            else writer.writeByte(c) catch return error.WriteFailure;
        }
        writer.writeByte('"') catch return error.WriteFailure;
    }

    const ExportContext = struct {
        writer: *std.Io.Writer,
        alloc: std.mem.Allocator,

        pub const Error = error{ WriteFailure, MissingStamp, OutOfMemory };

        fn onLeaf(ctx: ExportContext, stmt: *const Statement) @This().Error!void {
            switch (stmt.type) {
                .choice => |c| {
                    if (c.id_token == null or UUID.isEmpty(c.id)) return error.MissingStamp;
                    const str = c.content.type.string;
                    ctx.writer.print("\"{s}\",\"CHOICE\",", .{&c.id}) catch return error.WriteFailure;
                    try writeRawFieldWithIndices(ctx.writer, str.raw, ctx.alloc);
                    ctx.writer.writeAll(",") catch return error.WriteFailure;
                    writeCsvField(ctx.writer, str.value) catch return error.WriteFailure;
                    ctx.writer.writeAll("\n") catch return error.WriteFailure;
                },
                .dialogue => |d| {
                    if (d.id_token == null or UUID.isEmpty(d.id)) return error.MissingStamp;
                    const str = d.content.type.string;
                    ctx.writer.print("\"{s}\",\"{s}\",", .{ &d.id, d.speaker orelse "NONE" }) catch return error.WriteFailure;
                    try writeRawFieldWithIndices(ctx.writer, str.raw, ctx.alloc);
                    ctx.writer.writeAll(",") catch return error.WriteFailure;
                    writeCsvField(ctx.writer, str.value) catch return error.WriteFailure;
                    ctx.writer.writeAll("\n") catch return error.WriteFailure;
                },
                else => unreachable,
            }
        }
    };

    const MergeExportContext = struct {
        writer: *std.Io.Writer,
        index: *const CsvIndex,
        alloc: std.mem.Allocator,

        pub const Error = error{ WriteFailure, MissingStamp, OutOfMemory };

        fn onLeaf(ctx: MergeExportContext, stmt: *const Statement) @This().Error!void {
            const has_valid_source_id = switch (stmt.type) {
                .choice => |c| c.id_token != null and !UUID.isEmpty(c.id),
                .dialogue => |d| d.id_token != null and !UUID.isEmpty(d.id),
                else => unreachable,
            };
            if (!has_valid_source_id) return error.MissingStamp;

            const id: UUID.ID = switch (stmt.type) {
                .choice => |c| c.id,
                .dialogue => |d| d.id,
                else => unreachable,
            };

            switch (stmt.type) {
                .choice => |c| {
                    const str = c.content.type.string;
                    ctx.writer.print("\"{s}\",\"CHOICE\",", .{&c.id}) catch return error.WriteFailure;
                    try writeRawFieldWithIndices(ctx.writer, str.raw, ctx.alloc);
                    ctx.writer.writeAll(",") catch return error.WriteFailure;
                    writeCsvField(ctx.writer, str.value) catch return error.WriteFailure;
                },
                .dialogue => |d| {
                    const str = d.content.type.string;
                    ctx.writer.print("\"{s}\",\"{s}\",", .{ &d.id, d.speaker orelse "NONE" }) catch return error.WriteFailure;
                    try writeRawFieldWithIndices(ctx.writer, str.raw, ctx.alloc);
                    ctx.writer.writeAll(",") catch return error.WriteFailure;
                    writeCsvField(ctx.writer, str.value) catch return error.WriteFailure;
                },
                else => unreachable,
            }

            if (ctx.index.rows.get(id)) |extra_values| {
                for (extra_values) |val| {
                    ctx.writer.print(",\"{s}\"", .{val}) catch return error.WriteFailure;
                }
            } else {
                for (0..ctx.index.extra_headers.len) |_| {
                    ctx.writer.writeAll(",\"\"") catch return error.WriteFailure;
                }
            }
            ctx.writer.writeAll("\n") catch return error.WriteFailure;
        }
    };

    pub fn exportFileAtPath(full_path: []const u8, writer: *std.Io.Writer, allocator: std.mem.Allocator, base_lang: []const u8) !void {
        var mod = try Module.init(allocator, default_io, full_path);
        defer mod.deinit();
        try mod.entry.loadSource();
        try mod.entry.buildTree();
        try exportFile(mod.entry, writer, base_lang, allocator);
    }

    pub fn exportFileAtPathWithMerge(full_path: []const u8, writer: *std.Io.Writer, allocator: std.mem.Allocator, base_lang: []const u8, existing_csv: ?[]const u8) !void {
        var mod = try Module.init(allocator, default_io, full_path);
        defer mod.deinit();
        try mod.entry.loadSource();
        try mod.entry.buildTree();
        if (existing_csv) |csv| {
            try exportFileWithMerge(mod.entry, writer, base_lang, csv, allocator);
        } else {
            try exportFile(mod.entry, writer, base_lang, allocator);
        }
    }

    pub fn exportFile(file: *File, writer: *std.Io.Writer, base_lang: []const u8, allocator: std.mem.Allocator) !void {
        const tree = file.tree orelse return error.NotInitialized;
        try writer.print("\"id\",\"speaker\",\"raw\",\"{s}\"\n", .{base_lang});
        try walkLocalizable(tree.root, ExportContext{ .writer = writer, .alloc = allocator }, ExportContext.onLeaf);
    }

    pub fn exportFileWithMerge(file: *File, writer: *std.Io.Writer, base_lang: []const u8, existing_csv: []const u8, allocator: std.mem.Allocator) !void {
        const tree = file.tree orelse return error.NotInitialized;
        var index = try CsvIndex.init(allocator, existing_csv);
        defer index.deinit();

        // Write header: base columns + extra translation columns
        try writer.print("\"id\",\"speaker\",\"raw\",\"{s}\"", .{base_lang});
        for (index.extra_headers) |h| {
            try writer.print(",\"{s}\"", .{h});
        }
        try writer.writeAll("\n");

        try walkLocalizable(tree.root, MergeExportContext{
            .writer = writer,
            .index = &index,
            .alloc = allocator,
        }, MergeExportContext.onLeaf);
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

    const CsvCellIterator = struct {
        line: []const u8,
        pos: usize,

        fn init(line: []const u8) CsvCellIterator {
            return .{ .line = line, .pos = 0 };
        }

        /// Returns the next cell content. For quoted cells, returns content
        /// between outer quotes (with "" escapes preserved). For unquoted
        /// cells, returns raw content. Returns null when exhausted.
        fn next(self: *CsvCellIterator) ?[]const u8 {
            if (self.pos >= self.line.len) return null;

            if (self.line[self.pos] == '"') {
                self.pos += 1; // skip opening quote
                const content_start = self.pos;
                while (self.pos < self.line.len) : (self.pos += 1) {
                    if (self.line[self.pos] == '"') {
                        if (self.pos + 1 < self.line.len and self.line[self.pos + 1] == '"') {
                            self.pos += 1; // skip escaped quote
                        } else {
                            break; // closing quote
                        }
                    }
                }
                const content_end = self.pos;
                if (self.pos < self.line.len) self.pos += 1; // skip closing quote
                if (self.pos < self.line.len and self.line[self.pos] == ',') self.pos += 1;
                return self.line[content_start..content_end];
            } else {
                const start = self.pos;
                while (self.pos < self.line.len and self.line[self.pos] != ',') : (self.pos += 1) {}
                const end = self.pos;
                if (self.pos < self.line.len and self.line[self.pos] == ',') self.pos += 1;
                return self.line[start..end];
            }
        }
    };

    const CsvIndex = struct {
        extra_headers: []const []const u8,
        rows: std.AutoHashMapUnmanaged(UUID.ID, []const []const u8),
        allocator: std.mem.Allocator,

        fn init(allocator: std.mem.Allocator, csv_content: []const u8) !CsvIndex {
            const content = stripBom(csv_content);
            var pos: usize = 0;

            const header_line = nextCsvLine(content, &pos) orelse return error.InvalidCsv;
            var headers: std.ArrayList([]const u8) = .empty;
            defer headers.deinit(allocator);

            var header_iter = CsvCellIterator.init(header_line);
            var col: usize = 0;
            while (header_iter.next()) |cell| : (col += 1) {
                if (col < 4) continue; // skip id, speaker, raw, base_lang
                try headers.append(allocator, cell);
            }

            var rows: std.AutoHashMapUnmanaged(UUID.ID, []const []const u8) = .empty;
            errdefer {
                var it = rows.iterator();
                while (it.next()) |entry| allocator.free(entry.value_ptr.*);
                rows.deinit(allocator);
            }

            while (nextCsvLine(content, &pos)) |line| {
                var cell_iter = CsvCellIterator.init(line);
                var cell_idx: usize = 0;
                var id: ?UUID.ID = null;
                var extra_values: std.ArrayList([]const u8) = .empty;
                defer extra_values.deinit(allocator);

                while (cell_iter.next()) |cell| : (cell_idx += 1) {
                    if (cell_idx == 0) {
                        if (cell.len >= UUID.Size) {
                            id = UUID.fromString(cell[0..UUID.Size]);
                        }
                    } else if (cell_idx >= 4) {
                        try extra_values.append(allocator, cell);
                    }
                }

                if (id) |valid_id| {
                    while (extra_values.items.len < headers.items.len) {
                        try extra_values.append(allocator, "");
                    }
                    try rows.put(allocator, valid_id, try extra_values.toOwnedSlice(allocator));
                }
            }

            return .{
                .extra_headers = try headers.toOwnedSlice(allocator),
                .rows = rows,
                .allocator = allocator,
            };
        }

        fn deinit(self: *CsvIndex) void {
            self.allocator.free(self.extra_headers);
            var it = self.rows.iterator();
            while (it.next()) |entry| self.allocator.free(entry.value_ptr.*);
            self.rows.deinit(self.allocator);
        }
    };

    pub fn generateAtPath(
        allocator: std.mem.Allocator,
        csv_path: []const u8,
        output_folder: []const u8,
        filter_lang: ?[]const u8,
        dry: bool,
    ) !void {
        const file = try std.Io.Dir.cwd().openFile(default_io, csv_path, .{});
        const file_name = std.mem.trimEnd(u8, std.fs.path.basename(csv_path), ".topi.csv");
        defer file.close(default_io);

        var rbuf: [1024]u8 = undefined;
        var rdr = file.reader(default_io, &rbuf);
        const raw_content = try rdr.interface.allocRemaining(allocator, .unlimited);
        defer allocator.free(raw_content);
        const content = stripBom(raw_content);

        var pos: usize = 0;
        const header = nextCsvLine(content, &pos) orelse return error.InvalidCsv;

        var header_iter = CsvCellIterator.init(header);
        var col_idx: usize = 0;
        while (header_iter.next()) |h| : (col_idx += 1) {
            const key = std.mem.trim(u8, h, " ");
            if (std.mem.eql(u8, key, "id") or std.mem.eql(u8, key, "speaker") or std.mem.eql(u8, key, "raw")) continue;
            if (filter_lang) |f| if (!std.mem.eql(u8, key, f)) continue;

            if (!dry) {
                const out_name = try std.fmt.allocPrint(allocator, "{s}/{s}.{s}.topil", .{ output_folder, file_name, key });
                defer allocator.free(out_name);
                const out_file = try std.Io.Dir.cwd().createFile(default_io, out_name, .{});
                defer out_file.close(default_io);
                var file_buf: [1024]u8 = undefined;
                var file_writer = out_file.writer(default_io, &file_buf);
                const writer = &file_writer.interface;
                try generate(allocator, content, col_idx, writer, null);
            }
        }
    }

    pub const GenerateError = error{
        MissingCsvFiles,
        MismatchedLanguageColumns,
    };

    pub const GenerateResult = struct {
        allocator: std.mem.Allocator,
        missing_uuids: std.ArrayList(UUID.ID),
        extra_uuids: std.ArrayList(UUID.ID),
        missing_csv_files: std.ArrayList([]const u8),
        blank_translations: std.ArrayList(UUID.ID),

        pub fn deinit(self: *GenerateResult) void {
            self.missing_uuids.deinit(self.allocator);
            self.extra_uuids.deinit(self.allocator);
            for (self.missing_csv_files.items) |path| self.allocator.free(path);
            self.missing_csv_files.deinit(self.allocator);
            self.blank_translations.deinit(self.allocator);
        }

        pub fn hasWarnings(self: *const GenerateResult) bool {
            return self.missing_uuids.items.len > 0 or
                self.extra_uuids.items.len > 0 or
                self.missing_csv_files.items.len > 0 or
                self.blank_translations.items.len > 0;
        }
    };

    /// Generate .topil files from a .topi module by resolving includes,
    /// finding sibling .csv files, validating UUIDs, and merging all
    /// CSV content into one .topil per language.
    pub fn generateFromModule(
        allocator: std.mem.Allocator,
        topi_path: []const u8,
        output_folder: []const u8,
        filter_lang: ?[]const u8,
        dry: bool,
        error_writer: ?*std.Io.Writer,
    ) !GenerateResult {
        return generateFromModuleWithIo(allocator, default_io, topi_path, output_folder, filter_lang, dry, error_writer);
    }

    pub fn generateFromModuleWithIo(
        allocator: std.mem.Allocator,
        io: std.Io,
        topi_path: []const u8,
        output_folder: []const u8,
        filter_lang: ?[]const u8,
        dry: bool,
        error_writer: ?*std.Io.Writer,
    ) !GenerateResult {
        var result = GenerateResult{
            .allocator = allocator,
            .missing_uuids = .empty,
            .extra_uuids = .empty,
            .missing_csv_files = .empty,
            .blank_translations = .empty,
        };
        errdefer result.deinit();

        // Phase 1: Resolve the module's include graph and parse all ASTs
        var mod = try Module.init(allocator, io, topi_path);
        defer mod.deinit();
        mod.resolveIncludes() catch |err| {
            if (error_writer) |w| mod.writeErrors(w) catch {};
            return err;
        };

        // Build trees for all files
        var it = mod.includes.iterator();
        while (it.next()) |kvp| {
            kvp.value_ptr.*.buildTree() catch |err| {
                if (error_writer) |w| mod.writeErrors(w) catch {};
                return err;
            };
        }

        // Phase 2: Collect all localizable UUIDs from the ASTs, tracking per-file counts
        var expected_uuids = std.AutoHashMap(UUID.ID, void).init(allocator);
        defer expected_uuids.deinit();
        var file_uuid_counts = std.StringHashMap(usize).init(allocator);
        defer file_uuid_counts.deinit();

        var tree_it = mod.includes.iterator();
        while (tree_it.next()) |kvp| {
            const file = kvp.value_ptr.*;
            if (file.tree) |tree| {
                const before = expected_uuids.count();
                try walkLocalizable(tree.root, CollectContext{ .map = &expected_uuids }, CollectContext.onLeaf);
                const added = expected_uuids.count() - before;
                if (added > 0) {
                    try file_uuid_counts.put(kvp.key_ptr.*, added);
                }
            }
        }

        // Phase 3: Find sibling .csv files and merge their content
        var merged_csv = std.ArrayList(u8).empty;
        defer merged_csv.deinit(allocator);
        var csv_uuids = std.AutoHashMap(UUID.ID, void).init(allocator);
        defer csv_uuids.deinit();
        var header_written = false;
        var expected_header: ?[]const u8 = null;
        defer if (expected_header) |eh| allocator.free(eh);

        var csv_it = mod.includes.iterator();
        while (csv_it.next()) |kvp| {
            const file = kvp.value_ptr.*;
            const csv_name = try std.fmt.allocPrint(allocator, "{s}.csv", .{file.path});
            defer allocator.free(csv_name);

            const csv_file = mod.dir.openFile(mod.io, csv_name, .{}) catch {
                // Only warn if this file has localizable content
                if (file_uuid_counts.get(kvp.key_ptr.*) == null) continue;
                try result.missing_csv_files.append(allocator, try allocator.dupe(u8, kvp.key_ptr.*));
                continue;
            };
            defer csv_file.close(mod.io);

            var csv_buf: [1024]u8 = undefined;
            var csv_rdr = csv_file.reader(mod.io, &csv_buf);
            const raw_content = try csv_rdr.interface.allocRemaining(allocator, .unlimited);
            defer allocator.free(raw_content);
            const content = stripBom(raw_content);

            var pos: usize = 0;
            const header = nextCsvLine(content, &pos) orelse continue;

            if (!header_written) {
                try merged_csv.appendSlice(allocator, header);
                try merged_csv.append(allocator, '\n');
                expected_header = try allocator.dupe(u8, header);
                header_written = true;
            } else if (expected_header) |eh| {
                if (!std.mem.eql(u8, header, eh)) {
                    if (error_writer) |w| w.print("Error: CSV header mismatch in {s}\n", .{csv_name}) catch {};
                    return error.MismatchedLanguageColumns;
                }
            }

            // Append all data rows and collect CSV UUIDs
            while (nextCsvLine(content, &pos)) |line| {
                var cell_iter = CsvCellIterator.init(line);
                if (cell_iter.next()) |id_cell| {
                    if (id_cell.len >= UUID.Size) {
                        const id = UUID.fromString(id_cell[0..UUID.Size]);
                        if (!UUID.isEmpty(id)) {
                            try csv_uuids.put(id, {});
                        }
                    }
                }
                try merged_csv.appendSlice(allocator, line);
                try merged_csv.append(allocator, '\n');
            }
        }

        // Phase 4: Validate UUIDs — compare expected (AST) vs provided (CSV)
        var exp_it = expected_uuids.iterator();
        while (exp_it.next()) |entry| {
            if (!csv_uuids.contains(entry.key_ptr.*)) {
                try result.missing_uuids.append(allocator, entry.key_ptr.*);
            }
        }

        var csv_uuid_it = csv_uuids.iterator();
        while (csv_uuid_it.next()) |entry| {
            if (!expected_uuids.contains(entry.key_ptr.*)) {
                try result.extra_uuids.append(allocator, entry.key_ptr.*);
            }
        }

        // Phase 5: Generate .topil files from merged CSV
        if (!header_written) return result; // No CSVs found at all

        const merged_content = merged_csv.items;
        const entry_name = std.mem.trimEnd(u8, std.fs.path.basename(topi_path), ".topi");

        var mpos: usize = 0;
        const mheader = nextCsvLine(merged_content, &mpos) orelse return result;

        var m_header_iter = CsvCellIterator.init(mheader);
        var col_idx: usize = 0;
        while (m_header_iter.next()) |h| : (col_idx += 1) {
            const key = std.mem.trim(u8, h, " ");
            if (std.mem.eql(u8, key, "id") or std.mem.eql(u8, key, "speaker") or std.mem.eql(u8, key, "raw")) continue;
            if (filter_lang) |f| if (!std.mem.eql(u8, key, f)) continue;

            if (!dry) {
                const out_name = try std.fmt.allocPrint(allocator, "{s}/{s}.{s}.topil", .{ output_folder, entry_name, key });
                defer allocator.free(out_name);
                const out_file = try std.Io.Dir.cwd().createFile(io, out_name, .{});
                defer out_file.close(io);
                var file_buf: [1024]u8 = undefined;
                var file_writer = out_file.writer(io, &file_buf);
                const writer = &file_writer.interface;
                try generate(allocator, merged_content, col_idx, writer, &result.blank_translations);
            }
        }

        return result;
    }

    /// Count interpolations in the source-side `raw` column. Skips
    /// backslash-escaped `\{` (the lexer's escape for a literal brace),
    /// so this matches what the lowering pass would have counted.
    fn countRawInterps(raw_cell: []const u8) u32 {
        var n: u32 = 0;
        var i: usize = 0;
        while (i < raw_cell.len) : (i += 1) {
            const c = raw_cell[i];
            if (c == '\\') {
                i += 1; // skip escaped char
                continue;
            }
            if (c == '{') n += 1;
        }
        return n;
    }

    /// Parse the integer index out of a `{...}` body in a translator
    /// column. Accepts either bare `{N}` or echoed `{name | N}` form
    /// (silent strip — translators copying source format shouldn't
    /// have to clean it up). Returns the parsed index or an error.
    fn parseTranslatorIndex(body: []const u8) !u8 {
        const trimmed = std.mem.trim(u8, body, " \t");
        const pipe = std.mem.lastIndexOfScalar(u8, trimmed, '|');
        const num_text = if (pipe) |p| std.mem.trim(u8, trimmed[p + 1 ..], " \t") else trimmed;
        return std.fmt.parseInt(u8, num_text, 10);
    }

    /// Reads CSV rows, builds per-entry strings + segments, then writes
    /// a .topil v2 file. `lang_col` selects which translator column to
    /// emit; the `raw` column (column 2) is consulted to know how many
    /// interpolations are valid for translator indices.
    pub fn generate(allocator: std.mem.Allocator, raw_content: []const u8, lang_col: usize, writer: *std.Io.Writer, blank_translations: ?*std.ArrayList(UUID.ID)) !void {
        const content = stripBom(raw_content);
        var pos: usize = 0;
        _ = nextCsvLine(content, &pos); // skip header

        var entries: std.ArrayList(LocaleProvider.IndexEntry) = .empty;
        defer entries.deinit(allocator);
        var strings: std.ArrayList(u8) = .empty;
        defer strings.deinit(allocator);
        var seg_blob: std.ArrayList(u8) = .empty;
        defer seg_blob.deinit(allocator);
        var seg_buf: std.ArrayList(Segment) = .empty;
        defer seg_buf.deinit(allocator);

        const raw_col: usize = 2; // id=0, speaker=1, raw=2, lang_cols=3..

        while (nextCsvLine(content, &pos)) |line| {
            var cell_iter = CsvCellIterator.init(line);
            var current_cell: usize = 0;
            var id_str: ?[]const u8 = null;
            var raw_val: []const u8 = "";
            var target_val: ?[]const u8 = null;

            while (cell_iter.next()) |cell| : (current_cell += 1) {
                if (current_cell == 0) {
                    if (cell.len >= UUID.Size) {
                        id_str = cell[0..UUID.Size];
                    }
                } else if (current_cell == raw_col) {
                    raw_val = cell;
                } else if (current_cell == lang_col) {
                    target_val = cell;
                }
                if (id_str != null and target_val != null and current_cell >= lang_col) break;
            }

            if (id_str == null or target_val == null) continue;

            if (target_val.?.len == 0) {
                if (blank_translations) |bt| {
                    try bt.append(allocator, UUID.fromString(id_str.?));
                }
            }

            const interp_count = countRawInterps(raw_val);
            seg_buf.clearRetainingCapacity();

            const str_offset: u32 = @intCast(strings.items.len);
            const tv = target_val.?;

            // Walk the translator cell. For each `{...}` span: parse the
            // index (silently stripping any `name |` echo), validate
            // against `interp_count`, write `{N}` bytes into the string
            // blob (cat-readability), record an interp segment, and
            // bracket with literal segments for the surrounding text.
            // Backslash escape `\{` lets writers embed literal braces.
            var lit_start: u32 = @intCast(strings.items.len);
            var j: usize = 0;
            while (j < tv.len) : (j += 1) {
                const c = tv[j];
                if (c == '"' and j + 1 < tv.len and tv[j + 1] == '"') {
                    try strings.append(allocator, '"');
                    j += 1;
                    continue;
                }
                if (c == '\\' and j + 1 < tv.len) {
                    // Preserve the escape byte and the escaped char
                    try strings.append(allocator, c);
                    try strings.append(allocator, tv[j + 1]);
                    j += 1;
                    continue;
                }
                if (c == '{') {
                    // Close the literal segment up to the marker, if any.
                    const lit_end: u32 = @intCast(strings.items.len);
                    if (lit_end > lit_start) {
                        try seg_buf.append(allocator, .{ .literal = .{
                            .start = std.math.cast(u16, lit_start - str_offset) orelse return error.LocaleStringTooLong,
                            .end = std.math.cast(u16, lit_end - str_offset) orelse return error.LocaleStringTooLong,
                        } });
                    }

                    // Find the matching `}`.
                    const body_start = j + 1;
                    var k = body_start;
                    while (k < tv.len and tv[k] != '}') : (k += 1) {}
                    if (k >= tv.len) return error.UnterminatedInterpolation;
                    const body = tv[body_start..k];
                    const arg_idx = parseTranslatorIndex(body) catch return error.InvalidTranslatorIndex;
                    if (interp_count == 0 or arg_idx >= interp_count) return error.TranslatorIndexOutOfRange;

                    // Write `{N}` into the string blob so the file stays
                    // cat-readable. The literal segment we just emitted
                    // ended *before* the `{`, so the marker bytes are
                    // skipped by the walker.
                    try strings.append(allocator, '{');
                    var num_buf: [4]u8 = undefined;
                    const n_text = std.fmt.bufPrint(&num_buf, "{d}", .{arg_idx}) catch unreachable;
                    try strings.appendSlice(allocator, n_text);
                    try strings.append(allocator, '}');

                    try seg_buf.append(allocator, .{ .interp = arg_idx });

                    j = k; // loop increment will move past the `}`
                    lit_start = @intCast(strings.items.len);
                    continue;
                }
                try strings.append(allocator, c);
            }
            // Trailing literal.
            const tail_end: u32 = @intCast(strings.items.len);
            if (tail_end > lit_start) {
                try seg_buf.append(allocator, .{ .literal = .{
                    .start = std.math.cast(u16, lit_start - str_offset) orelse return error.LocaleStringTooLong,
                    .end = std.math.cast(u16, tail_end - str_offset) orelse return error.LocaleStringTooLong,
                } });
            }

            const seg_offset: u32 = @intCast(seg_blob.items.len);
            for (seg_buf.items) |seg| try appendSegmentBytes(allocator, &seg_blob, seg);

            try entries.append(allocator, .{
                .id = UUID.fromString(id_str.?),
                .str_offset = str_offset,
                .str_length = @intCast(strings.items.len - str_offset),
                .seg_offset = seg_offset,
                .seg_count = std.math.cast(u8, seg_buf.items.len) orelse return error.LocaleSegmentOverflow,
            });
        }

        try writer.writeAll(LocaleProvider.magic);
        try writer.writeInt(u16, LocaleProvider.version, .little);
        try writer.writeInt(C.CONSTANT, @intCast(entries.items.len), .little);
        try writer.writeInt(u32, @intCast(strings.items.len), .little);
        for (entries.items) |entry| {
            try writer.writeAll(&entry.id);
            try writer.writeInt(C.CONSTANT, entry.str_offset, .little);
            try writer.writeInt(u32, entry.str_length, .little);
            try writer.writeInt(u32, entry.seg_offset, .little);
            try writer.writeByte(entry.seg_count);
        }
        try writer.writeAll(strings.items);
        try writer.writeAll(seg_blob.items);
        try writer.flush();
    }
};
