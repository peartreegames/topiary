const std = @import("std");
const frontend = @import("frontend/index.zig");
const Statement = frontend.Statement;
const Expression = frontend.Expression;
const ast = @import("frontend/ast.zig");

const utils = @import("utils/index.zig");
const UUID = utils.UUID;

const module = @import("module.zig");
const Module = module.Module;

pub const Stamp = struct {
    pub fn stampFileAtPath(full_path: []const u8, allocator: std.mem.Allocator) ![]const u8 {
        var mod = try Module.init(allocator, std.Io.Threaded.global_single_threaded.io(), full_path);
        defer mod.deinit();
        try mod.entry.loadSource();
        try mod.entry.buildTree();
        return stampFile(mod.entry, allocator);
    }

    pub fn stampFile(file: *module.File, alloc: std.mem.Allocator) ![]const u8 {
        const source = file.source orelse return error.FileSourceNotLoaded;
        const tree = file.tree orelse return error.FileTreeNotLoaded;
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(alloc);
        var last_pos: usize = 0;

        try walkStampable(tree.root, StampContext{
            .alloc = alloc,
            .source = source,
            .buf = &buf,
            .last_pos = &last_pos,
        }, StampContext.onLeaf);

        try buf.appendSlice(alloc, source[last_pos..]);
        return buf.toOwnedSlice(alloc);
    }

    fn walkStampable(stmts: []const Statement, context: anytype, comptime onLeaf: fn (@TypeOf(context), Statement) @TypeOf(context).Error!void) @TypeOf(context).Error!void {
        for (stmts) |stmt| {
            switch (stmt.type) {
                .block => |b| try walkStampable(b, context, onLeaf),
                .bough => |b| {
                    try onLeaf(context, stmt);
                    try walkStampable(b.body, context, onLeaf);
                },
                .choice => |c| {
                    try onLeaf(context, stmt);
                    try walkStampable(c.body, context, onLeaf);
                },
                .dialogue => try onLeaf(context, stmt),
                .@"for" => |f| try walkStampable(f.body, context, onLeaf),
                .fork => |f| {
                    try onLeaf(context, stmt);
                    try walkStampable(f.body, context, onLeaf);
                },
                .@"if" => |i| {
                    try walkStampable(i.then_branch, context, onLeaf);
                    if (i.else_branch) |e| try walkStampable(e, context, onLeaf);
                },
                .@"while" => |w| try walkStampable(w.body, context, onLeaf),
                .@"switch" => |s| try walkStampable(s.prongs, context, onLeaf),
                .switch_prong => |sp| try walkStampable(sp.body, context, onLeaf),
                else => {},
            }
        }
    }

    const StampContext = struct {
        alloc: std.mem.Allocator,
        source: []const u8,
        buf: *std.ArrayList(u8),
        last_pos: *usize,

        pub const Error = error{ OutOfMemory, NoSpaceLeft };

        fn onLeaf(ctx: StampContext, stmt: Statement) @This().Error!void {
            switch (stmt.type) {
                .bough => |b| {
                    if (b.id_token != null and !UUID.isEmpty(b.id)) return;
                    try ctx.copyUpTo(if (b.id_token) |idt| idt.start else b.name_token.end);
                    try ctx.writeId();
                    if (b.id_token) |idt| ctx.last_pos.* = idt.end;
                },
                .fork => |f| {
                    if (f.id_token != null and !UUID.isEmpty(f.id)) return;
                    try ctx.copyUpTo(if (f.id_token) |idt| idt.start else f.end_token.end);
                    try ctx.writeId();
                    if (f.id_token) |idt| ctx.last_pos.* = idt.end;
                },
                .dialogue => |d| {
                    if (d.id_token != null and !UUID.isEmpty(d.id)) return;
                    try ctx.copyUpTo(if (d.id_token) |idt| idt.start else insertAfterContent(d.content, d.tags));
                    try ctx.writeId();
                    if (d.id_token) |idt| ctx.last_pos.* = idt.end;
                },
                .choice => |c| {
                    if (c.id_token != null and !UUID.isEmpty(c.id)) return;
                    try ctx.copyUpTo(if (c.id_token) |idt| idt.start else insertAfterContent(&c.content, c.tags));
                    try ctx.writeId();
                    if (c.id_token) |idt| ctx.last_pos.* = idt.end;
                },
                else => {},
            }
        }

        fn copyUpTo(ctx: StampContext, pos: usize) Error!void {
            try ctx.buf.appendSlice(ctx.alloc, ctx.source[ctx.last_pos.*..pos]);
            ctx.last_pos.* = pos;
        }

        fn insertAfterContent(content: *const Expression, tags: []const ast.Tag) usize {
            if (tags.len > 0) return tags[tags.len - 1].token.end;
            // For both simple and interpolated strings, token.start is after the
            // opening quote, and raw.len covers through to the closing quote.
            // So token.start + raw.len = position of closing quote,
            // and + 1 = position after closing quote.
            return content.token.start + content.type.string.raw.len + 1;
        }

        fn writeId(ctx: StampContext) Error!void {
            const new_id = UUID.new();
            var tmp: [UUID.Size + 2]u8 = undefined;
            _ = std.fmt.bufPrint(&tmp, " @{s}", .{new_id}) catch unreachable;
            try ctx.buf.appendSlice(ctx.alloc, &tmp);
        }
    };
};
