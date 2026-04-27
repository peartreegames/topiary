//! Debug printer for IR programs. Format is not stable — for human
//! reading and tests only.

const std = @import("std");
const ir = @import("program.zig");

pub fn dump(program: *const ir.Program, writer: *std.Io.Writer) !void {
    try writer.print("=== IR ===\n", .{});
    try writer.print("globals: {d}\n", .{program.globals_count});
    try writer.print("anchors: {d}\n", .{program.anchors.count()});
    try writer.print("files: {d}\n", .{program.files.len});
    try writer.print("---\n", .{});
    for (program.body) |s| try dumpStmt(&s, writer, 0);
    try writer.print("===     ===\n", .{});
}

fn pad(writer: *std.Io.Writer, depth: usize) !void {
    var i: usize = 0;
    while (i < depth) : (i += 1) try writer.print("  ", .{});
}

fn dumpStmt(stmt: *const ir.Stmt, writer: *std.Io.Writer, depth: usize) !void {
    try pad(writer, depth);
    try writer.print("{s}", .{@tagName(stmt.kind)});
    switch (stmt.kind) {
        .line => |l| {
            try writer.print(" speaker={s} segs={d} tags={d}\n", .{
                l.speaker orelse "(none)",
                l.segments.len,
                l.tags.len,
            });
        },
        .choice => |c| {
            try writer.print(" name={s} unique={} segs={d}\n", .{
                c.name orelse "(anon)",
                c.is_unique,
                c.segments.len,
            });
            for (c.body) |s| try dumpStmt(&s, writer, depth + 1);
        },
        .fork, .backup_fork => |f| {
            try writer.print(" name={s} path={s}\n", .{
                f.name orelse "(anon)",
                f.anchor.path,
            });
            for (f.body) |s| try dumpStmt(&s, writer, depth + 1);
        },
        .divert, .backup_divert => |d| {
            try writer.print(" => {s}\n", .{d.target.path});
        },
        .bough => |b| {
            try writer.print(" {s}\n", .{b.anchor.path});
            for (b.body) |s| try dumpStmt(&s, writer, depth + 1);
        },
        .visit => |v| try writer.print(" {s}\n", .{v.target.path}),
        .block => |b| {
            try writer.print("\n", .{});
            for (b.body) |s| try dumpStmt(&s, writer, depth + 1);
        },
        .@"if" => |i| {
            try writer.print("\n", .{});
            for (i.then_branch) |s| try dumpStmt(&s, writer, depth + 1);
            if (i.else_branch) |eb| for (eb) |s| try dumpStmt(&s, writer, depth + 1);
        },
        .@"while" => |w| {
            try writer.print("\n", .{});
            for (w.body) |s| try dumpStmt(&s, writer, depth + 1);
        },
        .@"for" => |f| {
            try writer.print(" capture={s}\n", .{f.capture_name});
            for (f.body) |s| try dumpStmt(&s, writer, depth + 1);
        },
        .@"switch" => |sw| {
            try writer.print(" prongs={d}\n", .{sw.prongs.len});
            for (sw.prongs) |p| {
                try pad(writer, depth + 1);
                try writer.print("PRONG (default={})\n", .{p.values == null});
                for (p.body) |s| try dumpStmt(&s, writer, depth + 2);
            }
        },
        .function => |f| {
            try writer.print(" {s}({d}) extern={}\n", .{ f.name, f.parameters.len, f.is_extern });
            for (f.body) |s| try dumpStmt(&s, writer, depth + 1);
        },
        .class => |c| {
            try writer.print(" {s} fields={d} methods={d}\n", .{
                c.anchor.path,
                c.field_names.len,
                c.methods.len,
            });
        },
        .enum_decl => |e| {
            try writer.print(" {s} seq={} values={d}\n", .{
                e.anchor.path,
                e.is_seq,
                e.values.len,
            });
        },
        .var_decl => |v| {
            try writer.print(" {s} mut={}\n", .{ v.name, v.is_mutable });
        },
        .include => |inc| try writer.print(" {s}\n", .{inc.resolved_path}),
        .return_value, .expr_stmt => try writer.print("\n", .{}),
        .return_void, .@"break", .@"continue", .fin => try writer.print("\n", .{}),
    }
}

test "construct empty program and dump fin" {
    var program = ir.Program.init(std.testing.allocator);
    defer program.deinit();

    const arena = program.allocator();
    const body = try arena.alloc(ir.Stmt, 1);
    body[0] = .{
        .loc = .{ .file_index = 0, .start = undefined },
        .kind = .fin,
    };
    program.body = body;

    var buf: [256]u8 = undefined;
    var fbw = std.Io.Writer.fixed(&buf);
    try dump(&program, &fbw);

    const out = fbw.buffered();
    try std.testing.expect(std.mem.indexOf(u8, out, "fin") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "globals: 0") != null);
}
