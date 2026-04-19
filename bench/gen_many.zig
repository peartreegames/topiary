// Many-modules fixture generator.
//
// Emits N independent self-contained .topi files modelled on the
// chapter template in gen.zig, but without cross-file includes.
// Each file is its own top-level entry that could compile into its
// own .topib — matching the "multiple conversation artifacts" shipping
// model described in CLAUDE.md.
//
// Usage: bench-gen-many <output-dir>

const std = @import("std");

const module_count: usize = 100;
const boughs_per_module: usize = 6;

const io = std.Io.Threaded.global_single_threaded.io();

pub fn main(init: std.process.Init.Minimal) !void {
    var args_iter = std.process.Args.Iterator.init(init.args);
    _ = args_iter.skip();
    const out_dir_path = args_iter.next() orelse {
        std.debug.print("usage: bench-gen-many <output-dir>\n", .{});
        return error.MissingArgument;
    };

    try std.Io.Dir.cwd().createDirPath(io, out_dir_path);
    var dir = try std.Io.Dir.cwd().openDir(io, out_dir_path, .{});
    defer dir.close(io);

    var i: usize = 1;
    while (i <= module_count) : (i += 1) {
        try writeModule(dir, i);
    }

    std.debug.print(
        "bench-gen-many: wrote {d} independent .topi files into {s}\n",
        .{ module_count, out_dir_path },
    );
}

fn writeModule(dir: std.Io.Dir, n: usize) !void {
    var name_buf: [32]u8 = undefined;
    const name = try std.fmt.bufPrint(&name_buf, "mod_{d:0>3}.topi", .{n});

    var file = try dir.createFile(io, name, .{ .truncate = true });
    defer file.close(io);
    var buf: [4096]u8 = undefined;
    var fw = file.writer(io, &buf);
    const w = &fw.interface;

    try w.print("// Module {d:0>3} -- self-contained, compiles to its own .topib.\n\n", .{n});

    // Constants
    try w.writeAll("const TITLE = \"A short conversation.\"\n");
    try w.writeAll("const AUTHOR = \"Anonymous\"\n\n");

    // Enums
    try w.writeAll("enum Mood { Calm, Tense, Climax, Resolution }\n");
    try w.writeAll("enumseq Phase { Intro, Rising, Falling, End }\n\n");

    // Class
    try w.writeAll("class Agent {\n");
    try w.writeAll("    name = \"Unknown\",\n");
    try w.writeAll("    trust = 50,\n");
    try w.writeAll("    questioned = false,\n");
    try w.writeAll("    notes = List{},\n");
    try w.writeAll("\n");
    try w.writeAll("    fn adjust |amt| {\n");
    try w.writeAll("        self.trust += amt\n");
    try w.writeAll("        if self.trust > 100 self.trust = 100\n");
    try w.writeAll("        if self.trust < 0 self.trust = 0\n");
    try w.writeAll("    }\n");
    try w.writeAll("\n");
    try w.writeAll("    fn note |s| {\n");
    try w.writeAll("        self.notes.add(s)\n");
    try w.writeAll("        self.questioned = true\n");
    try w.writeAll("    }\n");
    try w.writeAll("}\n\n");

    // Vars
    try w.writeAll("var state = Mood.Calm\n");
    try w.writeAll("var phase = Phase.Intro\n");
    try w.writeAll("var clues = 0\n");
    try w.writeAll("var score = 0\n");
    try w.writeAll("var inventory = Set{}\n");
    try w.writeAll("var evidence = Map{}\n");
    try w.print("var agent = new Agent {{ name = \"Agent {d:0>3}\" }}\n", .{n});
    try w.writeAll("var lines = List{\"The wind picks up.\", \"A shadow moves.\", \"Footsteps above.\", \"A floorboard groans.\"}\n");
    try w.writeAll("var replies = List{\"Nothing to say.\", \"You wouldnt understand.\", \"Ask someone else.\"}\n\n");

    // Functions
    try w.writeAll("fn describe || {\n");
    try w.writeAll("    switch state {\n");
    try w.writeAll("        Mood.Calm: return \"All quiet for now.\",\n");
    try w.writeAll("        Mood.Tense: return \"The air feels heavier.\",\n");
    try w.writeAll("        Mood.Climax: return \"This is the moment.\",\n");
    try w.writeAll("        Mood.Resolution: return \"Things settle.\"\n");
    try w.writeAll("    }\n");
    try w.writeAll("    return \"\"\n");
    try w.writeAll("}\n\n");

    try w.writeAll("fn summary || {\n");
    try w.writeAll("    var total = 0\n");
    try w.writeAll("    for evidence |e| {\n");
    try w.writeAll("        total += e.value\n");
    try w.writeAll("    }\n");
    try w.writeAll("    score = clues * 10 + total\n");
    try w.writeAll("    return score\n");
    try w.writeAll("}\n\n");

    // Boughs
    const bough_names = [_][]const u8{ "START", "INVESTIGATE", "TALK", "DISCOVER", "DECIDE", "END" };
    const moods = [_][]const u8{
        "introduction",
        "investigation",
        "conversation",
        "discovery",
        "decision",
        "conclusion",
    };
    comptime std.debug.assert(bough_names.len == boughs_per_module);
    comptime std.debug.assert(moods.len == boughs_per_module);

    var b: usize = 0;
    while (b < boughs_per_module) : (b += 1) {
        try writeBough(w, bough_names[b], moods[b], bough_names);
    }

    try w.flush();
}

fn writeBough(
    w: *std.Io.Writer,
    name: []const u8,
    mood: []const u8,
    bough_names: [boughs_per_module][]const u8,
) !void {
    try w.print("=== {s} {{\n", .{name});
    try w.print("    :: \"Beginning {s}.\"\n", .{mood});
    try w.writeAll("    :: \"{describe()}\"\n");
    try w.writeAll("    cycle(lines)\n");

    try w.writeAll("    if clues > 3 {\n");
    try w.writeAll("        :: \"You have gathered clues.\"\n");
    try w.writeAll("        state = Mood.Tense\n");
    try w.writeAll("    }\n");

    try w.writeAll("    :Agent: \"Tell me what you saw.\"\n");
    try w.writeAll("    random(replies)\n");

    const target_a = pickTarget(bough_names, name, 1);
    const target_b = pickTarget(bough_names, name, 2);

    try w.writeAll("    fork^ {\n");

    try w.writeAll("        ~ \"Tell the truth\" {\n");
    try w.writeAll("            agent.adjust(10)\n");
    try w.writeAll("            clues += 1\n");
    try w.writeAll("            inventory.add(\"truth\")\n");
    try w.writeAll("            evidence.add(\"truth\", 2)\n");
    try w.writeAll("            :: \"The agent listens.\"\n");
    try w.print("            => {s}\n", .{target_a});
    try w.writeAll("        }\n");

    try w.writeAll("        ~ \"Stay silent\" {\n");
    try w.writeAll("            agent.note(\"silent\")\n");
    try w.writeAll("            :: \"The silence stretches.\"\n");
    try w.print("            => {s}\n", .{target_b});
    try w.writeAll("        }\n");

    try w.writeAll("    }\n");

    try w.writeAll("    summary()\n");
    try w.writeAll("}\n\n");
}

fn pickTarget(
    bough_names: [boughs_per_module][]const u8,
    current: []const u8,
    offset: usize,
) []const u8 {
    var idx: usize = 0;
    for (bough_names, 0..) |b, i| {
        if (std.mem.eql(u8, b, current)) {
            idx = i;
            break;
        }
    }
    const next = (idx + offset) % bough_names.len;
    return bough_names[next];
}
