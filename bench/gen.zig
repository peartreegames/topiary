// Benchmark fixture generator.
//
// Emits 1 entry file plus 20 chapter files modelled (loosely) on
// examples/story.topi. Each chapter is ~400 lines and uses unique
// symbol prefixes (CHAPTER_NN_*, cNN_*, CNN, ENN) so all 20 can be
// included into the same module without collisions.
//
// Usage: bench-gen <output-dir>

const std = @import("std");

const chapter_count: usize = 200;
const boughs_per_chapter: usize = 10;

const io = std.Io.Threaded.global_single_threaded.io();

pub fn main(init: std.process.Init.Minimal) !void {
    var args_iter = std.process.Args.Iterator.init(init.args);
    _ = args_iter.skip();
    const out_dir_path = args_iter.next() orelse {
        std.debug.print("usage: bench-gen <output-dir>\n", .{});
        return error.MissingArgument;
    };

    try std.Io.Dir.cwd().createDirPath(io, out_dir_path);
    var dir = try std.Io.Dir.cwd().openDir(io, out_dir_path, .{});
    defer dir.close(io);

    try writeEntry(dir);
    var i: usize = 1;
    while (i <= chapter_count) : (i += 1) {
        try writeChapter(dir, i);
    }

    std.debug.print("bench-gen: wrote entry.topi + {d} chapter files into {s}\n", .{ chapter_count, out_dir_path });
}

fn writeEntry(dir: std.Io.Dir) !void {
    var file = try dir.createFile(io, "entry.topi", .{ .truncate = true });
    defer file.close(io);
    var buf: [4096]u8 = undefined;
    var fw = file.writer(io, &buf);
    const w = &fw.interface;

    try w.writeAll("// Benchmark entry file -- includes all chapters and diverts into chapter 1.\n\n");

    var i: usize = 1;
    while (i <= chapter_count) : (i += 1) {
        try w.print("include \"chapter_{d:0>2}.topi\"\n", .{i});
    }
    try w.writeAll("\n");

    try w.writeAll("var bench_total_score = 0\n\n");

    try w.writeAll("=== START {\n");
    try w.writeAll("    :: \"Begin benchmark.\"\n");
    try w.writeAll("    => CHAPTER_01_START\n");
    try w.writeAll("}\n");

    try w.flush();
}

fn writeChapter(dir: std.Io.Dir, n: usize) !void {
    var name_buf: [32]u8 = undefined;
    const name = try std.fmt.bufPrint(&name_buf, "chapter_{d:0>2}.topi", .{n});

    var file = try dir.createFile(io, name, .{ .truncate = true });
    defer file.close(io);
    var buf: [4096]u8 = undefined;
    var fw = file.writer(io, &buf);
    const w = &fw.interface;

    // --- Header & constants ---
    try w.print("// Chapter {d:0>2}\n\n", .{n});
    try w.print("const CHAPTER_{d:0>2}_TITLE = \"Chapter {d:0>2}: The Investigation\"\n", .{ n, n });
    try w.print("const CHAPTER_{d:0>2}_AUTHOR = \"Anonymous\"\n", .{n});
    try w.print("const CHAPTER_{d:0>2}_LOCATION = \"Wardens Rock\"\n\n", .{n});

    // --- Enums ---
    try w.print("enum E{d:0>2} {{ Calm, Tense, Climax, Resolution }}\n", .{n});
    try w.print("enumseq Phase{d:0>2} {{ Intro, Rising, Falling, End }}\n\n", .{n});

    // --- Class ---
    try w.print("class C{d:0>2} {{\n", .{n});
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

    // --- Vars ---
    try w.print("var c{d:0>2}_state = E{d:0>2}.Calm\n", .{ n, n });
    try w.print("var c{d:0>2}_phase = Phase{d:0>2}.Intro\n", .{ n, n });
    try w.print("var c{d:0>2}_clues = 0\n", .{n});
    try w.print("var c{d:0>2}_score = 0\n", .{n});
    try w.print("var c{d:0>2}_health = 3\n", .{n});
    try w.print("var c{d:0>2}_inventory = Set{{}}\n", .{n});
    try w.print("var c{d:0>2}_evidence = Map{{}}\n", .{n});
    try w.print("var c{d:0>2}_agent = new C{d:0>2} {{ name = \"Agent {d:0>2}\" }}\n", .{ n, n, n });
    try w.print(
        "var c{d:0>2}_lines = List{{\"The wind picks up.\", \"A shadow moves at the edge of sight.\", \"Footsteps somewhere overhead.\", \"The smell of brine and old paper.\", \"A floorboard groans.\"}}\n",
        .{n},
    );
    try w.print(
        "var c{d:0>2}_replies = List{{\"I have nothing to say.\", \"You wouldnt understand.\", \"Ask someone else.\", \"Leave me alone.\"}}\n\n",
        .{n},
    );

    // --- Functions ---
    try w.print("fn c{d:0>2}_total || {{\n", .{n});
    try w.writeAll("    var total = 0\n");
    try w.print("    for c{d:0>2}_evidence |e| {{\n", .{n});
    try w.writeAll("        total += e.value\n");
    try w.writeAll("    }\n");
    try w.writeAll("    return total\n");
    try w.writeAll("}\n\n");

    try w.print("fn c{d:0>2}_describe || {{\n", .{n});
    try w.print("    switch c{d:0>2}_state {{\n", .{n});
    try w.print("        E{d:0>2}.Calm: return \"All quiet for now.\",\n", .{n});
    try w.print("        E{d:0>2}.Tense: return \"The air feels heavier.\",\n", .{n});
    try w.print("        E{d:0>2}.Climax: return \"This is the moment.\",\n", .{n});
    try w.print("        E{d:0>2}.Resolution: return \"Things settle, for a while.\"\n", .{n});
    try w.writeAll("    }\n");
    try w.writeAll("    return \"\"\n");
    try w.writeAll("}\n\n");

    try w.print("fn c{d:0>2}_summary || {{\n", .{n});
    try w.print("    var total = c{d:0>2}_total()\n", .{n});
    try w.print("    c{d:0>2}_score = c{d:0>2}_clues * 10 + total\n", .{ n, n });
    try w.print("    return c{d:0>2}_score\n", .{n});
    try w.writeAll("}\n\n");

    // --- Boughs ---
    const moods = [_][]const u8{
        "introduction",
        "investigation",
        "conversation",
        "anticipation",
        "discovery",
        "confrontation",
        "revelation",
        "decision",
        "resolution",
        "conclusion",
    };
    const bough_names = [_][]const u8{
        "START",
        "INVESTIGATE",
        "TALK",
        "WAIT",
        "DISCOVER",
        "CONFRONT",
        "REVEAL",
        "DECIDE",
        "RESOLVE",
        "END",
    };
    comptime std.debug.assert(moods.len == boughs_per_chapter);
    comptime std.debug.assert(bough_names.len == boughs_per_chapter);

    var b: usize = 0;
    while (b < boughs_per_chapter) : (b += 1) {
        try writeBough(w, n, bough_names[b], moods[b], bough_names);
    }

    try w.flush();
}

fn writeBough(
    w: *std.Io.Writer,
    n: usize,
    name: []const u8,
    mood: []const u8,
    bough_names: [boughs_per_chapter][]const u8,
) !void {
    try w.print("=== CHAPTER_{d:0>2}_{s} {{\n", .{ n, name });
    try w.print("    :: \"Beginning {s} for chapter {d:0>2}.\"\n", .{ mood, n });
    try w.print("    :: \"{{c{d:0>2}_describe()}}\"\n", .{n});
    try w.print("    cycle(c{d:0>2}_lines)\n", .{n});

    try w.print("    if c{d:0>2}_clues > 5 {{\n", .{n});
    try w.writeAll("        :: \"You have gathered many clues so far.\"\n");
    try w.print("        c{d:0>2}_state = E{d:0>2}.Tense\n", .{ n, n });
    try w.writeAll("    }\n");

    try w.print("    if c{d:0>2}_clues > 10 {{\n", .{n});
    try w.writeAll("        :: \"The picture is starting to come together.\"\n");
    try w.print("        c{d:0>2}_state = E{d:0>2}.Climax\n", .{ n, n });
    try w.writeAll("    }\n");

    try w.print("    :C{d:0>2}_Agent: \"Tell me what you saw.\"\n", .{n});
    try w.print("    random(c{d:0>2}_replies)\n", .{n});

    // Pick three divert targets that exist (avoid self-divert noise).
    const target_a = pickTarget(bough_names, name, 1);
    const target_b = pickTarget(bough_names, name, 2);
    const target_c = pickTarget(bough_names, name, 3);

    try w.writeAll("    fork^ {\n");

    try w.writeAll("        ~ \"Tell the truth\" {\n");
    try w.print("            c{d:0>2}_agent.adjust(10)\n", .{n});
    try w.print("            c{d:0>2}_clues += 1\n", .{n});
    try w.print("            c{d:0>2}_inventory.add(\"truth\")\n", .{n});
    try w.print("            c{d:0>2}_evidence.add(\"truth\", 2)\n", .{n});
    try w.writeAll("            :: \"The agent listens carefully.\"\n");
    try w.print("            => CHAPTER_{d:0>2}_{s}\n", .{ n, target_a });
    try w.writeAll("        }\n");

    try w.writeAll("        ~ \"Lie\" {\n");
    try w.print("            c{d:0>2}_agent.adjust(-15)\n", .{n});
    try w.print("            c{d:0>2}_state = E{d:0>2}.Tense\n", .{ n, n });
    try w.writeAll("            :: \"The agent narrows their eyes.\"\n");
    try w.print("            => CHAPTER_{d:0>2}_{s}\n", .{ n, target_b });
    try w.writeAll("        }\n");

    try w.writeAll("        ~ \"Stay silent\" {\n");
    try w.print("            c{d:0>2}_agent.note(\"silent\")\n", .{n});
    try w.writeAll("            :: \"The silence stretches.\"\n");
    try w.print("            => CHAPTER_{d:0>2}_{s}\n", .{ n, target_c });
    try w.writeAll("        }\n");

    try w.writeAll("    }\n");

    try w.print("    c{d:0>2}_summary()\n", .{n});
    try w.writeAll("}\n\n");
}

fn pickTarget(
    bough_names: [boughs_per_chapter][]const u8,
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
