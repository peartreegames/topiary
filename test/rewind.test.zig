const std = @import("std");
const testing = std.testing;

const topi = @import("topi");
const Vm = topi.runtime.Vm;
const Runner = topi.runtime.Runner;
const Line = topi.runtime.Line;
const Choice = topi.runtime.Choice;

const Value = topi.types.Value;
const Module = topi.module.Module;

const compileSource = @import("compiler.test.zig").compileSource;
const allocator = testing.allocator;

const RewindAction = union(enum) {
    /// Pick the choice at the given index.
    select: usize,
    /// Call vm.rewind() instead of selecting.
    rewind,
    /// Call vm.redo() instead of selecting.
    redo,
};

/// Test runner that drives forks with a scripted sequence of `RewindAction`s
/// and records every line and every choice list it sees, in order. This
/// makes it easy to assert that rewinding from a fork re-presents the
/// previous fork and that state mutations between forks are rolled back.
const RewindRunner = struct {
    runner: Runner,
    alloc: std.mem.Allocator,
    actions: []const RewindAction,
    action_index: usize = 0,
    output: std.ArrayList([]const u8) = .empty,
    /// One entry per fork the runner has been shown, recording the choice
    /// content list it saw at that fork.
    fork_history: std.ArrayList([][]const u8) = .empty,

    fn init(alloc: std.mem.Allocator, actions: []const RewindAction) RewindRunner {
        return .{
            .runner = .{
                .on_line = onLine,
                .on_choices = onChoices,
                .on_value_changed = onValueChanged,
            },
            .alloc = alloc,
            .actions = actions,
        };
    }

    fn deinit(self: *RewindRunner) void {
        for (self.output.items) |s| self.alloc.free(s);
        self.output.deinit(self.alloc);
        for (self.fork_history.items) |group| {
            for (group) |s| self.alloc.free(s);
            self.alloc.free(group);
        }
        self.fork_history.deinit(self.alloc);
    }

    fn onLine(_: *Runner, vm: *Vm, dialogue: Line) void {
        const self: *RewindRunner = @fieldParentPtr("runner", vm.runner);
        const owned = self.alloc.dupe(u8, dialogue.content) catch unreachable;
        self.output.append(self.alloc, owned) catch unreachable;
        vm.selectContinue();
    }

    fn onChoices(_: *Runner, vm: *Vm, choices: []Choice) void {
        const self: *RewindRunner = @fieldParentPtr("runner", vm.runner);
        const group = self.alloc.alloc([]const u8, choices.len) catch unreachable;
        for (choices, 0..) |c, i| group[i] = self.alloc.dupe(u8, c.content) catch unreachable;
        self.fork_history.append(self.alloc, group) catch unreachable;

        if (self.action_index >= self.actions.len) {
            // No-op: leave the VM paused at this fork. The driving loop
            // is responsible for stopping when actions are exhausted.
            return;
        }
        const action = self.actions[self.action_index];
        self.action_index += 1;
        switch (action) {
            .select => |i| vm.selectChoice(i) catch {},
            .rewind => vm.rewind() catch {},
            .redo => vm.redo() catch {},
        }
    }

    fn onValueChanged(_: *Runner, _: *Vm, _: []const u8, _: Value) void {}
};

fn initRewindVm(source: []const u8, mod: *Module, rr: *RewindRunner) !Vm {
    var bytecode = try compileSource(source, mod);
    errdefer bytecode.free(allocator);
    var vm = try Vm.init(allocator, std.testing.io, bytecode, &rr.runner);
    vm.history_capacity = 16;
    return vm;
}

fn runVm(vm: *Vm) !void {
    vm.interpret() catch |err| {
        var buffer: [128]u8 = undefined;
        var writer = std.Io.File.stderr().writer(std.testing.io, &buffer);
        const stderr = &writer.interface;
        vm.err.print(stderr);
        return err;
    };
}

test "Rewind: basic two-fork rewind and re-pick" {
    const source =
        \\ === START {
        \\     fork^ {
        \\         ~ "A1" => B
        \\         ~ "A2" => B
        \\     }
        \\     === B {
        \\         fork^ {
        \\             ~ "B1" { :Sp: "Picked B1" }
        \\             ~ "B2" { :Sp: "Picked B2" }
        \\         }
        \\     }
        \\ }
    ;
    const actions = [_]RewindAction{
        .{ .select = 0 }, // pick A1 -> enter B
        .rewind, // back to first fork
        .{ .select = 1 }, // pick A2 -> enter B again
        .{ .select = 1 }, // pick B2
    };

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var rr = RewindRunner.init(allocator, &actions);
    defer rr.deinit();
    var vm = try initRewindVm(source, mod, &rr);
    defer vm.deinit();
    defer vm.bytecode.free(allocator);

    try runVm(&vm);

    // We should have been shown the first fork, then the second fork, then
    // (after rewind) the first fork again, then the second fork again.
    try testing.expectEqual(@as(usize, 4), rr.fork_history.items.len);
    try testing.expectEqualStrings("A1", rr.fork_history.items[0][0]);
    try testing.expectEqualStrings("B1", rr.fork_history.items[1][0]);
    try testing.expectEqualStrings("A1", rr.fork_history.items[2][0]);
    try testing.expectEqualStrings("B1", rr.fork_history.items[3][0]);
    // Final dialogue line should be Picked B2 only.
    try testing.expectEqual(@as(usize, 1), rr.output.items.len);
    try testing.expectEqualStrings("Picked B2", rr.output.items[0]);
}

test "Rewind: visit counters roll back" {
    const source =
        \\ === START {
        \\     fork^ {
        \\         ~ "first" => B
        \\         ~ "second" => B
        \\     }
        \\     === B {
        \\         fork^ {
        \\             ~ "x" { :Sp: "visits={B}" }
        \\             ~ "y" { :Sp: "visits={B}" }
        \\         }
        \\     }
        \\ }
    ;
    const actions = [_]RewindAction{
        .{ .select = 0 }, // pick first -> B (visits B = 1)
        .rewind, // back to first fork; visit count for B should restore to 0
        .{ .select = 1 }, // pick second -> B again (visits B = 1, NOT 2)
        .{ .select = 0 },
    };

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var rr = RewindRunner.init(allocator, &actions);
    defer rr.deinit();
    var vm = try initRewindVm(source, mod, &rr);
    defer vm.deinit();
    defer vm.bytecode.free(allocator);

    try runVm(&vm);

    try testing.expectEqual(@as(usize, 1), rr.output.items.len);
    try testing.expectEqualStrings("visits=1", rr.output.items[0]);
}

test "Rewind: mutable list mutation rolled back" {
    const source =
        \\ var nums = List{1, 2, 3}
        \\ === START {
        \\     fork^ {
        \\         ~ "add" {
        \\             nums.add(99)
        \\             => SHOW
        \\         }
        \\         ~ "skip" {
        \\             => SHOW
        \\         }
        \\     }
        \\     === SHOW {
        \\         fork^ {
        \\             ~ "ok" { :Sp: "len={nums.count()}" }
        \\         }
        \\     }
        \\ }
    ;
    const actions = [_]RewindAction{
        .{ .select = 0 }, // pick add -> list becomes [1,2,3,99]
        .rewind, // back; list should restore to [1,2,3]
        .{ .select = 1 }, // pick skip -> list still [1,2,3]
        .{ .select = 0 },
    };

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var rr = RewindRunner.init(allocator, &actions);
    defer rr.deinit();
    var vm = try initRewindVm(source, mod, &rr);
    defer vm.deinit();
    defer vm.bytecode.free(allocator);

    try runVm(&vm);

    try testing.expectEqual(@as(usize, 1), rr.output.items.len);
    try testing.expectEqualStrings("len=3", rr.output.items[0]);
}

test "Rewind: ring buffer evicts oldest" {
    const source =
        \\ === START {
        \\     fork^ { ~ "1" => B }
        \\     === B {
        \\         fork^ { ~ "2" => C }
        \\         === C {
        \\             fork^ { ~ "3" => D }
        \\             === D {
        \\                 fork^ { ~ "4" { :Sp: "done" } }
        \\             }
        \\         }
        \\     }
        \\ }
    ;
    const actions = [_]RewindAction{
        .{ .select = 0 },
        .{ .select = 0 },
        .{ .select = 0 },
        .{ .select = 0 },
    };

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var rr = RewindRunner.init(allocator, &actions);
    defer rr.deinit();
    var vm = try initRewindVm(source, mod, &rr);
    vm.history_capacity = 2; // override the default of 16
    defer vm.deinit();
    defer vm.bytecode.free(allocator);

    try runVm(&vm);

    // After 4 forks with capacity 2, history should hold at most 2.
    try testing.expect(vm.history.items.len <= 2);
}

test "Rewind: redo restores forward state" {
    const source =
        \\ === START {
        \\     fork^ {
        \\         ~ "A" => B
        \\         ~ "B" => B
        \\     }
        \\     === B {
        \\         fork^ {
        \\             ~ "X" { :Sp: "X" }
        \\             ~ "Y" { :Sp: "Y" }
        \\         }
        \\     }
        \\ }
    ;
    const actions = [_]RewindAction{
        .{ .select = 0 }, // pick A -> B
        .rewind, // back to first fork
        .redo, // forward again to B's fork
        .{ .select = 0 }, // pick X
    };

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var rr = RewindRunner.init(allocator, &actions);
    defer rr.deinit();
    var vm = try initRewindVm(source, mod, &rr);
    defer vm.deinit();
    defer vm.bytecode.free(allocator);

    try runVm(&vm);

    // Sequence: first-fork, B-fork (initial), first-fork (after rewind),
    // B-fork (after redo).
    try testing.expectEqual(@as(usize, 4), rr.fork_history.items.len);
    try testing.expectEqualStrings("A", rr.fork_history.items[0][0]);
    try testing.expectEqualStrings("X", rr.fork_history.items[1][0]);
    try testing.expectEqualStrings("A", rr.fork_history.items[2][0]);
    try testing.expectEqualStrings("X", rr.fork_history.items[3][0]);
    try testing.expectEqual(@as(usize, 1), rr.output.items.len);
    try testing.expectEqualStrings("X", rr.output.items[0]);
}

test "Rewind: new pick after rewind clears redo" {
    const source =
        \\ === START {
        \\     fork^ {
        \\         ~ "A" => B
        \\         ~ "B" => B
        \\     }
        \\     === B {
        \\         fork^ {
        \\             ~ "X" { :Sp: "done" }
        \\         }
        \\     }
        \\ }
    ;
    const actions = [_]RewindAction{
        .{ .select = 0 }, // A -> B
        .rewind, // back
        .{ .select = 1 }, // pick B (different) -> should clear forward redo
        .{ .select = 0 }, // X
    };

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var rr = RewindRunner.init(allocator, &actions);
    defer rr.deinit();
    var vm = try initRewindVm(source, mod, &rr);
    defer vm.deinit();
    defer vm.bytecode.free(allocator);

    try runVm(&vm);

    // After the script finishes, we should not be able to redo: picking
    // forward from a rewound state invalidates any future redo entries.
    try testing.expect(!vm.canRedo());
}

test "Rewind: 3-fork chain rewind goes to previous, not current" {
    const source =
        \\ === START {
        \\     fork^ {
        \\         ~ "A1" => B
        \\         ~ "A2" => B
        \\     }
        \\     === B {
        \\         fork^ {
        \\             ~ "B1" => C
        \\             ~ "B2" => C
        \\         }
        \\         === C {
        \\             fork^ {
        \\                 ~ "C1" { :Sp: "picked C1" }
        \\                 ~ "C2" { :Sp: "picked C2" }
        \\             }
        \\         }
        \\     }
        \\ }
    ;
    const actions = [_]RewindAction{
        .{ .select = 0 }, // A1 -> B
        .{ .select = 0 }, // B1 -> C
        .rewind, // back from C to B
        .{ .select = 1 }, // B2 -> C again
        .{ .select = 0 }, // C1
    };

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var rr = RewindRunner.init(allocator, &actions);
    defer rr.deinit();
    var vm = try initRewindVm(source, mod, &rr);
    defer vm.deinit();
    defer vm.bytecode.free(allocator);

    try runVm(&vm);

    // Sequence of forks shown: A, B, C, B (after rewind), C (after re-pick).
    try testing.expectEqual(@as(usize, 5), rr.fork_history.items.len);
    try testing.expectEqualStrings("A1", rr.fork_history.items[0][0]);
    try testing.expectEqualStrings("B1", rr.fork_history.items[1][0]);
    try testing.expectEqualStrings("C1", rr.fork_history.items[2][0]);
    try testing.expectEqualStrings("B1", rr.fork_history.items[3][0]);
    try testing.expectEqualStrings("C1", rr.fork_history.items[4][0]);
    try testing.expectEqual(@as(usize, 1), rr.output.items.len);
    try testing.expectEqualStrings("picked C1", rr.output.items[0]);
}

test "Rewind: deinit while paused at rewound fork doesn't leak" {
    // Regression: rewinding allocates fresh `current_choices` and tag
    // arrays in vm.alloc; if the host destroys the VM before the user
    // picks (e.g. closes the conversation), Vm.deinit must free both the
    // outer slice and each Choice.tags array. The testing allocator will
    // fail this test if anything leaks.
    const source =
        \\ === START {
        \\     fork^ {
        \\         ~ "A1" #t1 #t2 => B
        \\         ~ "A2" #t3 => B
        \\     }
        \\     === B {
        \\         fork^ {
        \\             ~ "B1" #t4 { :Sp: "B1" }
        \\             ~ "B2" #t5 #t6 #t7 { :Sp: "B2" }
        \\         }
        \\     }
        \\ }
    ;
    const actions = [_]RewindAction{
        .{ .select = 0 }, // A1 -> B
        .rewind, // back to first fork; restore allocates tag arrays
        // No further action: VM will be deinit'd while paused at the
        // rewound fork with un-freed current_choices.
    };

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var rr = RewindRunner.init(allocator, &actions);
    defer rr.deinit();
    var vm = try initRewindVm(source, mod, &rr);
    defer vm.deinit();
    defer vm.bytecode.free(allocator);

    // Drive the VM until the runner runs out of scripted actions and
    // leaves us paused at the rewound first fork. Mirror vm.interpret()'s
    // anchor-discovery so START is actually entered.
    const start_path: ?[]const u8 = for (vm.bytecode.constants) |v| {
        if (v == .obj and v.obj.data == .anchor) break v.obj.data.anchor.name;
    } else null;
    try vm.start(start_path);
    var iterations: usize = 0;
    while (vm.can_continue and iterations < 1000) : (iterations += 1) {
        if (rr.action_index >= rr.actions.len and vm.is_waiting) break;
        vm.run() catch break;
    }

    // VM should now be paused at the rewound first fork.
    try testing.expect(vm.is_waiting);
    try testing.expect(!vm.choices_freed);
    // The defer chain above will deinit vm; if we leak tag arrays the
    // testing allocator will report it and fail the test.
}

test "Rewind: GC stress with tight threshold" {
    const source =
        \\ var nums = List{1, 2, 3}
        \\ === START {
        \\     fork^ {
        \\         ~ "add" {
        \\             nums.add(99)
        \\             => SHOW
        \\         }
        \\     }
        \\     === SHOW {
        \\         fork^ {
        \\             ~ "ok" { :Sp: "{nums.count()}" }
        \\         }
        \\     }
        \\ }
    ;
    const actions = [_]RewindAction{
        .{ .select = 0 },
        .rewind,
        .{ .select = 0 },
        .{ .select = 0 },
    };

    var mod = try Module.initEmpty(allocator, std.testing.io);
    defer mod.deinit();
    var rr = RewindRunner.init(allocator, &actions);
    defer rr.deinit();
    var vm = try initRewindVm(source, mod, &rr);
    vm.gc.setThreshold(1); // force collection on every allocation
    defer vm.deinit();
    defer vm.bytecode.free(allocator);

    try runVm(&vm);

    // Should not have crashed; output should reflect the latest state.
    try testing.expectEqual(@as(usize, 1), rr.output.items.len);
}
