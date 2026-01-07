const std = @import("std");

const backend = @import("../backend/index.zig");
const Compiler = backend.Compiler;
const Bytecode = backend.Bytecode;
const OpCode = backend.OpCode;

const types = @import("../types/index.zig");
const Class = types.Class;
const Value = types.Value;
const Nil = types.Nil;
const True = types.True;
const False = types.False;
const Void = types.Void;
const Iterator = types.Iterator;
const Function = types.Function;

const Extern = @import("./extern.zig").Extern;

const utils = @import("../utils/index.zig");
const UUID = utils.UUID;
const C = utils.C;

const Stack = @import("stack.zig").Stack;
const Gc = @import("gc.zig").Gc;
const Frame = @import("frame.zig").Frame;
const StateMap = @import("state.zig").StateMap;
const builtins = @import("builtins.zig");

const runners = @import("runner.zig");
const Runner = runners.Runner;
const Line = runners.Line;
const Choice = runners.Choice;
const RuntimeErr = @import("error.zig").RuntimeErr;

const stack_size = 4096;
const frame_size = 255;
const iterator_size = 255;
const globals_size = 65535;

const InterpretResult = union(enum) {
    Completed,
    Paused,
};

/// Virtual Machine
/// Executes bytecode
pub const Vm = struct {
    alloc: std.mem.Allocator,
    frames: Stack(Frame),
    err: RuntimeErr,
    gc: Gc,
    globals: []Value,
    value_subscribers: std.StringHashMapUnmanaged(void),

    stack: Stack(Value),
    /// Current iterators to allow easy nesting
    iterators: Stack(Iterator),
    /// List of positions to jump back to using `^`
    jump_backups: std.ArrayList(C.JUMP),
    /// Used to ensure preceding code is executed before arriving at Bough
    jump_requests: std.ArrayList(C.JUMP),
    anchor_stack: std.ArrayList(C.CONSTANT),
    externs: std.ArrayList(*Extern),

    bytecode: Bytecode,
    /// Current instruction position
    ip: usize = 0,
    break_on_assert: bool = true,

    /// Used to cache the choices
    choices_list: std.ArrayList(Choice),
    /// Used to send to the on_choices method
    current_choices: []Choice = undefined,
    choices_freed: bool = true,
    /// Determines if the vm is waiting on input
    /// hopefully will remove with async?
    is_waiting: bool = false,
    can_continue: bool = false,

    /// The localization language key
    /// eg. en-US, zh-CN, de, etc
    loc_key: ?[]const u8 = null,

    /// The currently loaded localization map
    loc_map: std.AutoHashMapUnmanaged(UUID.ID, []const u8),

    runner: *Runner,

    pub const Error = error{
        RuntimeError,
        BoughNotFound,
        InvalidChoice,
        Uninitialized,
    } || Compiler.Error;

    /// Initialize Vm
    pub fn init(allocator: std.mem.Allocator, bytecode: Bytecode, runner: anytype) !Vm {
        const globals = try allocator.alloc(Value, bytecode.global_symbols.len);
        @memset(globals, .void);
        for (bytecode.constants) |c| {
            if (c != .obj) continue;
            switch (c.obj.data) {
                // init visit
                .anchor => globals[c.obj.data.anchor.visit_index] = .{ .visit = 0 },
                else => {},
            }
        }

        const main_func_obj = try allocator.create(Value.Obj);
        main_func_obj.* = .{
            .id = UUID.new(),
            .data = .{
                .function = .{
                    .arity = 0,
                    .instructions = bytecode.instructions,
                    .locals_count = bytecode.locals_count,
                    .debug_info = bytecode.debug_info,
                },
            },
        };
        var vm = Vm{
            .alloc = allocator,
            .bytecode = bytecode,
            .frames = try Stack(Frame).init(allocator, frame_size),
            .err = .{},
            .globals = globals,
            .runner = runner,
            .gc = Gc.init(allocator),
            .stack = try Stack(Value).init(allocator, stack_size),
            .iterators = try Stack(Iterator).init(allocator, iterator_size),
            .jump_backups = .empty,
            .jump_requests = .empty,
            .anchor_stack = .empty,
            .externs = .empty,
            .choices_list = .empty,
            .value_subscribers = .empty,
            .loc_map = .empty,
        };

        vm.stack.resize(bytecode.locals_count);
        vm.frames.push(try Frame.create(main_func_obj, 0));
        return vm;
    }

    pub fn deinit(self: *Vm) void {
        self.alloc.destroy(self.frames.backing[0].func);
        self.frames.deinit();
        self.choices_list.deinit(self.alloc);
        self.stack.deinit();
        self.iterators.deinit();
        self.jump_backups.deinit(self.alloc);
        self.anchor_stack.deinit(self.alloc);
        self.jump_requests.deinit(self.alloc);
        self.externs.deinit(self.alloc);
        self.gc.deinit();
        self.value_subscribers.deinit(self.alloc);
        self.alloc.free(self.globals);
        if (!self.choices_freed) self.alloc.free(self.current_choices);
        if (self.err.msg) |msg| {
            self.alloc.free(msg);
            self.err.msg = null;
            self.err.file = null;
        }
    }

    pub fn setLocale(self: *Vm, key: ?[]const u8) !void {
        self.loc_key = key;
        self.loc_map.clearRetainingCapacity();
        if (key == null) return;
        var fbs = std.Io.fixedBufferStream(self.bytecode.loc);
        var reader = fbs.reader();
        var line = std.ArrayList(u8).empty;
        defer line.deinit(self.alloc);
        const writer = line.writer(self.alloc);
        try reader.streamUntilDelimiter(writer, '\n', null);
        var headers = std.mem.splitSequence(u8, line.items, ",");
        var index: usize = 0;
        while (headers.next()) |header| : (index += 1) {
            if (std.mem.eql(u8, key.?, std.mem.trim(u8, header, "\""))) break;
        }
        line.clearRetainingCapacity();
        while (true) {
            defer line.clearRetainingCapacity();
            reader.streamUntilDelimiter(writer, '\n', null) catch break;
            var i: usize = 0;
            var c_start: usize = 0;
            var count: usize = 0;
            var in_value = false;
            const id: UUID.ID = UUID.fromString(line.items[1..(UUID.Size + 1)]);
            // Since lines can include ',' and '""' we'll parse out the cells manually
            while (i < line.items.len) : (i += 1) {
                const c = line.items[i];
                const is_end = i == line.items.len - 1;
                if (!is_end) {
                    if (c == '"' and line.items[i + 1] != '"') in_value = !in_value;
                    if (in_value) continue;
                    if (c == ',') count += 1;
                    if (count == index) c_start = i + 2;
                    if (count <= index) continue;
                }
                try self.loc_map.put(self.alloc, id, try self.alloc.dupe(u8, line.items[c_start..i]));
            }
        }
    }

    /// Returns root values that should not be cleaned up by the garbage collector
    pub fn roots(self: *Vm) []const []Value {
        return &([_][]Value{ self.globals, self.stack.backing });
    }

    fn currentFrame(self: *Vm) *Frame {
        return self.frames.peek();
    }

    /// Continue execution
    pub fn selectContinue(self: *Vm) void {
        self.is_waiting = false;
    }

    pub fn selectChoice(self: *Vm, index: usize) Error!void {
        if (index < 0 or index >= self.current_choices.len) {
            return Error.InvalidChoice;
        }

        const choice = self.current_choices[index];
        self.currentFrame().ip = choice.ip;
        self.is_waiting = false;
        for (self.current_choices) |c| {
            self.alloc.free(c.tags);
        }
        self.alloc.free(self.current_choices);
        self.choices_freed = true;
    }

    pub fn getGlobalsIndex(self: *Vm, name: []const u8) !usize {
        for (self.bytecode.global_symbols) |s| {
            if (!std.mem.eql(u8, name, s.name)) continue;
            return s.index;
        }
        return error.SymbolNotFound;
    }

    fn getExternIndex(self: *Vm, name: []const u8) !usize {
        for (self.bytecode.constants, 0..) |c, i| {
            if (c != .obj or c.obj.data != .function) continue;
            if (c.obj.data.function.extern_name) |func_name| {
                if (!std.mem.eql(u8, name, func_name)) return i;
            }
        }
        return error.NotFound;
    }

    pub fn setExtern(self: *Vm, name: []const u8, value: *Extern) !void {
        const index = try self.getExternIndex(name);
        if (self.bytecode.constants[index].obj.data.function.extern_index != null)
            return error.AlreadySet;
        try self.externs.append(self.alloc, value);
        self.bytecode.constants[index].obj.data.function.extern_index = @intCast(self.externs.items.len - 1);
    }

    pub fn getExtern(self: *Vm, name: []const u8) !Value {
        const index = self.getExternIndex(name) catch |err| {
            if (err == Error.IllegalOperation) return err;
            return Nil;
        };
        return self.globals[index];
    }

    pub fn subscribeToValueChange(self: *Vm, name: []const u8) !bool {
        if (self.value_subscribers.contains(name)) return true;
        for (self.bytecode.global_symbols) |s| {
            if (!std.mem.eql(u8, name, s.name)) continue;
            try self.value_subscribers.put(self.alloc, s.name, {});
            return true;
        }
        return false;
    }

    pub fn unusbscribeToValueChange(self: *Vm, name: []const u8) bool {
        return self.value_subscribers.remove(name);
    }

    pub fn notifyValueChange(self: *Vm, index: usize, old_value: Value, new_value: Value) void {
        const name = self.bytecode.global_symbols[index].name;
        if (self.value_subscribers.contains(name) and !old_value.eql(new_value)) {
            self.runner.onValueChanged(self, name, new_value);
        }
    }

    pub fn interpret(self: *Vm) !void {
        const path: ?[]const u8 = for (self.bytecode.constants) |v| {
            if (v == .obj and v.obj.data == .anchor) break v.obj.data.anchor.name;
        } else null;
        try self.start(path);
        while (self.can_continue) {
            try self.run();
        }
    }

    pub fn start(self: *Vm, start_path: ?[]const u8) !void {
        self.can_continue = true;
        self.stack.resize(self.bytecode.locals_count);
        while (self.frames.count > 1) {
            _ = self.frames.pop();
        }
        self.currentFrame().ip = 0;
        for (self.bytecode.constants, 0..) |v, i| {
            if (v == .obj and v.obj.data == .anchor) {
                if (start_path != null and !std.mem.eql(u8, v.obj.data.anchor.name, start_path.?)) continue;
                try self.prepareDivert(@intCast(i));
                return;
            }
        } else {
            self.err.msg = try std.fmt.allocPrint(self.alloc, "Could not find starting path {?s}", .{start_path});
            self.err.line = 1;
            return Error.BoughNotFound;
        }
    }

    fn fail(self: *Vm, comptime msg: []const u8, args: anytype) !void {
        self.err.msg = try std.fmt.allocPrint(self.alloc, msg, args);
        const ip = self.currentFrame().ip;
        cont: for (self.currentFrame().func.data.function.debug_info) |d| {
            for (d.ranges.items) |r| {
                if (ip >= r.start and ip <= r.end) {
                    self.err.line = r.line;
                    self.err.file = d.file;
                    break :cont;
                }
            }
        }
        while (self.frames.count > 1) {
            _ = self.frames.pop();
        }
        self.can_continue = false;
        return Error.RuntimeError;
    }

    fn takeByte(self: *Vm) u8 {
        var frame = self.currentFrame();
        const byte = frame.instructions()[frame.ip];
        frame.ip += 1;
        return byte;
    }

    fn takeInt(self: *Vm, comptime T: type) T {
        var frame = self.currentFrame();
        if (T == u8) {
            return self.takeByte();
        } else {
            const result = std.mem.readVarInt(T, frame.instructions()[frame.ip..(frame.ip + @sizeOf(T))], .little);
            frame.ip += @sizeOf(T);
            return result;
        }
    }

    fn end(self: *Vm) void {
        self.can_continue = false;
    }

    pub fn run(self: *Vm) !void {
        if (!self.can_continue or self.is_waiting) return;
        while (self.ip < self.currentFrame().instructions().len) : (self.ip = self.currentFrame().ip) {
            const instruction = self.takeByte();
            if (instruction == 170) {
                self.end();
                return;
            }
            const op: OpCode = @enumFromInt(instruction);
            const is_in_jump = self.jump_requests.items.len > 0;

            switch (op) {
                .constant => {
                    const index = self.takeInt(C.CONSTANT);
                    if (index >= self.bytecode.constants.len)
                        return self.fail("Constant index {d} outside of bounds length {d}", .{ index, self.bytecode.constants.len });
                    const value = self.bytecode.constants[index];
                    try self.push(value);
                },
                .pop => _ = try self.pop(),
                .add => {
                    var right = try self.pop();
                    var left = try self.pop();
                    if (right == .visit) {
                        const value: f32 = @floatFromInt(right.visit);
                        right = .{ .number = value };
                    }
                    if (left == .visit) {
                        const value: f32 = @floatFromInt(left.visit);
                        left = .{ .number = value };
                    }
                    if (left == .obj and left.obj.data == .string) {
                        const str = left.obj.data.string;
                        left = .{ .const_string = str };
                    }
                    if (right == .obj and right.obj.data == .string) {
                        const str = right.obj.data.string;
                        right = .{ .const_string = str };
                    }

                    if (@intFromEnum(right) != @intFromEnum(left)) {
                        return self.fail("Cannot add types {s} and {s}: {f} {f}", .{ left.typeName(), right.typeName(), left, right });
                    }
                    switch (right) {
                        .const_string => |s| try self.pushAlloc(.{ .string = try std.mem.concat(self.alloc, u8, &.{ std.mem.trimRight(u8, left.const_string, &[_]u8{0}), s }) }),
                        .number => try self.push(.{ .number = right.number + left.number }),
                        .obj => |o| {
                            switch (o.data) {
                                else => return self.fail("Cannot add types {s} and {s}: {f} {f}", .{ left.typeName(), right.typeName(), left, right }),
                            }
                        },
                        else => return self.fail("Cannot add types {s} and {s}: {f} {f}", .{ left.typeName(), right.typeName(), left, right }),
                    }
                },
                .subtract, .multiply, .divide, .modulus => try self.binaryNumberOp(op),
                .equal, .not_equal, .greater_than, .greater_than_equal => try self.comparisonOp(op),
                .true => try self.push(True),
                .false => try self.push(False),
                .nil => try self.push(Nil),
                .negate => try self.push(.{ .number = -(try self.pop()).number }),
                .not => {
                    const value = try self.pop();
                    switch (value) {
                        .bool => |b| try self.push(if (b) False else True),
                        .nil => try self.push(True),
                        .number => |n| try self.push(if (@abs(n) < 0.00001) False else True),
                        else => try self.push(False),
                    }
                },
                .@"or" => {
                    const right = try self.pop();
                    const left = try self.pop();
                    if (right != .bool or left != .bool) {
                        return self.fail("Conditionals must be of type bool not types {s} and {s}", .{ left.typeName(), right.typeName() });
                    }
                    try self.push(.{ .bool = right.bool or left.bool });
                },
                .@"and" => {
                    const right = try self.pop();
                    const left = try self.pop();
                    if (right != .bool or left != .bool) {
                        return self.fail("Conditionals must be of type bool not types {s} and {s}", .{ left.typeName(), right.typeName() });
                    }
                    try self.push(.{ .bool = right.bool and left.bool });
                },
                .jump => {
                    const dest = self.takeInt(C.JUMP);
                    if (self.jump_requests.pop()) |req| {
                        self.currentFrame().ip = req;
                        continue;
                    }
                    if (dest > self.currentFrame().instructions().len) break;
                    self.currentFrame().ip = dest;
                },
                .jump_if_false => {
                    const dest = self.takeInt(C.JUMP);
                    var condition = try self.pop();
                    if (!try condition.isTruthy()) {
                        self.currentFrame().ip = dest;
                    }
                },
                .prong => {
                    const dest = self.takeInt(C.JUMP);
                    const values_count = self.takeInt(u8);
                    const index = self.stack.count - values_count - 1;
                    const capture = self.stack.items[index];
                    var i: usize = 0;
                    var match = false;
                    while (i < values_count) : (i += 1) {
                        const value = try self.pop();
                        if (!self.prongIsMatch(capture, value)) continue;
                        match = true;
                        self.stack.count -= values_count - i - 1;
                        break;
                    }

                    if (match or values_count == 0) {
                        self.currentFrame().ip = dest;
                    }
                },
                .decl_global => {
                    const index = self.takeInt(C.GLOBAL);
                    if (index > globals_size) return self.fail("Globals index {} is out of bounds of max size", .{index});
                    const value = try self.pop();
                    // global already set from loaded state
                    if (self.globals[index] != .void) {
                        continue;
                    }
                    if (value == .obj) value.obj.index = @intCast(index);
                    self.globals[index] = value;
                },
                .set_global => {
                    const index = self.takeInt(C.GLOBAL);
                    if (index > globals_size) return self.fail("Globals index {} is out of bounds of max size", .{index});

                    if (index >= self.globals.len) return self.fail("Globals index {} is out of bounds of current size {}", .{ index, self.globals.len });
                    var value = try self.pop();
                    const current = self.globals[index];
                    if (current == .enum_value and value == .enum_value and current.enum_value.base == value.enum_value.base and current.enum_value.base.data.@"enum".is_seq) {
                        if (current.enum_value.index > value.enum_value.index) value = current;
                    }
                    if (value == .obj) value.obj.index = @intCast(index);
                    const old_value = self.globals[index];
                    self.globals[index] = value;
                    self.notifyValueChange(index, old_value, value);
                },
                .get_global => {
                    const index = self.takeInt(C.GLOBAL);
                    const value = self.globals[index];
                    try self.push(value);
                },
                .set_local => {
                    const index = self.takeInt(C.LOCAL);
                    const frame = self.currentFrame();
                    var value = try self.pop();
                    const current = self.stack.items[frame.bp + index];
                    if (current == .enum_value and value == .enum_value and current.enum_value.base == value.enum_value.base and current.enum_value.base.data.@"enum".is_seq) {
                        if (current.enum_value.index > value.enum_value.index) value = current;
                    }
                    self.stack.items[frame.bp + index] = value;
                },
                .get_local => {
                    const index = self.takeInt(C.LOCAL);
                    const frame = self.currentFrame();
                    const value = self.stack.items[frame.bp + index];
                    try self.push(value);
                },
                .set_property => {
                    const field_value = try self.pop();
                    const instance_value = try self.pop();
                    const new_value = try self.pop();
                    if (instance_value != .obj) return self.fail("Property may only be set on object types, found '{s}'", .{instance_value.typeName()});

                    switch (instance_value.obj.data) {
                        .list => |l| {
                            const n: usize = @intFromFloat(field_value.number);
                            if (n >= l.items.len) return self.fail("Index {d} out of bounds for list of length {d}", .{ n, l.items.len });
                            l.items[n] = new_value;
                        },
                        .map => |*m| {
                            try m.*.put(self.alloc, field_value, new_value);
                        },
                        .instance => |inst| {
                            const field_name = field_value.obj.data.string;
                            if (inst.base.data.class.getFieldIndex(field_name)) |idx| {
                                inst.fields[idx] = new_value;
                            } else {
                                return self.fail("Instance of '{s}' does not contain field '{s}'", .{ inst.base.data.class.name, field_name });
                            }
                        },
                        // todo add string indexing
                        else => return self.fail("Cannot index '{s}' into type {s}", .{ field_value.typeName(), instance_value.typeName() }),
                    }
                },
                .get_upvalue => {
                    const frames_to_skip = self.takeInt(u8);
                    const index = self.takeInt(C.LOCAL);
                    const frame_count = self.frames.items.len;

                    if (frame_count < frames_to_skip) {
                        return self.fail("Variable is no longer in scope", .{});
                    }

                    const target_frame = self.frames.items[frame_count - 1 - frames_to_skip];
                    const value = self.stack.items[target_frame.bp + index];
                    self.stack.push(value);
                },
                .set_upvalue => {
                    const frames_to_skip = self.takeInt(u8);
                    const index = self.takeInt(C.LOCAL);
                    const frame_count = self.frames.items.len;

                    if (frame_count < frames_to_skip) {
                        return self.fail("Variable is no longer in scope", .{});
                    }

                    const target_frame = self.frames.items[frame_count - frames_to_skip];
                    self.stack.items[target_frame.bp + index] = try self.pop();
                },
                .string, .loc => {
                    const index = self.takeInt(C.CONSTANT);
                    const str = if (op == .string) self.bytecode.constants[index].obj.*.data.string else self.loc_map.get(self.bytecode.uuids[index]) orelse return self.fail("Could not find localization id '{s}'", .{self.bytecode.uuids[index]});
                    var count = self.takeInt(u8);

                    if (count == 0) {
                        try self.push(.{ .const_string = str });
                        continue;
                    }

                    var args = try self.alloc.alloc(Value, count);
                    defer self.alloc.free(args);
                    while (count > 0) : (count -= 1) {
                        args[count - 1] = try self.pop();
                    }
                    var list = std.ArrayList(u8).empty;
                    defer list.deinit(self.alloc);
                    var writer = list.writer(self.alloc);
                    // index
                    var i: usize = 0;
                    // start
                    var s: usize = 0;
                    // need to implement our own formatter due to runtime values
                    while (i < str.len) : (i += 1) {
                        var c = str[i];
                        if (c == '{') {
                            try writer.writeAll(str[s..i]);
                            const open = i + 1;
                            var close = open;
                            while (c != '}') : (i += 1) {
                                c = str[i];
                                close = i;
                            }
                            const arg_index = try std.fmt.parseInt(u8, str[open..close], 10);
                            const val = args[arg_index];
                            switch (val) {
                                .number => |n| {
                                    try std.fmt.format(writer, "{d}", .{n});
                                },
                                .timestamp => |t| {
                                    try std.fmt.format(writer, "{d}", .{t});
                                },
                                .bool => |b| try writer.writeAll(if (b) "true" else "false"),
                                .enum_value => |e| {
                                    if (e.index >= e.base.data.@"enum".values.len)
                                        return self.fail("Enum index {d} is out of bounds for '{s}'", .{ e.index, e.base.data.@"enum".name });
                                    try writer.writeAll(e.base.data.@"enum".values[e.index]);
                                },
                                .const_string => |cs| try writer.writeAll(std.mem.trimRight(u8, cs, &[_]u8{0})),
                                .obj => |o| {
                                    switch (o.data) {
                                        // remove final 0
                                        .string => try writer.writeAll(std.mem.trimRight(u8, o.data.string, &[_]u8{0})),
                                        else => return self.fail("Unsupported interpolated type '{s}' for '{s}'", .{ val.typeName(), str }),
                                    }
                                },
                                .visit => |v| try writer.print("{}", .{v}),
                                else => return self.fail("Unsupported interpolated type '{s}' for '{s}'", .{ val.typeName(), str }),
                            }
                            s = i;
                        }
                    }
                    try writer.writeAll(str[s..]);
                    try self.pushAlloc(.{ .string = try list.toOwnedSlice(self.alloc) });
                },
                .list => {
                    var count = self.takeInt(C.COLLECTION);
                    var list = try std.ArrayList(Value).initCapacity(self.alloc, count);
                    while (count > 0) : (count -= 1) {
                        list.appendAssumeCapacity(try self.pop());
                    }
                    std.mem.reverse(Value, list.items);
                    try self.pushAlloc(.{ .list = list });
                },
                .map => {
                    var count = self.takeInt(C.COLLECTION);
                    var map = Value.Obj.MapType.empty;
                    while (count > 0) : (count -= 1) {
                        const value = try self.pop();
                        const key = try self.pop();
                        try map.put(self.alloc, key, value);
                    }
                    map.sort(Value.adapter);
                    try self.pushAlloc(.{ .map = map });
                },
                .set => {
                    var count = self.takeInt(C.COLLECTION);
                    var set = Value.Obj.SetType.empty;
                    while (count > 0) : (count -= 1) {
                        try set.put(self.alloc, try self.pop(), {});
                    }
                    set.sort(Value.adapter);
                    try self.pushAlloc(.{ .set = set });
                },
                .iter_start => {
                    var value = try self.pop();
                    self.iterators.push(.{
                        .value = value,
                        .index = 0,
                    });
                    if (value.len() > 0) try self.push(value.getAtIndex(0));
                },
                .iter_next => {
                    const iter = self.iterators.peek().*;
                    var value = iter.value;
                    if (iter.index >= value.len()) {
                        try self.push(Nil);
                        try self.push(False);
                    } else {
                        try self.push(value.getAtIndex(iter.index));
                        try self.push(True);
                    }
                    self.iterators.peek().index += 1;
                },
                .iter_end => {
                    _ = self.iterators.pop();
                },
                .instance => {
                    const value = try self.pop();
                    if (value != .obj and value.obj.data != .class)
                        return self.fail("Instance expected type class, but found '{s}'", .{value.typeName()});

                    var count = self.takeInt(C.FIELDS);
                    var fields = std.ArrayList(Class.Member).empty;
                    defer fields.deinit(self.alloc);
                    while (count > 0) : (count -= 1) {
                        const name = try self.pop();
                        const str_name = name.obj.data.string;
                        const field = try self.pop();
                        try fields.append(self.alloc, .{
                            .name = str_name,
                            .value = field,
                        });
                    }
                    std.mem.reverse(Class.Member, fields.items);
                    const instance = try self.createInstance(value.obj, try fields.toOwnedSlice(self.alloc));
                    try self.pushAlloc(.{ .instance = instance });
                },
                .range => {
                    const left = try self.pop();
                    const right = try self.pop();

                    if (left != .number or right != .number)
                        return self.fail("Range must be two number, found '{s}' and '{s}'", .{ left.typeName(), right.typeName() });
                    try self.push(.{
                        .range = .{
                            .start = @as(i32, @intFromFloat(left.number)),
                            .end = @as(i32, @intFromFloat(right.number)),
                        },
                    });
                },
                .index => {
                    const index = try self.pop();
                    const target = try self.pop();
                    switch (target) {
                        .obj => |o| switch (o.data) {
                            .string => {
                                if (index.asString()) |name| {
                                    if (std.mem.eql(u8, name, "has")) {
                                        try self.push(builtins.methods.get("has").?);
                                        try self.push(target);
                                    } else if (std.mem.eql(u8, name, "count")) {
                                        try self.push(.{ .number = @as(f32, @floatFromInt(o.data.string.len)) });
                                    } else return self.fail("Unknown method '{s}' on string. Only \"count\", \"has\" are allowed.", .{index.obj.data.string});
                                }
                            },
                            .list => |l| {
                                if (index.asString()) |name| {
                                    if (builtins.methods.get(name)) |method_value| {
                                        try self.push(method_value);
                                        try self.push(target);
                                    } else return self.fail("Unknown method '{s}' on list. Only \"count\", \"add\", \"remove\", \"has\", or \"clear\" are allowed.", .{index.obj.data.string});
                                } else if (index == .number) {
                                    const i = @as(u32, @intFromFloat(index.number));
                                    if (i < 0 or i >= l.items.len) {
                                        try self.push(Nil);
                                    } else try self.push(l.items[i]);
                                } else try self.push(Nil);
                            },
                            .map => |m| {
                                if (m.get(index)) |v| {
                                    try self.push(v);
                                } else if (index.asString()) |name| {
                                    if (std.mem.eql(u8, name, "add")) {
                                        try self.push(builtins.methods.get("__addmap").?);
                                        try self.push(target);
                                    } else if (builtins.methods.get(name)) |method_value| {
                                        try self.push(method_value);
                                        try self.push(target);
                                    } else return self.fail("Unknown method '{s}' on map. Only \"count\", \"add\", \"remove\", \"has\", or \"clear\" are allowed.", .{index.obj.data.string});
                                } else try self.push(Nil);
                            },
                            .set => {
                                const name = index.asString() orelse return self.fail("Can only query set methods by string name, found '{s}'", .{index.typeName()});
                                const method = builtins.methods.get(name) orelse return self.fail("Unknown method '{s}' on set. Only \"count\", \"add\", \"remove\", \"has\", or \"clear\" are allowed.", .{index.obj.data.string});
                                try self.push(method);
                                try self.push(target);
                            },
                            .instance => |i| {
                                const name = index.asString() orelse return self.fail("Can only query instance fields by string name, found '{s}'", .{index.typeName()});
                                const value = try i.getProperty(name) orelse return self.fail("Unknown field '{s}' on instance of {s}.", .{ name, i.base.data.class.name });
                                try self.push(value);
                                if (value == .obj and value.obj.data == .function) {
                                    // push instance self
                                    try self.push(target);
                                }
                            },
                            .@"enum" => |e| {
                                const name = index.asString() orelse return self.fail("Can only query enum values by string name, found '{s}'", .{index.typeName()});
                                for (e.values, 0..) |value, i| {
                                    if (std.mem.eql(u8, value, name)) {
                                        try self.push(.{ .enum_value = .{ .index = @intCast(i), .base = target.obj } });
                                        break;
                                    }
                                } else return self.fail("Unknown value '{s}' on enum '{s}'", .{ index.obj.data.string, e.name });
                            },
                            .class => |c| {
                                const name = index.asString() orelse return self.fail("Can only query class fields by string name, found '{s}'", .{index.typeName()});
                                for (c.fields) |field| {
                                    if (std.mem.eql(u8, field.name, name)) {
                                        try self.push(field.value);
                                        break;
                                    }
                                } else {
                                    for (c.methods) |method| {
                                        if (std.mem.eql(u8, method.name, name)) {
                                            try self.push(method.value);
                                            if (method.value == .obj and method.value.obj.data == .function) {
                                                try self.push(Nil); // add nil for "self" in static method
                                            }
                                            break;
                                        }
                                    } else return self.fail("Unknown value '{s}' on Class '{s}'", .{ name, c.name });
                                }
                            },
                            else => return self.fail("Unknown target type '{s}' to index. Only lists, maps, sets, or instances can be indexed.", .{index.typeName()}),
                        },
                        .map_pair => |mp| {
                            const name = index.asString() orelse return self.fail("Unknown index key '{s}' on map key/value pair. Only \"key\" or \"value\" are allowed.", .{index.typeName()});
                            if (std.mem.eql(u8, name, "key")) {
                                try self.push(mp.key.*);
                            } else if (std.mem.eql(u8, name, "value")) {
                                try self.push(mp.value.*);
                            } else return self.fail("Unknown index key '{s}' on map key/value pair. Only \"key\" or \"value\" are allowed.", .{index.typeName()});
                        },
                        else => {
                            return self.fail("Invalid index '{s}' on target type '{s}'", .{ index.typeName(), target.typeName() });
                        },
                    }
                },
                .dialogue => {
                    const has_speaker = self.takeInt(u8) == 1;
                    var speaker: ?[]const u8 = null;
                    if (has_speaker) {
                        const speaker_value = try self.pop();
                        if (speaker_value == .obj and speaker_value.obj.data != .string)
                            return self.fail("Speaker must be of type string, but found '{s}'", .{speaker_value.typeName()});
                        speaker = speaker_value.obj.data.string;
                    }

                    const dialogue_value = try self.pop();
                    const dialogue_str = dialogue_value.asString() orelse return self.fail("Dialogue must be of type string, but found '{s}'", .{dialogue_value.typeName()});

                    const tag_count = self.takeInt(u8);
                    var tags = try self.alloc.alloc([]const u8, tag_count);
                    defer self.alloc.free(tags);
                    var i: usize = 0;
                    while (i < tag_count) : (i += 1) {
                        const tag_value = try self.pop();
                        if (tag_value != .obj and tag_value.obj.data != .string)
                            return self.fail("Tag must be of type string, but found '{s}'", .{tag_value.typeName()});
                        tags[tag_count - i - 1] = tag_value.obj.data.string;
                    }
                    const id_index = self.takeInt(C.CONSTANT);
                    if (is_in_jump) continue;
                    self.is_waiting = true;
                    self.runner.onLine(self, .{
                        .content = dialogue_str,
                        .speaker = speaker,
                        .tags = tags,
                        .id = self.bytecode.uuids[id_index],
                    });
                    return;
                },
                .call => {
                    const arg_count = self.takeInt(C.ARGS);
                    const value = self.stack.items[self.stack.count - 1 - arg_count];
                    if (value != .obj)
                        return self.fail("Cannot call non-function type {s}", .{value.typeName()});
                    switch (value.obj.data) {
                        .function => |f| {
                            if (f.arity != arg_count) {
                                return self.fail(
                                    "Function expected {} arguments, but found {}",
                                    .{ f.arity, arg_count },
                                );
                            }
                            const frame = try Frame.create(value.obj, self.stack.count - arg_count);
                            self.frames.push(frame);
                            self.stack.count = frame.bp + f.locals_count;
                        },
                        .builtin => |b| {
                            if (b.arity != arg_count)
                                return self.fail(
                                    "Builtin Function expected {} arguments, but found {}",
                                    .{ b.arity, arg_count },
                                );
                            const result = b.backing(self, self.stack.items[self.stack.count - arg_count .. self.stack.count]);
                            if (self.break_on_assert and std.mem.eql(u8, b.name, "assert") and result != .void) {
                                return self.fail("Assertion Failed: {s}", .{result.asString() orelse unreachable});
                            }
                            self.stack.count -= arg_count + 1;
                            try self.push(result);
                        },
                        else => {
                            return self.fail("Cannot call non-function type '{s}'", .{value.typeName()});
                        },
                    }
                },
                .return_value => {
                    const value = try self.pop();
                    const frame = self.frames.pop();
                    self.stack.count = frame.bp - 1;
                    try self.push(value);
                },
                .return_void => {
                    const frame = self.frames.pop();
                    self.stack.count = frame.bp - 1;
                    try self.push(Void);
                },
                .fork => {
                    self.current_choices = try self.choices_list.toOwnedSlice(self.alloc);
                    self.choices_freed = false;
                    self.choices_list.clearRetainingCapacity();
                    if (is_in_jump) {
                        const next_target = self.jump_requests.getLast();
                        // If the current IP is the target, pop it.
                        if (self.currentFrame().ip >= next_target) {
                            _ = self.jump_requests.pop();
                        } else continue;
                    }

                    self.is_waiting = true;
                    self.runner.onChoices(self, self.current_choices);
                    return;
                },
                .choice => {
                    const ip = self.takeInt(C.JUMP); // The skip-jump
                    const is_unique = self.takeInt(u8) == 1;
                    const id_index = self.takeInt(C.CONSTANT);

                    const anchor_idx = self.takeInt(C.CONSTANT);
                    const anchor_val = self.bytecode.constants[anchor_idx];
                    const anchor = anchor_val.obj.data.anchor;

                    const visit_count = self.globals[anchor.visit_index].visit;

                    const content_value = try self.pop();
                    const content_str = content_value.asString() orelse return self.fail("Choice content must be of type string, but found '{s}'", .{content_value.typeName()});

                    const tag_count = self.takeInt(u8);
                    var tags = try self.alloc.alloc([]const u8, tag_count);
                    var i: usize = 0;
                    while (i < tag_count) : (i += 1) {
                        const tag_value = try self.pop();
                        if (tag_value != .obj and tag_value.obj.data != .string)
                            return self.fail("Tag must be of type string, but found '{s}'", .{tag_value.typeName()});
                        tags[tag_count - i - 1] = tag_value.obj.data.string;
                    }
                    if (visit_count > 0 and is_unique) continue;

                    try self.choices_list.append(self.alloc, .{
                        .content = content_str,
                        .tags = tags,
                        .visit_count = visit_count,
                        .ip = ip,
                        .id = self.bytecode.uuids[id_index],
                    });
                },
                .visit => {
                    const anchor_idx = self.takeInt(C.CONSTANT);
                    const value = self.bytecode.constants[anchor_idx];
                    const anchor = value.obj.data.anchor;
                    self.globals[anchor.visit_index].visit += 1;
                    try self.anchor_stack.append(self.alloc, anchor_idx);
                },
                .divert => {
                    // If in a jump, go to the next jump instead
                    if (self.jump_requests.pop()) |req| {
                        self.currentFrame().ip = req;
                        continue;
                    }
                    const anchor_idx = self.takeInt(C.CONSTANT);
                    try self.prepareDivert(anchor_idx);

                    if (self.jump_requests.pop()) |dest| {
                        const len = self.currentFrame().instructions().len;
                        if (dest > len) return self.fail("Divert {d} is out of range {d}", .{ dest, len });
                        self.currentFrame().ip = dest;
                    }
                },
                .backup => {
                    const ip = self.takeInt(C.JUMP);
                    if (is_in_jump) continue;
                    if (self.jump_backups.items.len > 0 and self.jump_backups.getLast() == ip) {
                        _ = try self.pop();
                        continue;
                    }
                    try self.jump_backups.append(self.alloc, ip);
                },
                .fin => {
                    if (self.anchor_stack.items.len > 0) {
                        _ = self.anchor_stack.pop();
                    }

                    if (self.jump_requests.pop()) |req| {
                        self.currentFrame().ip = req;
                        continue;
                    }
                    if (self.jump_backups.pop()) |backup| {
                        self.currentFrame().ip = backup;
                        continue;
                    }
                    self.end();
                    break;
                },
            }
        }
        self.end();
    }

    fn performBin(comptime T: type, op: OpCode, left: T, right: T) !T {
        return switch (op) {
            .subtract => left - right,
            .multiply => left * right,
            .divide => if (@typeInfo(T) == .int) @divExact(left, right) else left / right,
            .modulus => @mod(left, right),
            else => return error.InvalidOperation,
        };
    }

    fn prepareDivert(self: *Vm, anchor_idx: C.CONSTANT) !void {
        var current_idx: ?C.CONSTANT = anchor_idx;
        var is_target = true;
        while (current_idx) |idx| {
            // If this is an ancestor (not the target itself) and it's already active,
            // we stop climbing because the environment for the target is already set up.
            if (!is_target) {
                for (self.anchor_stack.items) |active_idx| {
                    if (active_idx == idx) return;
                }
            }

            const anchor = self.bytecode.constants[idx].obj.data.anchor;
            try self.jump_requests.append(self.alloc, anchor.ip);

            // If the target itself is already in the stack, we still want to
            // jump to its IP, but we should treat it as "leaving" and "re-entering".
            // We pop it from the anchor_stack so the visit logic treats it as a fresh arrival.
            if (is_target) {
                for (self.anchor_stack.items, 0..) |active_idx, i| {
                    if (active_idx == idx) {
                        self.anchor_stack.shrinkRetainingCapacity(i);
                        break;
                    }
                }
            }

            current_idx = anchor.parent_anchor_index;
            is_target = false;
        }
    }

    fn binaryNumberOp(self: *Vm, op: OpCode) !void {
        var right = try self.pop();
        var left = try self.pop();
        if (right == .timestamp and left == .timestamp) {
            try self.push(.{ .timestamp = performBin(i64, op, left.timestamp, right.timestamp) catch
                return self.fail("Unknown binary operator '{s}'", .{@tagName(op)}) });
            return;
        }
        if (right == .visit) {
            const value: f32 = @floatFromInt(right.visit);
            right = .{ .number = value };
        }
        if (left == .visit) {
            const value: f32 = @floatFromInt(left.visit);
            left = .{ .number = value };
        }
        if (right != .number or left != .number)
            return self.fail("Cannot perform binary operation on types of '{s}' and '{s}'", .{ left.typeName(), right.typeName() });
        const right_num = right.number;
        const left_num = left.number;
        try self.push(.{ .number = performBin(f32, op, left_num, right_num) catch
            return self.fail("Unknown binary operator '{s}'", .{@tagName(op)}) });
    }

    fn comparisonOp(self: *Vm, op: OpCode) !void {
        var right = try self.pop();
        var left = try self.pop();
        if (right == .visit) {
            const value: f32 = @floatFromInt(right.visit);
            right = .{ .number = value };
        }
        if (left == .visit) {
            const value: f32 = @floatFromInt(left.visit);
            left = .{ .number = value };
        }

        if (@intFromEnum(right) != @intFromEnum(left)) {
            return self.fail("Cannot compare mismatched types '{s}' and '{s}': {f} {f}", .{ left.typeName(), right.typeName(), left, right });
        }
        if (right == .enum_value) {
            const value: f32 = @floatFromInt(right.enum_value.index);
            right = .{ .number = value };
        }
        if (left == .enum_value) {
            const value: f32 = @floatFromInt(left.enum_value.index);
            left = .{ .number = value };
        }
        switch (op) {
            .equal => try self.push(.{ .bool = right.eql(left) }),
            .not_equal => try self.push(.{ .bool = !right.eql(left) }),
            .greater_than => try self.push(.{ .bool = left.number > right.number }),
            .greater_than_equal => try self.push(.{ .bool = left.number >= right.number }),
            else => return self.fail("Unknown comparison operator '{s}'", .{@tagName(op)}),
        }
    }

    fn prongIsMatch(self: *Vm, capture: Value, case: Value) bool {
        _ = self;
        const result = switch (case) {
            .range => |r| @as(i32, @intFromFloat(capture.number)) >= r.start and @as(i32, @intFromFloat(capture.number)) <= r.end,
            else => capture.eql(case),
        };
        return result;
    }

    fn clone(self: *Vm, value: Value) !Value {
        if (value != .obj) return value;

        const allocator = self.alloc;
        const obj = value.obj;
        switch (obj.data) {
            .list => |l| {
                var new_list = try std.ArrayList(Value).initCapacity(allocator, l.items.len);
                for (l.items) |item| {
                    try new_list.append(allocator, try self.clone(item));
                }
                return try self.gc.create(self, .{ .list = new_list });
            },
            .map => |m| {
                var new_map = Value.Obj.MapType.empty;
                var it = m.iterator();
                while (it.next()) |entry| {
                    try new_map.put(allocator, try self.clone(entry.key_ptr.*), try self.clone(entry.value_ptr.*));
                }
                return try self.gc.create(self, .{ .map = new_map });
            },
            .set => |s| {
                var new_set = Value.Obj.SetType.empty;
                var it = s.iterator();
                while (it.next()) |entry| {
                    try new_set.put(allocator, try self.clone(entry.key_ptr.*), {});
                }
                return try self.gc.create(self, .{ .set = new_set });
            },
            .instance => |inst| {
                var values = try self.alloc.alloc(Value, inst.fields.len);
                for (inst.fields, 0..) |inst_value, i| {
                    values[i] = try self.clone(inst_value);
                }
                return try self.gc.create(self, .{ .instance = .{
                    .base = inst.base,
                    .fields = values,
                } });
            },
            else => return value,
        }
    }

    fn createInstance(self: *Vm, base: *Value.Obj, inst_fields: []Class.Member) !Class.Instance {
        const class = base.data.class;
        var values = try self.alloc.alloc(Value, class.fields.len);
        for (class.fields, 0..) |f, i| values[i] = try self.clone(f.value);
        for (inst_fields) |inst_field| {
            if (class.getFieldIndex(inst_field.name)) |i| values[i] = inst_field.value;
        }
        return .{
            .base = base,
            .fields = values,
        };
    }

    fn push(self: *Vm, value: Value) !void {
        if (self.stack.items.len >= stack_size) return self.fail("Stack is full", .{});
        self.stack.push(value);
    }

    fn pushAlloc(self: *Vm, data: Value.Obj.Data) !void {
        const value = try self.gc.create(self, data);
        self.stack.push(value);
    }

    fn pop(self: *Vm) !Value {
        if (self.stack.count == 0) {
            self.fail("Trying to pop an empty stack", .{}) catch |err| return err;
            return Nil;
        }
        return self.stack.pop();
    }

    pub fn print(self: *Vm, writer: *std.Io.Writer) void {
        writer.print("==STACK==\n", .{});
        for (self.stack.items) |*item| {
            writer.print("[", .{});
            item.print(writer);
            writer.print("]\n", .{});
        }
        writer.print("\n", .{});
    }
};
