//! IR-driven codegen tests + A/B parity harness against the AST compiler.
//!
//! `compileBoth` runs both pipelines on the same source and returns both
//! Bytecode values. `expectByteParity` slice-equality checks instructions,
//! constants, and global_symbols. As statement support lands in
//! codegen.zig, more cases get added below.

const std = @import("std");

const topi = @import("topi");
const ir = topi.ir;
const backend = topi.backend;
const Bytecode = backend.Bytecode;
const Compiler = backend.Compiler;
const Codegen = backend.Codegen;
const OpCode = backend.OpCode;
const Value = topi.types.Value;
const builtins = topi.runtime.builtins;
const Module = topi.module.Module;
const File = topi.module.File;

const testing = std.testing;
const allocator = testing.allocator;
const cl = builtins.functions.values().len;

fn newModuleWithSource(source: []const u8) !*Module {
    const mod = try Module.initEmpty(allocator, std.testing.io);
    errdefer mod.deinit();
    const file = try mod.arena.allocator().create(File);
    file.* = .{
        .path = "",
        .name = "",
        .dir_name = "",
        .source = source,
        .module = mod,
    };
    mod.entry = file;
    try mod.includes.putNoClobber(allocator, file.path, file);
    return mod;
}

/// Run the IR pipeline directly (without the module-level flag) so tests
/// can A/B compare against the AST compiler.
fn compileViaIr(mod: *Module) !Bytecode {
    try mod.entry.loadSource();
    try mod.entry.buildTree();
    var program = try ir.lower(allocator, mod);
    defer program.deinit();
    return try Codegen.emit(allocator, mod, &program);
}

fn compileViaAst(mod: *Module) !Bytecode {
    try mod.entry.loadSource();
    try mod.entry.buildTree();
    var compiler = try Compiler.init(allocator, mod);
    defer compiler.deinit();
    try compiler.compile();
    return try compiler.bytecode();
}

const Pair = struct {
    ast: Bytecode,
    ir: Bytecode,

    fn deinit(self: *Pair) void {
        self.ast.free(allocator);
        self.ir.free(allocator);
    }
};

/// Compile `source` through both pipelines on independent modules.
fn compileBoth(source: []const u8) !Pair {
    var ast_mod = try newModuleWithSource(source);
    defer ast_mod.deinit();
    const ast_bc = try compileViaAst(ast_mod);
    errdefer ast_bc.free(allocator);

    var ir_mod = try newModuleWithSource(source);
    defer ir_mod.deinit();
    const ir_bc = try compileViaIr(ir_mod);

    return .{ .ast = ast_bc, .ir = ir_bc };
}

fn expectByteParity(pair: Pair) !void {
    try testing.expectEqualSlices(u8, pair.ast.instructions, pair.ir.instructions);
    try testing.expectEqual(pair.ast.constants.len, pair.ir.constants.len);
    try testing.expectEqual(pair.ast.global_symbols.len, pair.ir.global_symbols.len);
    for (pair.ast.global_symbols, pair.ir.global_symbols) |a, b| {
        try testing.expectEqualStrings(a.name, b.name);
        try testing.expectEqual(a.index, b.index);
        try testing.expectEqual(a.is_mutable, b.is_mutable);
        try testing.expectEqualSlices(u8, &a.uuid, &b.uuid);
    }
    try testing.expectEqual(pair.ast.locals_count, pair.ir.locals_count);
}

test "Codegen empty program" {
    var mod = try newModuleWithSource("");
    defer mod.deinit();
    var bc = try compileViaIr(mod);
    defer bc.free(allocator);

    // Builtins always populate the head of the constants pool.
    try testing.expectEqual(cl, bc.constants.len);
    try testing.expectEqual(@as(usize, 0), bc.instructions.len);
    try testing.expectEqual(@as(usize, 0), bc.global_symbols.len);
    try testing.expectEqual(@as(usize, 0), bc.locals_count);
}

test "Codegen empty program parity" {
    var pair = try compileBoth("");
    defer pair.deinit();
    try expectByteParity(pair);
}

test "Codegen expressions parity" {
    const cases = [_][]const u8{
        "1 + 2",
        "1 - 2",
        "3 * 4",
        "10 / 2",
        "5 % 2",
        "1 == 2",
        "1 != 2",
        "1 < 2",
        "1 <= 2",
        "1 > 2",
        "1 >= 2",
        "true and false",
        "true or false",
        "!true",
        "-5",
        "true",
        "false",
        "0..10",
    };
    for (cases) |src| {
        errdefer std.log.warn("case: {s}", .{src});
        var pair = try compileBoth(src);
        defer pair.deinit();
        try expectByteParity(pair);
    }
}
