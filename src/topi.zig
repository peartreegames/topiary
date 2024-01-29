const vm = @import("vm.zig");
const runner = @import("runner.zig");
const compiler = @import("compiler.zig");
const compilerError = @import("compiler-error.zig");

pub const values = @import("values.zig");

/// Virtual Machine to execture Bytecode
pub const Vm = vm.Vm;
pub const RuntimeErr = vm.RuntimeErr;

/// Convert source text to bytecode
pub const Compiler = compiler.Compiler;
/// Convinence method to convert source text to bytecode
pub const compileSource = compiler.compileSource;
pub const CompilerErr = compilerError.CompilerErr;
pub const CompilerErrors = compilerError.CompilerErrors;

/// Bytecode
pub const Bytecode = @import("bytecode.zig").Bytecode;
pub const StateMap = @import("state.zig").StateMap;

/// Runner interface to hook into the Vm
pub const Runner = runner.Runner;
/// Dialogue struct
pub const Dialogue = runner.Dialogue;
/// Choice struct
pub const Choice = runner.Choice;
/// Topi Value
pub const Value = values.Value;
/// Topi Enum
pub const Enum = @import("enum.zig").Enum;
/// Topi Class
pub const Class = @import("class.zig").Class;

/// Topi Symbol
pub const Symbol = @import("scope.zig").Symbol;
/// Abstract Syntax Tree
pub const ast = @import("ast.zig");
/// Parser to convert text to ast
pub const parser = @import("parser.zig");