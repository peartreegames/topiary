const program = @import("program.zig");

pub const Loc = program.Loc;
pub const Slot = program.Slot;
pub const AnchorRef = program.AnchorRef;
pub const Program = program.Program;

pub const Stmt = program.Stmt;
pub const Line = program.Line;
pub const Choice = program.Choice;
pub const Fork = program.Fork;
pub const Divert = program.Divert;
pub const Bough = program.Bough;
pub const Visit = program.Visit;
pub const Block = program.Block;
pub const IfStmt = program.IfStmt;
pub const WhileStmt = program.WhileStmt;
pub const ForStmt = program.ForStmt;
pub const SwitchStmt = program.SwitchStmt;
pub const Prong = program.Prong;
pub const FunctionDecl = program.FunctionDecl;
pub const ClassDecl = program.ClassDecl;
pub const EnumDecl = program.EnumDecl;
pub const Parameter = program.Parameter;
pub const VarDecl = program.VarDecl;
pub const Include = program.Include;
pub const Tag = program.Tag;

pub const Scope = @import("scope.zig").Scope;
pub const Symbol = @import("scope.zig").Symbol;

pub const TextSegment = program.TextSegment;

pub const ExprRef = program.ExprRef;
pub const Expr = program.Expr;
pub const LoadConst = program.LoadConst;
pub const MapPair = program.MapPair;
pub const Range = program.Range;
pub const BinOp = program.BinOp;
pub const UnOp = program.UnOp;
pub const IfExpr = program.IfExpr;
pub const Index = program.Index;
pub const Call = program.Call;
pub const Instance = program.Instance;
pub const BuiltinExpr = program.BuiltinExpr;

pub const dump = @import("dump.zig").dump;
pub const lower = @import("lower.zig").lower;

test {
    _ = @import("dump.zig");
    _ = @import("lower.zig");
}
