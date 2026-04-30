//! Topiary IR — middle representation between AST and bytecode.
//!
//! The IR is intentionally close to the AST in shape, but resolves three
//! things that the AST leaves syntactic:
//!
//!   1. Names → Slots. Every load/store carries the resolved slot index
//!      and tag, so a downstream emitter never re-walks scopes.
//!   2. Anchors → AnchorRef. Diverts, forks, choices, visits, and boughs
//!      carry the fully-qualified path and the stamped UUID. The IR
//!      deliberately does not carry constant-pool indices — those are a
//!      codegen concern, computed when the compiler walks the IR.
//!   3. Interpolated strings → []TextSegment. The AST's "{0}"/"{1}"
//!      placeholder markers are gone; segments are explicit literal
//!      and interpolation runs.
//!
//! Other lowering choices the IR bakes in:
//!
//!   - `fork^` and `=>^` are tag-distinct from `fork` and `=>`. Backup
//!     stack semantics are not a flag — Zig's exhaustive switch checker
//!     forces every consumer to acknowledge the backup case.
//!   - `Visit` is a first-class statement. Today the visit-count
//!     increment is implicit in choice/bough/fork compilation; in IR
//!     it is a node you can see, count, and reorder.
//!   - `Bough` survives lowering as a distinct node (writers' entry
//!     points), not a generic anchored block.
//!
//! What the IR does NOT do:
//!   - No type checking. `VarType` is preserved as a hint only.
//!   - No constant folding, dead-code elimination, or inlining.
//!   - No diagnostics emission. `Loc` is exposed; emitting a CompilerErr
//!     is the consumer's job.
//!
//! Memory: a `Program` owns an arena. All IR nodes, segment slices,
//! anchor paths, and the anchors hash map live in that arena. `deinit`
//! frees the whole graph at once. Identifier name slices may borrow
//! from the parser's arena — that contract belongs to the lowering pass
//! once it exists.

const std = @import("std");

const Token = @import("../frontend/token.zig").Token;
const ast = @import("../frontend/ast.zig");
const utils = @import("../utils/index.zig");
const C = utils.C;
const UUID = utils.UUID;
const VarType = @import("../types/var_type.zig").VarType;

// ===========================================================================
// Source locations
// ===========================================================================

/// Source range for an IR node. `start` is always present; `end` is set
/// for compound nodes (boughs, blocks, control flow) where diagnostics
/// want to underline the whole span.
pub const Loc = struct {
    file_index: usize,
    start: Token,
    end: ?Token = null,

    pub fn fromToken(tok: Token) Loc {
        return .{ .file_index = tok.file_index, .start = tok };
    }

    pub fn span(start: Token, end: Token) Loc {
        return .{ .file_index = start.file_index, .start = start, .end = end };
    }
};

// ===========================================================================
// Slots — resolved symbol references
// ===========================================================================

/// A resolved variable reference. Mirrors `scope.Symbol.tag`. Names that
/// resolve to constant-pool entries (boughs, classes, enums, top-level
/// functions) are not represented here — those flow through `AnchorRef`
/// or `Expr.load_const` instead.
///
/// scope.zig's `function` tag collapses into `local` here — both live on
/// the call frame stack and use the same get_local/set_local opcodes.
///
/// `Slot` doubles as the payload for `Expr.Kind.load`: every load is a
/// reference to a definition site, and the descriptor needed at the use
/// site (kind, index, var_type, mutability) is the same one stored at
/// the declaration. `name` is set when borrowing from a parser-arena
/// identifier; declaration sites synthesize one as needed.
pub const Slot = union(enum) {
    /// Stack-frame local. Includes function parameters and the function's
    /// own name (callable recursively).
    local: Local,
    /// Captured by an inner function. `depth` is the number of function
    /// scopes between the use site and the definition site.
    upvalue: Upvalue,
    /// Program-level global.
    global: Global,

    pub const Local = struct {
        index: C.LOCAL,
        is_mutable: bool,
        var_type: VarType = .unknown,
        /// Borrowed from the parser arena (or duped into IR arena for
        /// loads). Used for diagnostics and dumps.
        name: []const u8 = "",
    };

    pub const Upvalue = struct {
        index: C.LOCAL,
        depth: u8,
        is_mutable: bool,
        var_type: VarType = .unknown,
        name: []const u8 = "",
    };

    pub const Global = struct {
        index: C.GLOBAL,
        is_mutable: bool,
        var_type: VarType = .unknown,
        name: []const u8 = "",
    };
};

// ===========================================================================
// Anchors
// ===========================================================================

/// A resolved anchor reference. Used by Divert, Fork, Choice, Visit,
/// Bough, Instance, and LoadConst targets. Lowering produces these with
/// the fully-qualified path; codegen looks the path up in `Program.anchors`
/// to materialize a constant-pool index.
///
/// `kind` is filled in eagerly for backward references (the anchor was
/// declared before this reference) and patched at the end of lowering
/// for forward references. After `lower()` returns successfully, every
/// reachable AnchorRef has `kind != null`.
pub const AnchorRef = struct {
    kind: ?Kind = null,
    /// Fully-qualified path ("Story.Chapter.choice_3"). Owned by the IR
    /// arena.
    path: []const u8,
    /// Stamped UUID, stable across recompiles. `UUID.Empty` for fully
    /// anonymous anchors (today, only synthesized choice anchors).
    uuid: UUID.ID = UUID.Empty,

    pub const Kind = enum {
        bough,
        fork,
        choice,
        function,
        extern_function,
        class,
        @"enum",
    };
};

// ===========================================================================
// Program
// ===========================================================================

/// The IR for one compiled topi program (one entry file plus its includes).
/// Owns its own arena; deinit frees the whole graph.
pub const Program = struct {
    arena: std.heap.ArenaAllocator,

    /// Number of slots reserved in the global frame. The runtime
    /// allocates `globals[globals_count]` before executing.
    globals_count: C.GLOBAL = 0,

    /// High-water mark of locals reachable from top-level execution
    /// (boughs / top-level forks / top-level if/block bodies / etc.).
    /// Codegen writes this into `Bytecode.locals_count` for the
    /// runtime to size the main frame's stack window.
    top_level_locals_count: u32 = 0,

    /// Top-level statements: typically VarDecl, Function, Class, Enum,
    /// Bough — but any Stmt is legal here.
    body: []const Stmt = &.{},

    /// Index of every named anchor in the program, keyed by full path.
    /// Mirrors the compiler's `constants_map`, but only contains
    /// anchor-typed entries — for fast IR-walking lookups.
    anchors: std.array_hash_map.String(AnchorRef) = .empty,

    /// Source files this program spans (file_index → path). Populated by
    /// lowering from the parser's include resolution.
    files: []const []const u8 = &.{},

    pub fn init(parent_allocator: std.mem.Allocator) Program {
        return .{ .arena = std.heap.ArenaAllocator.init(parent_allocator) };
    }

    pub fn deinit(self: *Program) void {
        self.arena.deinit();
    }

    pub fn allocator(self: *Program) std.mem.Allocator {
        return self.arena.allocator();
    }
};

// ===========================================================================
// Statements
// ===========================================================================

pub const Stmt = struct {
    loc: Loc,
    kind: Kind,

    pub const Kind = union(enum) {
        // ---- Narrative ----------------------------------------------------

        line: Line,
        choice: Choice,
        /// Non-backup fork.
        fork: Fork,
        /// `fork^` — pushes a return marker on the backup stack so any
        /// `=>^` inside (or end-of-fork) unwinds back here.
        backup_fork: Fork,
        /// `=> path` — non-backup direct jump.
        divert: Divert,
        /// `=>^ path` — pushes the target on the backup stack so a later
        /// fin/end-of-fork unwinds back here.
        backup_divert: Divert,
        bough: Bough,
        /// Explicit visit-count increment. Lowering emits one of these
        /// at the entry of each Bough and Choice body.
        visit: Visit,
        /// Class declaration with field initializers and method bodies.
        class: ClassDecl,
        /// Enum declaration.
        enum_decl: EnumDecl,

        // ---- Imperative ---------------------------------------------------

        block: Block,
        @"if": IfStmt,
        @"while": WhileStmt,
        @"for": ForStmt,
        @"switch": SwitchStmt,
        function: FunctionDecl,
        return_value: ExprRef,
        return_void,
        var_decl: VarDecl,
        @"break",
        @"continue",
        fin,
        expr_stmt: ExprRef,
        include: Include,
    };
};

pub const Line = struct {
    /// Stamped during semantic analysis; stable across recompiles.
    uuid: UUID.ID,
    /// Optional speaker (`Speaker: "..."`). Borrowed from the AST arena.
    speaker: ?[]const u8 = null,
    /// Lowered interpolation. `Literal` segments hold their text;
    /// `Interp` segments hold the IR for the `{...}` expression.
    segments: []const TextSegment = &.{},
    /// `#tag` annotations. Borrowed names.
    tags: []const Tag = &.{},
};

pub const Choice = struct {
    uuid: UUID.ID,
    /// Optional explicit name (`* [Name] "..."`). When null, lowering
    /// synthesizes one from the UUID.
    name: ?[]const u8 = null,
    /// The choice's prompt text.
    segments: []const TextSegment = &.{},
    is_unique: bool = false,
    body: []const Stmt = &.{},
    tags: []const Tag = &.{},
    /// Resolved when the bough containing this choice is lowered; the
    /// underlying anchor's `.ip` field is patched at codegen time.
    anchor: AnchorRef,
};

pub const Fork = struct {
    uuid: UUID.ID,
    /// Optional fork name (qualified-path-resolvable). Anonymous forks
    /// are addressed by UUID only.
    name: ?[]const u8 = null,
    body: []const Stmt = &.{},
    anchor: AnchorRef,
};

pub const Divert = struct {
    /// Resolved target. `target.path` holds the writer's "Foo.Bar" form
    /// for diagnostics.
    target: AnchorRef,
};

pub const Bough = struct {
    uuid: UUID.ID,
    /// Unqualified name as written. Use `anchor.path` for the qualified
    /// form ("Story.Chapter.intro").
    name: []const u8,
    body: []const Stmt = &.{},
    anchor: AnchorRef,
};

pub const Visit = struct {
    target: AnchorRef,
};

pub const Block = struct {
    body: []const Stmt = &.{},
};

pub const IfStmt = struct {
    condition: ExprRef,
    then_branch: []const Stmt = &.{},
    else_branch: ?[]const Stmt = null,
};

pub const WhileStmt = struct {
    condition: ExprRef,
    body: []const Stmt = &.{},
};

pub const ForStmt = struct {
    /// User-facing capture binding (`for x in xs |x|`). The iterator
    /// itself is opaque state pushed by the bytecode; the writer-visible
    /// per-iteration value lives in this slot.
    capture_slot: Slot,
    capture_name: []const u8,
    iterator: ExprRef,
    body: []const Stmt = &.{},
};

pub const SwitchStmt = struct {
    capture: ExprRef,
    prongs: []const Prong = &.{},
};

pub const Prong = struct {
    loc: Loc,
    /// `null` = default (`else =>`) prong.
    values: ?[]const ExprRef = null,
    body: []const Stmt = &.{},
};

pub const FunctionDecl = struct {
    name: []const u8,
    /// Set when the declaration binds a stack-local (a nested function).
    /// `null` when the function is top-level or a class method — the
    /// function is reachable through `Program.anchors[anchor.path]`.
    name_slot: ?Slot = null,
    /// Set when this declaration is a method on a class. The class is
    /// reachable through the surrounding `ClassDecl.anchor`.
    anchor: ?AnchorRef = null,
    is_method: bool = false,
    is_extern: bool = false,
    parameters: []const Parameter = &.{},
    body: []const Stmt = &.{},
    /// High-water mark of locals (parameters + var_decls + nested local
    /// scopes) needed to allocate this function's stack frame at runtime.
    /// Populated by lowering — codegen emits this directly into the
    /// produced `Function` object.
    locals_count: u32 = 0,
};

pub const ClassDecl = struct {
    name: []const u8,
    anchor: AnchorRef,
    /// Field names declared in source order. Codegen uses this order to
    /// align with class instance construction.
    field_names: []const []const u8 = &.{},
    /// Field initializers, parallel to `field_names`.
    field_initializers: []const ExprRef = &.{},
    /// Methods declared inside the class body. Each method's `anchor`
    /// is set to a path under this class's path.
    methods: []const FunctionDecl = &.{},
};

pub const EnumDecl = struct {
    name: []const u8,
    anchor: AnchorRef,
    is_seq: bool = false,
    values: []const []const u8 = &.{},
};

pub const Parameter = struct {
    name: []const u8,
    slot: Slot,
    var_type: VarType = .unknown,
};

pub const VarDecl = struct {
    name: []const u8,
    slot: Slot,
    is_mutable: bool,
    initializer: ExprRef,
};

pub const Include = struct {
    /// Resolved absolute path of the included file.
    resolved_path: []const u8,
};

pub const Tag = struct {
    name: []const u8,
    loc: Loc,
};

// ===========================================================================
// Text segments — lowered form of interpolated strings
// ===========================================================================

pub const TextSegment = union(enum) {
    /// Verbatim text, owned by the IR arena (lowering copies out of the
    /// AST's `{0}`-marked `value` and slices on placeholder boundaries).
    literal: []const u8,
    /// `{ expr }` placeholder. The expression is fully lowered.
    interp: ExprRef,
};

// ===========================================================================
// Expressions
// ===========================================================================

/// Pointer indirection lets statements and other expressions refer to
/// expressions without flattening the union. ExprRefs are arena-allocated.
pub const ExprRef = *const Expr;

pub const Expr = struct {
    loc: Loc,
    /// Best-effort type info. `unknown` means lowering had no signal;
    /// later passes may refine.
    var_type: VarType = .unknown,
    kind: Kind,

    pub const Kind = union(enum) {
        // ---- Loads --------------------------------------------------------

        /// Reference to a definition site (local/upvalue/global). The
        /// `Slot` descriptor doubles for both declaration and use, so
        /// every metadata field a walker needs (kind, index, var_type,
        /// is_mutable, name) is right here.
        load: Slot,
        load_const: LoadConst,

        // ---- Literals -----------------------------------------------------

        number: f32,
        bool: bool,
        nil,
        /// Interpolated text used as an expression (writer wrote a string
        /// in expression position).
        text: []const TextSegment,
        list: []const ExprRef,
        set: []const ExprRef,
        map: []const MapPair,
        range: Range,

        // ---- Operators ----------------------------------------------------

        bin_op: BinOp,
        un_op: UnOp,
        if_expr: IfExpr,

        // ---- Access / call ------------------------------------------------

        index: Index,
        field: Field,
        call: Call,
        instance: Instance,

        // ---- Builtins (placeholder; not yet parsed by AST) ----------------

        /// `cycle(...)`, `shuffle(...)`, `sequence(...)`, `random(...)`.
        /// AST does not yet produce these; once it does, lowering will
        /// detect them in `Call` position and rewrite to this node so
        /// downstream passes don't pattern-match on names.
        builtin: BuiltinExpr,

        @"extern",
    };
};

pub const LoadConst = struct {
    /// Anchor target — fully-qualified path. Codegen resolves this to
    /// a constant-pool index by looking up `Program.anchors[target.path]`.
    target: AnchorRef,
};

pub const MapPair = struct {
    key: ExprRef,
    value: ExprRef,
};

pub const Range = struct {
    /// Lower bound (inclusive).
    left: ExprRef,
    /// Upper bound (inclusive — matches AST `..` semantics).
    right: ExprRef,
};

pub const BinOp = struct {
    op: ast.BinaryOp,
    left: ExprRef,
    right: ExprRef,
    /// Set when the op is one of the `assign*` family and the left side
    /// is a resolvable lvalue. Lowering preserves the original op so
    /// tooling can still see it; codegen reads the slot directly.
    target_slot: ?Slot = null,
};

pub const UnOp = struct {
    op: ast.UnaryOp,
    operand: ExprRef,
};

pub const IfExpr = struct {
    condition: ExprRef,
    then_value: ExprRef,
    else_value: ExprRef,
};

pub const Index = struct {
    target: ExprRef,
    index: ExprRef,
};

/// Dot-access (`obj.field`). The field name is a borrowed string —
/// not a symbol/anchor — so lowering does not run name resolution on
/// it. Borrows from the parser arena, like identifier names elsewhere
/// in the IR.
pub const Field = struct {
    target: ExprRef,
    name: []const u8,
};

pub const Call = struct {
    target: ExprRef,
    arguments: []const ExprRef = &.{},
};

pub const Instance = struct {
    /// Resolved class constant.
    class: AnchorRef,
    field_names: []const []const u8 = &.{},
    fields: []const ExprRef = &.{},
};

pub const BuiltinExpr = struct {
    pub const Kind = enum { cycle, shuffle, sequence, random };
    kind: Kind,
    arguments: []const ExprRef = &.{},
    /// `cycle` and `sequence` need a hidden global slot for the cursor.
    /// `null` for `shuffle` and `random`.
    cursor_slot: ?Slot = null,
};
