/// Compile-time semantic type hint carried on symbols and IR nodes.
///
/// `unknown` means analysis had no signal — consumers must treat it as
/// "could be anything." This is a hint, not a proof: there is no type
/// checker that enforces these tags match the values that flow at runtime.
pub const VarType = union(enum) {
    unknown,
    number,
    boolean,
    nil,
    string,
    list,
    set,
    map,
    instance: []const u8,
    /// Identifier resolves to an enum declaration. Payload is the
    /// anchor path (used as the `enum_by_path` lookup key).
    enum_type: []const u8,
    /// Identifier resolves to a class declaration. Payload is the
    /// anchor path (used as the `class_by_path` lookup key).
    class_type: []const u8,
    /// Identifier resolves to a top-level function (or extern fn).
    /// Diagnostic-only: function values cannot be stored because
    /// state serialization skips them (see `runtime/state.zig`).
    function_type,
};
