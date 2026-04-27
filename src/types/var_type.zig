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
};
