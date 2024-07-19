pub const CONSTANT = u32;
pub const GLOBAL = u32;
pub const JUMP = u32;
pub const LOCAL = u16;
pub const COLLECTION = u16;
pub const ARGS = u8;
pub const FIELDS = u8;
pub const FREE = u8;
pub const BUILTIN = u8;

// .string, .closure, .prong // CONSTANT index, ARGS count
// .choice => // JUMP choice dest, JUMP visit id, JUMP jump dest, ARGS tag count
