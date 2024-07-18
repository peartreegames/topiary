pub const CONSTANT = u32;
pub const GLOBAL = u32;
pub const JUMP = u32;
pub const LOCAL = u16;
pub const COLLECTION = u16;
pub const ARGS = u8;
pub const FIELDS = u8;
pub const FREE = u8;
pub const BUILTIN = u8;

// pub fn Size(comptime self: OpCode) type {
//         // kept separate to easily change if needed
//         return switch (self) {
//             .constant, .decl_global, .get_global, .set_global => u32,
//             .jump, .jump_if_false, .divert, .backup, .visit => u32,
//             .get_local,
//             .set_local,
//             => u16,
//             .list, .map, .set => u16,
//             .get_builtin,
//             .call,
//             .class,
//             .instance,
//             .get_free,
//             .set_free,
//             => u8,
//             .string, .closure, .prong => u40, // u32 for constant, u8 for expressions count
//             .choice => u100, // 32 for choice dest, 32 for visit id, 32 for jump dest, u8 for tag count
//             else => void,
//         };
