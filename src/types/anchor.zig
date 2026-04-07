const utils = @import("../utils/index.zig");
const C = utils.C;
const UUID = utils.UUID;

pub const Anchor = struct {
    name: []const u8,
    uuid: UUID.ID,
    visit_index: C.GLOBAL,
    parent_anchor_index: ?C.CONSTANT,
    ip: C.JUMP,
};
