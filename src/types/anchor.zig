const utils = @import("../utils/index.zig");
const C = utils.C;

pub const Anchor = struct {
    name: []const u8,
    visit_index: C.GLOBAL,
    parent_anchor_index: ?C.CONSTANT,
    ip: C.JUMP,
};
