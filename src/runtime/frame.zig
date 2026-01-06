const std = @import("std");
const Obj = @import("../types/index.zig").Value.Obj;

pub const Frame = struct {
    func: *Obj,
    ip: usize,
    bp: usize,

    pub fn create(obj: *Obj, bp: usize) !Frame {
        if (obj.data != .function) {
            return error.InvalidType;
        }
        return .{
            .func = obj,
            .ip = 0,
            .bp = bp,
        };
    }

    pub fn instructions(self: *Frame) []const u8 {
        return self.func.data.function.instructions;
    }
};
