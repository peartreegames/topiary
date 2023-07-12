const std = @import("std");
const Obj = @import("./values.zig").Value.Obj;

pub const Frame = struct {
    cl: *Obj,
    ip: usize,
    bp: usize,

    pub fn create(obj: *Obj, ip: usize, bp: usize) !Frame {
        if (obj.data != .closure) {
            return error.InvalidType;
        }
        return .{
            .cl = obj,
            .ip = ip,
            .bp = bp,
        };
    }

    pub fn instructions(self: *Frame) []const u8 {
        return self.cl.data.closure.data.function.instructions;
    }
};
