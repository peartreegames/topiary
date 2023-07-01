const Obj = @import("./values.zig").Value.Obj;

pub const Frame = struct {
    fp: *Obj,
    ip: usize,
    bp: usize,

    pub fn create(obj: *Obj, ip: usize, bp: usize) !Frame {
        if (obj.data != .function) return error.InvalidType;
        return .{
            .fp = obj,
            .ip = ip,
            .bp = bp,
        };
    }

    pub fn instructions(self: *Frame) []const u8 {
        return self.fp.data.function.instructions;
    }
};
