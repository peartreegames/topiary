const Obj = @import("./values.zig").Value.Obj;

pub const Frame = struct {
    frame_pointer: *Obj,
    ip: usize,

    pub fn create(obj: *Obj, ip: usize) !Frame {
        if (obj.*.data != .function) return error.InvalidType;
        return .{
            .frame_pointer = obj,
            .ip = ip,
        };
    }

    pub fn instructions(self: *Frame) []const u8 {
        return self.frame_pointer.*.data.function;
    }
};
