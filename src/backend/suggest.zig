const std = @import("std");

/// Case-insensitive Levenshtein distance between two strings.
fn distance(allocator: std.mem.Allocator, a: []const u8, b: []const u8) !usize {
    if (a.len == 0) return b.len;
    if (b.len == 0) return a.len;

    const row_len = b.len + 1;
    var prev = try allocator.alloc(usize, row_len);
    defer allocator.free(prev);
    var curr = try allocator.alloc(usize, row_len);
    defer allocator.free(curr);

    for (prev, 0..) |*v, i| v.* = i;

    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        curr[0] = i + 1;
        const ac = std.ascii.toLower(a[i]);
        var j: usize = 0;
        while (j < b.len) : (j += 1) {
            const bc = std.ascii.toLower(b[j]);
            const cost: usize = if (ac == bc) 0 else 1;
            const del = prev[j + 1] + 1;
            const ins = curr[j] + 1;
            const sub = prev[j] + cost;
            curr[j + 1] = @min(@min(del, ins), sub);
        }
        const tmp = prev;
        prev = curr;
        curr = tmp;
    }
    return prev[b.len];
}

/// Find the closest match in `haystack` to `needle`. Returns null when nothing
/// is close enough (threshold scaled by needle length). The returned slice is
/// a fresh dupe owned by the caller.
pub fn closest(
    allocator: std.mem.Allocator,
    needle: []const u8,
    haystack: []const []const u8,
) !?[]const u8 {
    if (needle.len == 0 or haystack.len == 0) return null;

    // Generous-but-bounded threshold: at most a third of the name, at least 1,
    // but never more than 3. Keeps suggestions relevant.
    const threshold: usize = @min(@max(needle.len / 3, 1), 3);

    var best: ?[]const u8 = null;
    var best_dist: usize = threshold + 1;
    for (haystack) |candidate| {
        // Skip identical matches (common when name was added to haystack in error path).
        if (candidate.len == needle.len and std.ascii.eqlIgnoreCase(candidate, needle)) continue;
        // Skip wildly different lengths early.
        const len_diff = if (candidate.len > needle.len) candidate.len - needle.len else needle.len - candidate.len;
        if (len_diff > threshold) continue;
        const d = try distance(allocator, needle, candidate);
        if (d <= threshold and d < best_dist) {
            best_dist = d;
            best = candidate;
        }
    }

    if (best) |b| return try allocator.dupe(u8, b);
    return null;
}

test "closest finds near match" {
    const allocator = std.testing.allocator;
    const names = [_][]const u8{ "INTRO", "KITCHEN", "MARKET" };
    const got = try closest(allocator, "INTO", &names);
    try std.testing.expect(got != null);
    defer allocator.free(got.?);
    try std.testing.expectEqualStrings("INTRO", got.?);
}

test "closest returns null when nothing close" {
    const allocator = std.testing.allocator;
    const names = [_][]const u8{ "apple", "banana" };
    const got = try closest(allocator, "xyzzy", &names);
    try std.testing.expect(got == null);
}

test "closest ignores exact matches" {
    const allocator = std.testing.allocator;
    const names = [_][]const u8{ "FOO", "FOO2" };
    const got = try closest(allocator, "FOO", &names);
    try std.testing.expect(got != null);
    defer allocator.free(got.?);
    try std.testing.expectEqualStrings("FOO2", got.?);
}
