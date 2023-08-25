const std = @import("std");
const traitor = @import("traitor.zig");

const GraphTrait = struct {
    pub const __traitor_trait_name = 0;

    pub const Directed = false;

    n: usize,

    pub fn hasEdge(i: usize, j: usize) bool {
        _ = j;
        _ = i;
        unreachable;
    }

    pub fn hasntEdge(i: usize, j: usize) bool {
        _ = j;
        _ = i;
        unreachable;
    }
};

pub fn aStar(graph: anytype, start: usize, end: usize) !void {
    _ = end;
    _ = start;
    comptime traitor.checkTrait(GraphTrait, @TypeOf(graph));

    std.debug.print("Is graph directed? {s}\n", .{if (@TypeOf(graph).Directed) "yes" else "no"});
}

const MyGraph = struct {
    pub const Directed = 5;

    n: usize = 0,

    pub fn hasEdge(i: usize, j: usize) bool {
        return (i + j) % 2 == 0;
    }
};

test {
    const graph: MyGraph = .{};

    _ = try aStar(graph, 0, 0);
}
