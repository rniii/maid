const std = @import("std");
const Parser = @import("parser.zig");

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const stdin = try std.fs.cwd().openFile("README.md", .{});

    var parser = try Parser.init(alloc);
    parser.parse(stdin.reader());
}
