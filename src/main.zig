pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const stdin = try std.fs.cwd().openFile("README.md", .{});

    var parser = try Parser.init(alloc);
    const tasks = try parser.parse(alloc, stdin.reader());

    for (tasks.items) |task| {
        std.debug.print("{s}\n", .{task.name});
        std.debug.print("  {s}\n\n", .{task.desc});
    }
}

const std = @import("std");
const Parser = @import("Parser.zig");
