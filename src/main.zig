pub const Task = struct {
    name: []u8,
    description: []u8,
    code: struct { lang: []u8, text: []u8 },
};

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const stdin = try std.fs.cwd().openFile("README.md", .{});

    var parser = try Parser.init(alloc);
    const tasks = try parser.parse(alloc, stdin.reader());

    for (tasks.items) |task| {
        std.debug.print("task: {s}\n", .{task.name});
        std.debug.print("{s}\n", .{task.description});
        std.debug.print("\n", .{});
        std.debug.print("code({s}):\n", .{task.code.lang});
        std.debug.print("{s}\n", .{task.code.text});
        std.debug.print("\n", .{});
    }
}

const std = @import("std");
const Parser = @import("Parser.zig");
