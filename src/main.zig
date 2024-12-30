pub fn main() !void {
    const out = std.io.getStdOut().writer();
    const alloc = std.heap.page_allocator;

    const tasks = try findTaskfile(alloc);
    const style = Style.default();

    try std.fmt.format(out, "{}Tasks in {s}\n\n", .{ style.primary, tasks.path });

    for (tasks.list) |task| {
        try std.fmt.format(out, "{}  {s}\n", .{ style.secondary, task.name });
        try std.fmt.format(out, "{}    {s}\n\n", .{ style.reset, task.desc });
    }
}

const Taskfile = struct {
    path: []const u8,
    list: []Task,
};

fn findTaskfile(alloc: std.mem.Allocator) !Taskfile {
    var parser = try Parser.init(alloc);
    var tasks = std.ArrayList(Task).init(alloc);
    var dir = std.fs.cwd();

    for (0..16) |_| {
        for ([_]([]const u8){ "README.md", "CONTRIBUTING.md" }) |name| {
            const file = try dir.openFile(name, .{});
            try parser.parseMarkdown(file.reader(), &tasks);

            if (tasks.items.len > 0) return .{
                .path = name,
                .list = try tasks.toOwnedSlice(),
            };
        }

        dir = try dir.openDir("..", .{});
    }

    return error.NoTaskfile;
}

const std = @import("std");
const Parser = @import("Parser.zig");
const Style = @import("Style.zig");
const Task = @import("Task.zig");
