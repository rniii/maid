const options =
    \\  -h       --help           Display this message
    \\  -l       --list           List tasks concisely
    \\  -n       --dry-run        Don't run anything, only display commands
    \\  -q       --quiet          Don't display anything
    \\  -f FILE  --taskfile=FILE  Use tasks in FILE
;

const Opts = union(enum) {
    help,
    list,
    dryRun,
    quiet,
    taskfile: []const u8,
};

var style: Style = undefined;

inline fn fatal(fmt: []const u8, args: anytype) noreturn {
    std.debug.print("{}error: {}", .{ style.err, style.secondary });
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});
    std.process.exit(1);
}

pub fn main() !void {
    style = Style.default();

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    var doList = false;
    var dryRun = false;
    var quiet = false;
    var taskName: ?[]const u8 = null;
    var tasksArg: ?Taskfile = null;
    var opts = getopt.parse(Opts, args, .{
        "h", "help",     .help,
        "l", "list",     .list,
        "n", "dry-run",  .dryRun,
        "q", "quiet",    .quiet,
        "f", "taskfile", .taskfile,
    });

    _ = opts.nextArg();

    while (true) {
        while (opts.next() catch |err| {
            fatal("{}", .{opts.errorMessage(err)});
        }) |opt| switch (opt) {
            .help => {
                std.debug.print("{}Usage: {}maid [options] [task]\n\n", .{ style.primary, style.secondary });
                std.debug.print("{}Options:{}\n", .{ style.primary, style.reset });
                std.debug.print("{s}\n", .{options});
                return;
            },
            .list => doList = true,
            .dryRun => dryRun = true,
            .quiet => quiet = true,
            .taskfile => |f| tasksArg = try readTaskfile(std.fs.cwd(), f),
        };

        if (opts.nextArg()) |arg| {
            if (taskName != null or doList) fatal("Unexpected argument {s}", .{arg});

            taskName = arg;
        } else {
            const tasks = tasksArg orelse try findTaskfile();

            if (doList) return listTasks(tasks);
            if (taskName) |a| return runTask(tasks, a);

            std.debug.print("{}Tasks in {s}\n\n", .{ style.primary, tasks.path });

            for (tasks.list) |task| {
                std.debug.print("{}  {s}\n", .{ style.secondary, task.name });
                std.debug.print("{}    {s}\n\n", .{ style.reset, task.desc });
            }

            return;
        }
    }
}

const Taskfile = struct {
    path: []const u8,
    list: []Task,
};

fn runTask(tasks: Taskfile, name: []const u8) !void {
    const task = for (tasks.list) |task| {
        if (std.mem.eql(u8, name, task.name))
            break task;
    } else fatal("No such task: {s}", .{name});

    std.debug.print("{}maid {s}{}", .{ style.secondary, name, style.reset });

    switch (task.code.lang) {
        .shell => {},
        .haskell => {},
        .javascript => {},
    }
}

fn listTasks(tasks: Taskfile) !void {
    const out = std.io.getStdOut().writer();

    for (tasks.list) |task|
        try out.print("{s} {s}\n", .{ task.name, task.desc });
}

fn readTaskfile(dir: std.fs.Dir, path: []const u8) !?Taskfile {
    const file = try dir.openFile(path, .{});

    var parser = try Parser.init(alloc);
    defer parser.deinit();

    var tasks = try parser.parseMarkdown(file.reader());
    defer tasks.deinit();

    if (tasks.items.len == 0) return null;

    return .{
        .path = path,
        .list = try tasks.toOwnedSlice(),
    };
}

fn findTaskfile() !Taskfile {
    var dir = std.fs.cwd();

    for (0..16) |_| {
        for ([_][]const u8{ "README.md", "CONTRIBUTING.md" }) |name|
            return try readTaskfile(dir, name) orelse continue;

        dir = try dir.openDir("..", .{});
    }

    fatal("No taskfile", .{});
}

const alloc = std.heap.page_allocator;

const std = @import("std");
const getopt = @import("getopt.zig");
const Parser = @import("Parser.zig");
const Style = @import("Style.zig");
const Task = @import("Task.zig");
