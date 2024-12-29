buffer: std.ArrayList(u8),
headingNr: usize = 0,
fenceNr: usize = 0,
lineNr: usize = 0,
blank: bool = true,

pub fn init(alloc: std.mem.Allocator) !Self {
    const buffer = try std.ArrayList(u8).initCapacity(alloc, 32);
    return .{ .buffer = buffer };
}

pub fn parse(self: *Self, alloc: std.mem.Allocator, file: anytype) !std.ArrayList(Task) {
    var tasksHeading: usize = 0;
    var state: enum(u8) { tasklist, magic, task, desc, code } = .tasklist;

    var tasks = std.ArrayList(Task).init(alloc);
    var task: Task = undefined;

    while (true) {
        self.nextBlock(file) catch break;

        std.debug.print("{d} {:3} | {s}\n", .{ @intFromEnum(state), self.lineNr, self.buffer.items });

        sw: switch (state) {
            .tasklist => {
                if (self.headingNr == 0) continue;

                state = .magic;
                tasksHeading = self.headingNr;
            },
            .magic => {
                if (self.blank) continue;

                state = if (isMagic(self.buffer.items)) .task else .tasklist;
            },
            .task => {
                if (self.headingNr == 0) continue;
                if (self.headingNr <= tasksHeading) break;

                state = .desc;
                task.name = try std.ascii.allocLowerString(
                    alloc,
                    std.mem.trim(u8, self.buffer.items[self.headingNr..], " \t"),
                );
            },
            .desc => {
                if (self.headingNr > 0) continue :sw .task;
                if (self.blank) continue;

                state = .code;
                if (self.fenceNr == 0) {
                    var desc = try std.ArrayList(u8).initCapacity(alloc, self.buffer.items.len);
                    while (!isBlank(self.buffer.items)) {
                        try desc.appendSlice(self.buffer.items);
                        try desc.append('\n');
                        try self.nextLine(file);
                    }
                    self.identify();
                    task.description = try desc.toOwnedSlice();
                } else {
                    task.description = try alloc.dupe(u8, "[No description]");
                }
                continue :sw state;
            },
            .code => {
                if (self.headingNr > 0) continue :sw .task;
                if (self.blank or self.fenceNr == 0) continue;

                state = .task;
                task.code.lang = try alloc.dupe(u8, self.buffer.items[self.fenceNr..]);
                {
                    var code = try std.ArrayList(u8).initCapacity(alloc, self.buffer.items.len);
                    while (fence(self.buffer.items) < self.fenceNr) {
                        try code.appendSlice(self.buffer.items);
                        try code.append('\n');
                        try self.nextLine(file);
                    }
                    self.identify();
                    task.code.text = try code.toOwnedSlice();
                }
                try tasks.append(task);
            },
        }
    }

    return tasks;
}

fn identify(self: *Self) void {
    self.headingNr = heading(self.buffer.items);
    self.fenceNr = fence(self.buffer.items);
    self.blank = isBlank(self.buffer.items);
}

fn nextBlock(self: *Self, file: anytype) !void {
    try self.nextLine(file);

    if (self.fenceNr > 0) {
        while (true) {
            try self.nextLine(file);
            if (fence(self.buffer.items) >= self.fenceNr) break;
        }
        try self.nextLine(file);
    } else if (!self.blank and self.headingNr == 0) {
        while (true) {
            if (isBlank(self.buffer.items)) break;
            try self.nextLine(file);
        }
    }

    self.identify();
}

fn nextLine(self: *Self, file: anytype) !void {
    self.buffer.clearRetainingCapacity();

    file.streamUntilDelimiter(self.buffer.writer(), '\n', null) catch |err| {
        if (self.buffer.items.len == 0) return err;
    };

    self.lineNr += 1;
}

fn isMagic(text: []u8) bool {
    return std.mem.eql(u8, text, "<!-- maid-tasks -->");
}

fn isBlank(text: []u8) bool {
    for (text) |c| if (c != ' ' and c != '\t') return false;

    return true;
}

fn fence(text: []u8) usize {
    if (text.len < 3)
        return 0;
    if (text[0] == '`' or text[0] == '~') {
        for (text[1..], 1..) |c, i| {
            if (c != text[0]) return if (i < 3) 0 else i;
        }
        return text.len;
    }
    return 0;
}

fn heading(text: []u8) usize {
    for (text, 0..) |c, i| {
        if (i > 6) return 0;
        if (c != '#') return i;
    }
    return 0;
}

const std = @import("std");
const Self = @This();
const Task = @import("main.zig").Task;
