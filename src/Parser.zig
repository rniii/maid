buffer: std.ArrayList(u8),
headingNr: usize = 0,
fenceNr: usize = 0,
lineNr: usize = 0,
blank: bool = true,

pub fn init(alloc: mem.Allocator) !Self {
    const buffer = try std.ArrayList(u8).initCapacity(alloc, 32);
    return .{ .buffer = buffer };
}

pub fn parse(self: *Self, alloc: mem.Allocator, file: anytype) !std.ArrayList(Task) {
    var tasksHeading: usize = 0;
    var state: enum { tasklist, magic, task, desc, code } = .tasklist;

    var tasks = std.ArrayList(Task).init(alloc);
    var task: Task = undefined;

    while (true) {
        self.nextBlock(file) catch break;

        switch (state) {
            .tasklist => {
                if (self.headingNr != 0) continue;

                state = .magic;
                tasksHeading = self.headingNr;
            },
            .magic => {
                if (self.blank) continue;

                state = if (isMagic(self.buffer.items)) .task else .tasklist;
            },
            .task => {
                if (self.blank or self.headingNr == 0) continue;

                if (self.headingNr <= tasksHeading) break;

                state = .desc;
                task.name = try ascii.allocLowerString(
                    alloc,
                    mem.trimLeft(u8, self.buffer.items[self.headingNr..], " \t"),
                );
            },
            .desc => {
                if (self.blank) continue;

                state = .task;
                try tasks.append(task);
                task = undefined;
            },
            .code => {},
        }
    }

    return tasks;
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
            try self.nextLine(file);
            if (isBlank(self.buffer.items)) break;
        }
    }

    self.headingNr = heading(self.buffer.items);
    self.fenceNr = fence(self.buffer.items);
    self.blank = isBlank(self.buffer.items);
}

fn nextLine(self: *Self, file: anytype) !void {
    self.buffer.clearRetainingCapacity();

    file.streamUntilDelimiter(self.buffer.writer(), '\n', null) catch |err| {
        if (self.buffer.items.len == 0) return err;
    };

    self.lineNr += 1;
}

fn isMagic(text: []u8) bool {
    return mem.eql(u8, text, "<!-- maid-tasks -->");
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
const ascii = std.ascii;
const mem = std.mem;
const Self = @This();
const Task = @import("main.zig").Task;
