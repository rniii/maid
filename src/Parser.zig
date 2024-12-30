alloc: std.mem.Allocator,
line: std.ArrayList(u8),
block: std.ArrayList(u8),

headingNr: usize = 0,
fenceNr: usize = 0,
lineNr: usize = 0,
lang: ?Task.Lang = null,

pub fn init(alloc: std.mem.Allocator) !Parser {
    const line = try std.ArrayList(u8).initCapacity(alloc, 32);
    const block = try std.ArrayList(u8).initCapacity(alloc, 128);

    return .{ .alloc = alloc, .line = line, .block = block };
}

pub fn parseMarkdown(self: *Parser, file: anytype, tasks: *std.ArrayList(Task)) !void {
    var task: Task = undefined;

    var tasksHeading: usize = 0;
    var state: enum { tasklist, magic, task, desc, code } = .tasklist;

    while (self.nextBlock(file) catch null) |_| sw: switch (state) {
        .tasklist => {
            if (self.headingNr == 0) continue;

            state = .magic;
            tasksHeading = self.headingNr;
        },
        .magic => {
            state = if (isMagic(self.block.items)) .task else .tasklist;
        },
        .task => {
            if (self.headingNr == 0) continue;
            if (self.headingNr <= tasksHeading) break;

            state = .desc;
            task.name = try std.ascii.allocLowerString(self.alloc, self.block.items);
        },
        .desc => {
            if (self.headingNr > 0) continue :sw .task;

            state = .code;
            task.desc = try self.alloc.dupe(u8, if (self.fenceNr == 0)
                trim(u8, self.block.items, " \t")
            else
                "[No description]");
            continue :sw state;
        },
        .code => {
            if (self.headingNr > 0) continue :sw .task;
            if (self.fenceNr == 0) continue;

            state = .task;
            task.code.lang = self.lang.?;
            task.code.text = try self.alloc.dupe(u8, self.block.items);
            try tasks.append(task);
        },
    };
}

fn nextBlock(self: *Parser, file: anytype) !void {
    self.block.clearRetainingCapacity();

    while (true) {
        try self.nextLine(file);
        if (!isBlank(self.line.items)) break;
    }

    self.headingNr = heading(self.line.items);
    self.fenceNr = fence(self.line.items);

    if (self.headingNr > 0) {
        try self.block.appendSlice(trim(u8, self.line.items[self.headingNr..], " \t"));
    } else if (self.fenceNr > 0) {
        self.lang = Task.Lang.fromId(trim(u8, self.line.items[self.fenceNr..], " \t"));

        while (true) : ({
            try self.block.append('\n');
            try self.block.appendSlice(self.line.items);
        }) {
            try self.nextLine(file);
            if (fence(self.line.items) >= self.fenceNr) break;
        }
    } else {
        try self.block.appendSlice(self.line.items);

        while (true) : ({
            try self.block.append(' ');
            try self.block.appendSlice(self.line.items);
        }) {
            try self.nextLine(file);
            if (isBlank(self.line.items)) break;
        }
    }
}

fn nextLine(self: *Parser, file: anytype) !void {
    self.line.clearRetainingCapacity();
    self.lineNr += 1;

    return file.streamUntilDelimiter(self.line.writer(), '\n', null) catch |err|
        if (self.line.items.len == 0) err;
}

fn isMagic(text: []u8) bool {
    return std.mem.startsWith(u8, text, "<!-- maid-tasks -->");
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
const trim = std.mem.trim;
const Parser = @This();
const Task = @import("Task.zig");
