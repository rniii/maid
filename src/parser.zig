buffer: std.ArrayList(u8),
headingNr: usize = 0,

pub fn init(alloc: std.mem.Allocator) !Self {
    const buffer = try std.ArrayList(u8).initCapacity(alloc, 32);
    return .{ .buffer = buffer };
}

pub fn parse(self: *Self, file: anytype) void {
    self.nextBlock(file) catch return;

    self.headingNr = heading(self.buffer.items);
    if (self.headingNr != 0)
        return @call(.always_tail, parseTasklist, .{ self, file })
    else
        return @call(.always_tail, parse, .{ self, file });
}

fn parseTasklist(self: *Self, file: anytype) void {
    self.nextBlock(file) catch return;

    if (std.mem.eql(u8, self.buffer.items, "<!-- maid-tasks -->"))
        return @call(.always_tail, parseTasks, .{ self, file })
    else if (isBlank(self.buffer.items))
        return @call(.always_tail, parseTasklist, .{ self, file })
    else
        return @call(.always_tail, parse, .{ self, file });
}

fn parseTasks(self: *Self, file: anytype) void {
    self.nextBlock(file) catch return;

    std.debug.print("{}\n", .{self.headingNr});
}

fn nextBlock(self: *Self, file: anytype) !void {
    const line = self.buffer.items;
    const count = fence(line);
    if (count > 0) {
        try self.nextLine(file);

        while (true) : (try self.nextLine(file)) {
            if (fence(line) >= count) break;
        }
    } else {
        try self.nextLine(file);
    }
}

fn nextLine(self: *Self, file: anytype) !void {
    self.buffer.clearRetainingCapacity();

    file.streamUntilDelimiter(self.buffer.writer(), '\n', null) catch |err| {
        if (self.buffer.items.len == 0) return err;
    };
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
    }
    return 0;
}

fn heading(text: []u8) usize {
    for (text, 0..6) |c, i| {
        if (c != '#') return i;
    }
    return 0;
}

const std = @import("std");
const Self = @This();
