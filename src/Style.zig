primary: Color,
secondary: Color,
reset: Color,
err: Color,

pub fn default() Self {
    return if (std.process.hasEnvVarConstant("NO_COLOR") or
        !std.io.getStdOut().getOrEnableAnsiEscapeSupport())
        .{
            .primary = .none,
            .secondary = .none,
            .reset = .none,
            .err = .none,
        }
    else
        .{
            .primary = Color.make("95;1"),
            .secondary = Color.make("0;1"),
            .reset = Color.make(""),
            .err = Color.make("31;1"),
        };
}

pub const Color = union(enum) {
    esc: []const u8,
    none,

    fn make(esc: []const u8) Color {
        return .{ .esc = esc };
    }

    pub fn format(
        self: Color,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .esc => |esc| try std.fmt.format(writer, "\x1b[{s}m", .{esc}),
            else => {},
        }
    }
};

const std = @import("std");
const Self = @This();
