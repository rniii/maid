//! getopt(3)-style command-line parsing
//!
//! https://git.transistor.house/rini/getopt.zig
//!
//! COPYRIGHT
//!     This file is provided in the same terms as Zig itself.
//!
//!     https://github.com/ziglang/zig/blob/master/LICENSE

pub var no_permute = false;

pub fn parse(comptime T: type, args: anytype, opts: anytype) Parser(T, @TypeOf(args), opts) {
    return .{ .args = args };
}

pub fn parseAlloc(
    comptime T: type,
    alloc: std.mem.Allocator,
    args: anytype,
    opts: anytype,
) ![]T {
    const buffer = try alloc.alloc(T, args.len);
    var parser = parse(T, args, opts);

    var i: usize = 0;
    while (try parser.next()) |opt| : (i += 1)
        buffer[i] = opt;

    return try alloc.realloc(buffer, i);
}

const ArgDescr = enum { noArg, optArg, reqArg };

const OptDescr = struct {
    short: ?u8,
    long: ?[]const u8,
    field: []const u8,
    arg: ArgDescr,
};

pub const ParseError = error{
    RequiredArgument,
    UnknownOption,
    UnexpectedArgument,
};

pub const ErrorMessage = struct {
    err: ParseError,
    opt: []const u8,

    pub fn format(
        self: ErrorMessage,
        _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        const prefix = if (self.opt.len > 1) "--" else "-";
        const args = .{ prefix, self.opt };

        try switch (self.err) {
            error.RequiredArgument => std.fmt.format(writer, "Option {s}{s} requires an argument\n", args),
            error.UnknownOption => std.fmt.format(writer, "Unknown option {s}{s}\n", args),
            error.UnexpectedArgument => std.fmt.format(writer, "Option {s}{s} doesn't allow arguments\n", args),
        };
    }
};

fn Parser(comptime T: type, comptime Args: type, comptime opts: anytype) type {
    comptime var descrs: [@divExact(opts.len, 3)]OptDescr = undefined;
    comptime {
        var i = 0;
        while (i < opts.len) : (i += 3) {
            const short = switch (opts[i + 0].len) {
                0 => null,
                1 => opts[i + 0][0],
                else => unreachable,
            };
            const long = switch (opts[i + 1].len) {
                0 => null,
                1 => unreachable,
                else => opts[i + 1],
            };
            const field = @tagName(opts[i + 2]);
            const arg = switch (@FieldType(T, field)) {
                ?[]const u8 => .optArg,
                []const u8 => .reqArg,
                void => .noArg,
                else => unreachable,
            };

            descrs[@divExact(i, 3)] =
                .{ .short = short, .long = long, .arg = arg, .field = field };
        }
    }

    return struct {
        const Self = @This();

        args: Args,
        index: usize = 0,
        opt_index: usize = 0,

        errored_opt: []const u8 = &.{},

        pub fn nextArg(self: *Self) ?[]const u8 {
            if (self.index < self.args.len) {
                defer self.index += 1;
                return self.args[self.index];
            }
            return null;
        }

        pub fn next(self: *Self) ParseError!?T {
            var index_shift: usize = 1;
            defer self.index += index_shift;

            if (self.opt_index > 0 and self.opt_index + 1 >= self.args[self.index].len) {
                self.opt_index = 0;
                self.index += 1;
            }

            if (self.index >= self.args.len) return null;

            if (self.args[self.index].len < 2 or self.args[self.index][0] != '-') {
                index_shift = 0;
                return null;

                // non-option; TODO: rotate arguments? ? ??
            }

            if (self.opt_index > 0 or self.args[self.index][1] != '-') {
                self.opt_index += 1;

                // short option
                const opt = self.args[self.index][self.opt_index];
                const arg = self.args[self.index][self.opt_index + 1 ..];

                inline for (descrs) |desc| if (desc.short) |short| {
                    if (opt == short) switch (desc.arg) {
                        .reqArg => {
                            self.opt_index = 0;
                            if (arg.len > 0) {
                                return @unionInit(T, desc.field, arg);
                            } else if (self.index + 1 < self.args.len) {
                                self.index += 1;
                                return @unionInit(T, desc.field, self.args[self.index]);
                            } else {
                                self.errored_opt = &.{opt};
                                return ParseError.RequiredArgument;
                            }
                        },
                        .optArg => {
                            self.opt_index = 0;
                            return @unionInit(T, desc.field, if (arg.len > 0) arg else null);
                        },
                        .noArg => {
                            index_shift = 0;
                            return @unionInit(T, desc.field, {});
                        },
                    };
                };

                self.errored_opt = &.{opt};
                return ParseError.UnknownOption;
            }

            if (self.args[self.index].len == 2) return null; // end of options

            // long option
            var opt_ = self.args[self.index];
            var opt = opt_[2..opt_.len]; // might be sentinel array
            var arg: ?[]const u8 = null;

            if (std.mem.indexOfScalar(u8, opt, '=')) |eq| {
                arg = opt[eq + 1 ..];
                opt = opt[0..eq];
            }

            inline for (descrs) |desc| if (desc.long) |long| {
                if (std.mem.eql(u8, opt, long)) switch (desc.arg) {
                    .reqArg => {
                        if (arg) |a| {
                            return @unionInit(T, desc.field, a);
                        } else if (self.index + 1 < self.args.len) {
                            self.index += 1;
                            return @unionInit(T, desc.field, self.args[self.index]);
                        } else {
                            self.errored_opt = opt;
                            return ParseError.RequiredArgument;
                        }
                    },
                    .optArg => return @unionInit(T, desc.field, arg),
                    .noArg => {
                        if (arg) |_| {
                            self.errored_opt = opt;
                            return ParseError.UnexpectedArgument;
                        } else {
                            return @unionInit(T, desc.field, {});
                        }
                    },
                };
            };

            self.errored_opt = opt;
            return ParseError.UnknownOption;
        }

        pub fn errorMessage(self: *Self, err: ParseError) ErrorMessage {
            return .{ .err = err, .opt = self.errored_opt };
        }
    };
}

inline fn ArgType(T: type) ArgDescr {
    return switch (T) {
        ?[]const u8 => .optArg,
        []const u8 => .reqArg,
        void => .noArg,
        else => unreachable,
    };
}

test "getopt" {
    const Opts = union(enum) {
        help,
        version,
        output: []const u8,
    };

    const args = [_][]const u8{
        "-omeow.a", "-o",   "meow.b", "--output=meow.c", "--output", "meow.d", "foo",
        "--help",   "-vvv",
    };
    var opts = parse(Opts, args, .{
        "h", "help",    .help,
        "v", "version", .version,
        "o", "output",  .output,
    });

    try expectEqualDeep(opts.next(), Opts{ .output = "meow.a" });
    try expectEqualDeep(opts.next(), Opts{ .output = "meow.b" });
    try expectEqualDeep(opts.next(), Opts{ .output = "meow.c" });
    try expectEqualDeep(opts.next(), Opts{ .output = "meow.d" });
    try expectEqualDeep(opts.next(), null);
    try expectEqualDeep(opts.nextArg(), "foo");
    try expectEqualDeep(opts.next(), Opts.help);
    try expectEqualDeep(opts.next(), Opts.version);
    try expectEqualDeep(opts.next(), Opts.version);
    try expectEqualDeep(opts.next(), Opts.version);
    try expectEqualDeep(opts.next(), null);
    try expectEqualDeep(opts.nextArg(), null);
}

const std = @import("std");
const expectEqualDeep = std.testing.expectEqualDeep;
