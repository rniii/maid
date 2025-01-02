name: []u8,
desc: []u8,
code: struct { lang: Lang, text: []u8 },

pub const Lang = enum {
    shell,
    haskell,
    javascript,

    pub fn fromId(id: []const u8) ?Lang {
        return langIds.get(id);
    }
};

const langIds = std.StaticStringMap(Lang).initComptime(.{
    .{ "sh", .shell },      .{ "bash", .shell },
    .{ "hs", .haskell },    .{ "haskell", .haskell },
    .{ "js", .javascript }, .{ "javascript", .javascript },
});

const std = @import("std");
