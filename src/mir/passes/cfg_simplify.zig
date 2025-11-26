const std = @import("std");
const mir = @import("../mir.zig");
const diag = @import("../../diag/diagnostics.zig");
const Pass = @import("passes.zig").Pass;

/// Removes unreachable blocks and rewrites branch targets to compact block indices.
pub const pass = Pass{ .name = "cfg-simplify", .run = run };

fn run(temp_allocator: std.mem.Allocator, mir_crate: *mir.MirCrate, diagnostics: *diag.Diagnostics) !void {
    _ = diagnostics;

    const arena = mir_crate.allocator();
    for (mir_crate.fns.items) |*func| {
        if (func.blocks.len == 0) continue;
        try simplifyFunction(temp_allocator, arena, func);
    }
}

fn simplifyFunction(temp_allocator: std.mem.Allocator, arena: std.mem.Allocator, func: *mir.MirFn) !void {
    const block_count = func.blocks.len;
    var reachable = try std.ArrayListUnmanaged(bool).initCapacity(temp_allocator, block_count);
    defer reachable.deinit(temp_allocator);
    reachable.appendNTimesAssumeCapacity(false, block_count);

    var worklist = std.ArrayListUnmanaged(mir.BlockId){};
    defer worklist.deinit(temp_allocator);
    try worklist.append(temp_allocator, 0);

    while (true) {
        const current = worklist.pop() orelse break;
        if (@as(usize, current) >= block_count) continue;
        if (reachable.items[current]) continue;
        reachable.items[current] = true;

        const term = func.blocks[current].term;
        switch (term) {
            .Goto => |target| try worklist.append(temp_allocator, target),
            .If => |if_term| {
                try worklist.append(temp_allocator, if_term.then_block);
                try worklist.append(temp_allocator, if_term.else_block);
            },
            .Ret => {},
        }
    }

    if (std.mem.indexOfScalar(bool, reachable.items, false) == null) return;

    var map = try std.ArrayListUnmanaged(?mir.BlockId).initCapacity(temp_allocator, block_count);
    defer map.deinit(temp_allocator);
    map.appendNTimesAssumeCapacity(null, block_count);

    var new_blocks = std.ArrayListUnmanaged(mir.Block){};
    defer new_blocks.deinit(arena);
    var old_ids = std.ArrayListUnmanaged(mir.BlockId){};
    defer old_ids.deinit(temp_allocator);

    for (func.blocks, 0..) |block, idx| {
        if (!reachable.items[idx]) continue;
        const new_id: mir.BlockId = @intCast(new_blocks.items.len);
        map.items[idx] = new_id;
        try new_blocks.append(arena, block);
        try old_ids.append(temp_allocator, @intCast(idx));
    }

    for (new_blocks.items, 0..) |*block, pos| {
        const old_id = old_ids.items[pos];
        block.term = rewriteTerm(func.blocks[old_id].term, map.items);
    }

    func.blocks = try new_blocks.toOwnedSlice(arena);
}

fn rewriteTerm(term: mir.TermKind, map: []const ?mir.BlockId) mir.TermKind {
    return switch (term) {
        .Goto => |target| .{ .Goto = map[target].? },
        .If => |if_term| .{
            .If = .{
                .cond = if_term.cond,
                .then_block = map[if_term.then_block].?,
                .else_block = map[if_term.else_block].?,
            },
        },
        .Ret => term,
    };
}

test "cfg simplification drops unreachable blocks" {
    var mir_crate = mir.MirCrate.init(std.testing.allocator);
    defer mir_crate.deinit();

    var blocks = std.ArrayListUnmanaged(mir.Block){};
    defer blocks.deinit(mir_crate.allocator());

    try blocks.append(mir_crate.allocator(), .{
        .insts = &[_]mir.Inst{},
        .term = .{ .Goto = 1 },
    });

    try blocks.append(mir_crate.allocator(), .{
        .insts = &[_]mir.Inst{},
        .term = .{ .Ret = null },
    });

    try blocks.append(mir_crate.allocator(), .{
        .insts = &[_]mir.Inst{},
        .term = .{ .Ret = null },
    });

    try mir_crate.fns.append(mir_crate.allocator(), .{
        .name = "cfg",
        .params = &[_]mir.LocalId{},
        .locals = &[_]mir.MirType{},
        .ret_ty = null,
        .blocks = try blocks.toOwnedSlice(mir_crate.allocator()),
    });

    var diagnostics = diag.Diagnostics.init(std.testing.allocator);
    defer diagnostics.deinit();

    try run(std.testing.allocator, &mir_crate, &diagnostics);
    try std.testing.expect(!diagnostics.hasErrors());

    const func = mir_crate.fns.items[0];
    try std.testing.expectEqual(@as(usize, 2), func.blocks.len);
    try std.testing.expect(func.blocks[0].term == .Goto);
    try std.testing.expectEqual(@as(mir.BlockId, 1), func.blocks[0].term.Goto);
}
