const std = @import("std");
const mir = @import("../mir.zig");
const diag = @import("../../diag/diagnostics.zig");
const Pass = @import("passes.zig").Pass;

/// Removes instructions whose results are never consumed.
pub const pass = Pass{ .name = "dead-code-elimination", .run = run };

fn run(temp_allocator: std.mem.Allocator, mir_crate: *mir.MirCrate, diagnostics: *diag.Diagnostics) !void {
    _ = diagnostics;

    const arena = mir_crate.allocator();
    for (mir_crate.fns.items) |*func| {
        try eliminateInFunction(temp_allocator, arena, func);
    }
}

fn eliminateInFunction(temp_allocator: std.mem.Allocator, arena: std.mem.Allocator, func: *mir.MirFn) !void {
    var max_temp: mir.TempId = 0;
    for (func.blocks) |block| {
        for (block.insts) |inst| {
            if (inst.dest) |tmp| {
                max_temp = @max(max_temp, tmp);
            }
            collectMaxTempFromInst(inst, &max_temp);
        }
        collectMaxTempFromTerm(block.term, &max_temp);
    }

    var used = try std.ArrayListUnmanaged(bool).initCapacity(temp_allocator, max_temp + 1);
    defer used.deinit(temp_allocator);
    used.appendNTimesAssumeCapacity(false, max_temp + 1);

    for (func.blocks) |block| {
        markUses(block.term, used.items);
    }

    // First pass: mark all uses from all instructions (not just dest==null)
    for (func.blocks) |block| {
        for (block.insts) |inst| {
            markUsesInInst(inst, used.items);
        }
    }
    
    // Iterate until no more changes to handle transitive uses
    var changed = true;
    while (changed) {
        changed = false;
        for (func.blocks) |block| {
            for (block.insts) |inst| {
                if (inst.dest) |tmp| {
                    if (tmp < used.items.len and used.items[tmp]) {
                        // This instruction's result is used, so mark its operands as used too
                        const prev_count = countUsed(used.items);
                        markUsesInInst(inst, used.items);
                        if (countUsed(used.items) > prev_count) changed = true;
                    }
                }
            }
        }
    }

    for (func.blocks) |*block| {
        var new_insts = std.ArrayListUnmanaged(mir.Inst){};
        defer new_insts.deinit(arena);

        var idx: usize = block.insts.len;
        while (idx > 0) {
            idx -= 1;
            const inst = block.insts[idx];

            const keep = if (inst.dest) |tmp| blk: {
                if (tmp < used.items.len and used.items[tmp]) {
                    break :blk true;
                }
                break :blk !isSideEffectFree(inst);
            } else true;

            if (keep) {
                try new_insts.insert(arena, 0, inst);
            }
        }

        block.insts = try new_insts.toOwnedSlice(arena);
    }
}

fn collectMaxTempFromInst(inst: mir.Inst, max_temp: *mir.TempId) void {
    markMaxTemp(inst.kind, max_temp);
}

fn collectMaxTempFromTerm(term: mir.TermKind, max_temp: *mir.TempId) void {
    switch (term) {
        .Goto => {},
        .If => |if_term| {
            if (if_term.cond == .Temp and if_term.cond.Temp > max_temp.*) max_temp.* = if_term.cond.Temp;
        },
        .Ret => |ret_op| if (ret_op) |op| {
            if (op == .Temp and op.Temp > max_temp.*) max_temp.* = op.Temp;
        },
    }
}

fn markMaxTemp(kind: mir.InstKind, max_temp: *mir.TempId) void {
    switch (kind) {
        .Copy => |copy| markOperandMax(copy.src, max_temp),
        .Bin => |bin| {
            markOperandMax(bin.lhs, max_temp);
            markOperandMax(bin.rhs, max_temp);
        },
        .Cmp => |cmp| {
            markOperandMax(cmp.lhs, max_temp);
            markOperandMax(cmp.rhs, max_temp);
        },
        .Unary => |un| markOperandMax(un.operand, max_temp),
        .Cast => |cast| markOperandMax(cast.src, max_temp),
        .StoreLocal => |store| markOperandMax(store.src, max_temp),
        .StorePtr => |store| {
            markOperandMax(store.ptr, max_temp);
            markOperandMax(store.src, max_temp);
        },
        .Call => |call| {
            markOperandMax(call.target, max_temp);
            for (call.args) |arg| markOperandMax(arg, max_temp);
        },
        .Range => |r| {
            markOperandMax(r.start, max_temp);
            markOperandMax(r.end, max_temp);
        },
        .Index => |idx| {
            markOperandMax(idx.target, max_temp);
            markOperandMax(idx.index, max_temp);
        },
        .Field => |field| markOperandMax(field.target, max_temp),
        .Array => |arr| for (arr.elems) |elem| markOperandMax(elem, max_temp),
        .StructInit => |st| for (st.fields) |field| markOperandMax(field.value, max_temp),
        .LoadLocal => {},
    }
}

fn markOperandMax(op: mir.Operand, max_temp: *mir.TempId) void {
    if (op == .Temp and op.Temp > max_temp.*) max_temp.* = op.Temp;
}

fn markUses(term: mir.TermKind, used: []bool) void {
    switch (term) {
        .Goto => {},
        .If => |if_term| markOperand(if_term.cond, used),
        .Ret => |ret_op| if (ret_op) |op| markOperand(op, used),
    }
}

fn markUsesInInst(inst: mir.Inst, used: []bool) void {
    switch (inst.kind) {
        .Copy => |copy| markOperand(copy.src, used),
        .Bin => |bin| {
            markOperand(bin.lhs, used);
            markOperand(bin.rhs, used);
        },
        .Cmp => |cmp| {
            markOperand(cmp.lhs, used);
            markOperand(cmp.rhs, used);
        },
        .Unary => |un| markOperand(un.operand, used),
        .Cast => |cast| markOperand(cast.src, used),
        .LoadLocal => {},
        .StoreLocal => |store| markOperand(store.src, used),
        .StorePtr => |store| {
            markOperand(store.ptr, used);
            markOperand(store.src, used);
        },
        .Call => |call| {
            markOperand(call.target, used);
            for (call.args) |arg| markOperand(arg, used);
        },
        .Range => |r| {
            markOperand(r.start, used);
            markOperand(r.end, used);
        },
        .Index => |idx| {
            markOperand(idx.target, used);
            markOperand(idx.index, used);
        },
        .Field => |field| markOperand(field.target, used),
        .Array => |arr| for (arr.elems) |elem| markOperand(elem, used),
        .StructInit => |st| for (st.fields) |field| markOperand(field.value, used),
    }
}

fn markOperand(op: mir.Operand, used: []bool) void {
    if (op == .Temp and op.Temp < used.len) {
        used[op.Temp] = true;
    }
}

fn countUsed(used: []const bool) usize {
    var count: usize = 0;
    for (used) |u| {
        if (u) count += 1;
    }
    return count;
}

fn isSideEffectFree(inst: mir.Inst) bool {
    return switch (inst.kind) {
        .Copy, .Bin, .Cmp, .Unary, .LoadLocal => true,
        else => false,
    };
}

test "dead code elimination removes unused temps" {
    var mir_crate = mir.MirCrate.init(std.testing.allocator);
    defer mir_crate.deinit();

    var insts = std.ArrayListUnmanaged(mir.Inst){};
    defer insts.deinit(mir_crate.allocator());
    try insts.append(mir_crate.allocator(), .{ .ty = .I32, .dest = 0, .kind = .{ .Copy = .{ .src = .{ .ImmInt = 7 } } } });
    try insts.append(mir_crate.allocator(), .{ .ty = .I32, .dest = 1, .kind = .{ .Copy = .{ .src = .{ .Temp = 0 } } } });
    try insts.append(mir_crate.allocator(), .{ .ty = .I32, .dest = 2, .kind = .{ .Copy = .{ .src = .{ .ImmInt = 9 } } } });

    const block = mir.Block{
        .insts = try insts.toOwnedSlice(mir_crate.allocator()),
        .term = .{ .Ret = .{ .Temp = 1 } },
    };

    var blocks = std.ArrayListUnmanaged(mir.Block){};
    defer blocks.deinit(mir_crate.allocator());
    try blocks.append(mir_crate.allocator(), block);

    try mir_crate.fns.append(mir_crate.allocator(), .{
        .name = "dce",
        .params = &[_]mir.LocalId{},
        .locals = &[_]mir.MirType{},
        .ret_ty = .I32,
        .blocks = try blocks.toOwnedSlice(mir_crate.allocator()),
    });

    var diagnostics = diag.Diagnostics.init(std.testing.allocator);
    defer diagnostics.deinit();

    try run(std.testing.allocator, &mir_crate, &diagnostics);
    try std.testing.expect(!diagnostics.hasErrors());

    const inst_slice = mir_crate.fns.items[0].blocks[0].insts;
    try std.testing.expectEqual(@as(usize, 2), inst_slice.len);
    try std.testing.expectEqual(@as(mir.TempId, 1), inst_slice[1].dest.?);
}
