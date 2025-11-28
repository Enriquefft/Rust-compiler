//! Pretty-printer for MIR data structures.
//!
//! This module provides tree-based visualization of MIR crates, functions, blocks,
//! and instructions. The output is formatted as an ASCII tree structure for easy
//! reading and debugging during compiler development.

const std = @import("std");
const mir = @import("mir.zig");
const AllocError = std.mem.Allocator.Error;

/// Helper for printing tree-structured output with proper indentation and connectors.
/// Manages a stack of indentation prefixes to create visual tree branches.
const TreePrinter = struct {
    allocator: std.mem.Allocator,
    /// Current indentation prefix string
    prefix: std.ArrayListUnmanaged(u8),
    /// Stack of segment lengths for prefix management
    segment_lengths: std.ArrayListUnmanaged(usize),

    /// Initialize a new tree printer with the given allocator.
    fn init(allocator: std.mem.Allocator) TreePrinter {
        return .{
            .allocator = allocator,
            .prefix = .{},
            .segment_lengths = .{},
        };
    }

    /// Release all memory used by this printer.
    fn deinit(self: *TreePrinter) void {
        self.prefix.deinit(self.allocator);
        self.segment_lengths.deinit(self.allocator);
    }

    /// Push a new indentation level. Use is_last to determine connector style.
    fn push(self: *TreePrinter, is_last: bool) !void {
        const segment: []const u8 = if (is_last) "    " else "│   ";
        try self.prefix.appendSlice(self.allocator, segment);
        try self.segment_lengths.append(self.allocator, segment.len);
    }

    /// Pop the most recent indentation level.
    fn pop(self: *TreePrinter) void {
        if (self.segment_lengths.items.len == 0) return;

        const last_len = self.segment_lengths.pop().?;
        const new_len = self.prefix.items.len - last_len;
        self.prefix.shrinkRetainingCapacity(new_len);
    }

    /// Print a node with the current indentation and appropriate connector.
    fn printNode(self: *TreePrinter, is_last: bool, label: []const u8) !void {
        const connector: []const u8 = if (self.prefix.items.len == 0)
            ""
        else if (is_last)
            "└── "
        else
            "├── ";

        std.debug.print("{s}{s}{s}\n", .{ self.prefix.items, connector, label });
    }

    /// Print a formatted node using the given format string and arguments.
    fn printNodeFmt(self: *TreePrinter, is_last: bool, comptime fmt: []const u8, args: anytype) !void {
        const label = try std.fmt.allocPrint(self.allocator, fmt, args);
        defer self.allocator.free(label);
        try self.printNode(is_last, label);
    }
};

/// Print a complete MIR crate as a tree structure to debug output.
pub fn printCrateTree(allocator: std.mem.Allocator, crate: *const mir.MirCrate) AllocError!void {
    var printer = TreePrinter.init(allocator);
    defer printer.deinit();

    try printer.printNode(true, "MIR Crate");
    if (crate.fns.items.len == 0) return;

    try printer.push(true);
    for (crate.fns.items, 0..) |func, idx| {
        try printFunction(&printer, func, idx == crate.fns.items.len - 1);
    }
    printer.pop();
}

/// Print a single MIR function as a tree node.
fn printFunction(printer: *TreePrinter, func: mir.MirFn, is_last: bool) AllocError!void {
    try printer.printNodeFmt(is_last, "Function {s}", .{func.name});
    try printer.push(is_last);

    // Print return type
    if (func.ret_ty) |ret_ty| {
        try printer.printNodeFmt(false, "Return: {s}", .{@tagName(ret_ty)});
    } else {
        try printer.printNode(false, "Return: ()");
    }

    // Print parameters
    try printer.printNode(false, "Params");
    if (func.params.len > 0) {
        try printer.push(false);
        for (func.params, 0..) |param_id, idx| {
            try printer.printNodeFmt(idx == func.params.len - 1, "Param {d}", .{param_id});
        }
        printer.pop();
    }

    // Print locals
    try printer.printNode(false, "Locals");
    if (func.locals.len > 0) {
        try printer.push(false);
        for (func.locals, 0..) |local_ty, idx| {
            try printer.printNodeFmt(idx == func.locals.len - 1, "Local {d}: {s}", .{ idx, @tagName(local_ty) });
        }
        printer.pop();
    }

    // Print blocks
    for (func.blocks, 0..) |block, block_idx| {
        try printBlock(printer, block, @intCast(block_idx), block_idx == func.blocks.len - 1);
    }

    printer.pop();
}

/// Print a basic block as a tree node.
fn printBlock(printer: *TreePrinter, block: mir.Block, block_id: mir.BlockId, is_last: bool) AllocError!void {
    try printer.printNodeFmt(is_last, "Block {d}", .{block_id});
    try printer.push(is_last);

    // Print instructions
    for (block.insts, 0..) |inst, inst_idx| {
        try printInst(printer, inst, inst_idx == block.insts.len - 1 and isSimpleTerminator(block.term));
    }

    // Print terminator
    try printTerminator(printer, block.term, true);

    printer.pop();
}

/// Check if a terminator is simple (void return) for formatting purposes.
fn isSimpleTerminator(term: mir.TermKind) bool {
    return switch (term) {
        .Ret => |val| val == null,
        else => false,
    };
}

/// Print a single instruction as a tree node.
fn printInst(printer: *TreePrinter, inst: mir.Inst, is_last: bool) AllocError!void {
    const dest_str = if (inst.dest) |d| blk: {
        break :blk try std.fmt.allocPrint(printer.allocator, "t{d} = ", .{d});
    } else "";
    defer if (inst.dest != null) printer.allocator.free(dest_str);

    switch (inst.kind) {
        .Copy => |copy| {
            const src = try formatOperand(printer.allocator, copy.src);
            defer printer.allocator.free(src);
            try printer.printNodeFmt(is_last, "{s}Copy {s}", .{ dest_str, src });
        },
        .Bin => |bin| {
            const lhs = try formatOperand(printer.allocator, bin.lhs);
            defer printer.allocator.free(lhs);
            const rhs = try formatOperand(printer.allocator, bin.rhs);
            defer printer.allocator.free(rhs);
            try printer.printNodeFmt(is_last, "{s}{s} {s}, {s}", .{ dest_str, @tagName(bin.op), lhs, rhs });
        },
        .Cmp => |cmp| {
            const lhs = try formatOperand(printer.allocator, cmp.lhs);
            defer printer.allocator.free(lhs);
            const rhs = try formatOperand(printer.allocator, cmp.rhs);
            defer printer.allocator.free(rhs);
            try printer.printNodeFmt(is_last, "{s}{s} {s}, {s}", .{ dest_str, @tagName(cmp.op), lhs, rhs });
        },
        .Unary => |unary| {
            const operand = try formatOperand(printer.allocator, unary.operand);
            defer printer.allocator.free(operand);
            try printer.printNodeFmt(is_last, "{s}{s} {s}", .{ dest_str, @tagName(unary.op), operand });
        },
        .Cast => |cast| {
            const src = try formatOperand(printer.allocator, cast.src);
            defer printer.allocator.free(src);
            try printer.printNodeFmt(is_last, "{s}Cast {s} as {s} -> {s}", .{ dest_str, src, @tagName(cast.from_ty), @tagName(cast.to_ty) });
        },
        .LoadLocal => |load| {
            try printer.printNodeFmt(is_last, "{s}LoadLocal {d}", .{ dest_str, load.local });
        },
        .StoreLocal => |store| {
            const src = try formatOperand(printer.allocator, store.src);
            defer printer.allocator.free(src);
            try printer.printNodeFmt(is_last, "StoreLocal {d} <- {s}", .{ store.local, src });
        },
        .StorePtr => |store| {
            const ptr = try formatOperand(printer.allocator, store.ptr);
            defer printer.allocator.free(ptr);
            const src = try formatOperand(printer.allocator, store.src);
            defer printer.allocator.free(src);
            try printer.printNodeFmt(is_last, "StorePtr {s} <- {s}", .{ ptr, src });
        },
        .Call => |call| {
            const target = try formatOperand(printer.allocator, call.target);
            defer printer.allocator.free(target);
            var args_buf = std.ArrayListUnmanaged(u8){};
            defer args_buf.deinit(printer.allocator);
            for (call.args, 0..) |arg, idx| {
                const arg_str = try formatOperand(printer.allocator, arg);
                defer printer.allocator.free(arg_str);
                try args_buf.appendSlice(printer.allocator, arg_str);
                if (idx < call.args.len - 1) {
                    try args_buf.appendSlice(printer.allocator, ", ");
                }
            }
            try printer.printNodeFmt(is_last, "{s}Call {s}({s})", .{ dest_str, target, args_buf.items });
        },
        .Range => |range| {
            const start = try formatOperand(printer.allocator, range.start);
            defer printer.allocator.free(start);
            const end = try formatOperand(printer.allocator, range.end);
            defer printer.allocator.free(end);
            const op = if (range.inclusive) "..=" else "..";
            try printer.printNodeFmt(is_last, "{s}Range {s}{s}{s}", .{ dest_str, start, op, end });
        },
        .Index => |index| {
            const target = try formatOperand(printer.allocator, index.target);
            defer printer.allocator.free(target);
            const idx = try formatOperand(printer.allocator, index.index);
            defer printer.allocator.free(idx);
            try printer.printNodeFmt(is_last, "{s}Index {s}[{s}]", .{ dest_str, target, idx });
        },
        .Field => |field| {
            const target = try formatOperand(printer.allocator, field.target);
            defer printer.allocator.free(target);
            try printer.printNodeFmt(is_last, "{s}Field {s}.{s}", .{ dest_str, target, field.name });
        },
        .Array => |array| {
            var elems_buf = std.ArrayListUnmanaged(u8){};
            defer elems_buf.deinit(printer.allocator);
            for (array.elems, 0..) |elem, idx| {
                const elem_str = try formatOperand(printer.allocator, elem);
                defer printer.allocator.free(elem_str);
                try elems_buf.appendSlice(printer.allocator, elem_str);
                if (idx < array.elems.len - 1) {
                    try elems_buf.appendSlice(printer.allocator, ", ");
                }
            }
            try printer.printNodeFmt(is_last, "{s}Array [{s}]", .{ dest_str, elems_buf.items });
        },
        .StructInit => |init| {
            var fields_buf = std.ArrayListUnmanaged(u8){};
            defer fields_buf.deinit(printer.allocator);
            for (init.fields, 0..) |field, idx| {
                const val_str = try formatOperand(printer.allocator, field.value);
                defer printer.allocator.free(val_str);
                try fields_buf.appendSlice(printer.allocator, field.name);
                try fields_buf.appendSlice(printer.allocator, ": ");
                try fields_buf.appendSlice(printer.allocator, val_str);
                if (idx < init.fields.len - 1) {
                    try fields_buf.appendSlice(printer.allocator, ", ");
                }
            }
            try printer.printNodeFmt(is_last, "{s}StructInit {{ {s} }}", .{ dest_str, fields_buf.items });
        },
    }
}

/// Print a block terminator as a tree node.
fn printTerminator(printer: *TreePrinter, term: mir.TermKind, is_last: bool) AllocError!void {
    switch (term) {
        .Goto => |target| {
            try printer.printNodeFmt(is_last, "Goto Block {d}", .{target});
        },
        .If => |if_term| {
            const cond = try formatOperand(printer.allocator, if_term.cond);
            defer printer.allocator.free(cond);
            try printer.printNodeFmt(is_last, "If {s} then Block {d} else Block {d}", .{ cond, if_term.then_block, if_term.else_block });
        },
        .Ret => |val| {
            if (val) |ret_val| {
                const ret = try formatOperand(printer.allocator, ret_val);
                defer printer.allocator.free(ret);
                try printer.printNodeFmt(is_last, "Ret {s}", .{ret});
            } else {
                try printer.printNode(is_last, "Ret");
            }
        },
    }
}

/// Format an operand as a human-readable string.
fn formatOperand(allocator: std.mem.Allocator, operand: mir.Operand) AllocError![]u8 {
    return switch (operand) {
        .Temp => |t| std.fmt.allocPrint(allocator, "t{d}", .{t}),
        .Local => |l| std.fmt.allocPrint(allocator, "local{d}", .{l}),
        .Param => |p| std.fmt.allocPrint(allocator, "param{d}", .{p}),
        .RetSecond => std.fmt.allocPrint(allocator, "ret_second", .{}),
        .ImmInt => |i| std.fmt.allocPrint(allocator, "{d}", .{i}),
        .ImmFloat => |f| std.fmt.allocPrint(allocator, "{d}", .{f}),
        .ImmBool => |b| std.fmt.allocPrint(allocator, "{}", .{b}),
        .ImmChar => |c| std.fmt.allocPrint(allocator, "'{u}'", .{c}),
        .ImmString => |s| std.fmt.allocPrint(allocator, "\"{s}\"", .{s}),
        .Symbol => |s| std.fmt.allocPrint(allocator, "@{s}", .{s}),
        .Global => |g| std.fmt.allocPrint(allocator, "global{d}", .{g}),
    };
}

test "printCrateTree prints empty crate" {
    const allocator = std.testing.allocator;
    var crate = mir.MirCrate.init(allocator);
    defer crate.deinit();

    try printCrateTree(allocator, &crate);
}

test "printCrateTree prints function with blocks" {
    const allocator = std.testing.allocator;
    var crate = mir.MirCrate.init(allocator);
    defer crate.deinit();

    // Create a simple function with one block
    const blocks = try crate.allocator().alloc(mir.Block, 1);
    blocks[0] = .{
        .insts = &[_]mir.Inst{},
        .term = .{ .Ret = null },
    };

    try crate.fns.append(crate.allocator(), .{
        .name = "main",
        .params = &[_]mir.LocalId{},
        .locals = &[_]mir.MirType{},
        .ret_ty = null,
        .blocks = blocks,
    });

    try printCrateTree(allocator, &crate);
}
