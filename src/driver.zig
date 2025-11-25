const std = @import("std");
const diag = @import("diag/diagnostics.zig");
const source_map = @import("diag/source_map.zig");
const lexer = @import("frontend/lexer.zig");
const parser = @import("frontend/parser.zig");
const ast = @import("frontend/ast.zig");
const hir = @import("hir/hir.zig");
const mir = @import("mir/mir.zig");
const mir_lower = @import("mir/lower.zig");

pub const CompileStatus = enum {
    success,
    errors,
};

pub const CompileResult = struct {
    diagnostics: diag.Diagnostics,
    source_map: source_map.SourceMap,
    status: CompileStatus,
    ast_arena: std.heap.ArenaAllocator,
    ast: ast.Crate,
    hir: hir.Crate,
    mir: mir.MirCrate,

    pub fn deinit(self: *CompileResult) void {
        self.diagnostics.deinit();
        self.source_map.deinit();
        self.ast_arena.deinit();
        self.hir.deinit();
        self.mir.deinit();
    }
};

pub const CompileOptions = struct {
    allocator: std.mem.Allocator,
    input_path: []const u8,
    emit_diagnostics: bool = true,
    exit_on_error: bool = true,
    source_override: ?[]const u8 = null,
};

pub fn compileFile(options: CompileOptions) !CompileResult {
    var sm = source_map.SourceMap.init(options.allocator);
    var diagnostics = diag.Diagnostics.init(options.allocator);

    const contents = options.source_override orelse try std.fs.cwd().readFileAlloc(options.allocator, options.input_path, std.math.maxInt(usize));
    defer if (options.source_override == null) options.allocator.free(contents);

    const file_id = try sm.addFile(options.input_path, contents);

    const tokens = try lexer.lex(options.allocator, file_id, contents, &diagnostics);
    defer options.allocator.free(tokens);

    var ast_arena = std.heap.ArenaAllocator.init(options.allocator);
    const crate = parser.parseCrate(ast_arena.allocator(), tokens, &diagnostics);

    var hir_crate = try hir.lowerFromAst(options.allocator, crate, &diagnostics);
    var mir_crate: mir.MirCrate = undefined;

    if (!diagnostics.hasErrors()) {
        try hir.performNameResolution(&hir_crate, &diagnostics);
    }

    if (!diagnostics.hasErrors()) {
        try hir.performTypeCheck(&hir_crate, &diagnostics);
    }

    if (!diagnostics.hasErrors()) {
        mir_crate = try mir_lower.lowerFromHir(options.allocator, &hir_crate, &diagnostics);
    } else {
        mir_crate = mir.MirCrate.init(options.allocator);
    }

    // TODO: MIR optimization passes and backend code generation.

    const status: CompileStatus = if (diagnostics.hasErrors()) .errors else .success;

    if (status == .errors and options.emit_diagnostics) {
        try diagnostics.emitAll(&sm);
    }

    if (status == .errors and options.exit_on_error) {
        diagnostics.deinit();
        sm.deinit();
        ast_arena.deinit();
        hir_crate.deinit();
        mir_crate.deinit();
        std.process.exit(1);
    }

    return .{
        .diagnostics = diagnostics,
        .source_map = sm,
        .status = status,
        .ast_arena = ast_arena,
        .ast = crate,
        .hir = hir_crate,
        .mir = mir_crate,
    };
}

test "compileFile succeeds on tokenizable source" {
    const allocator = std.testing.allocator;
    var result = try compileFile(.{
        .allocator = allocator,
        .input_path = "test.rs",
        .emit_diagnostics = false,
        .exit_on_error = false,
        .source_override = "fn main() {}",
    });
    defer result.deinit();

    try std.testing.expectEqual(CompileStatus.success, result.status);
    try std.testing.expect(!result.diagnostics.hasErrors());
    try std.testing.expect(result.mir.fns.items.len > 0);
}

test "compileFile records diagnostics on lexer errors" {
    const allocator = std.testing.allocator;
    var result = try compileFile(.{
        .allocator = allocator,
        .input_path = "test.rs",
        .emit_diagnostics = false,
        .exit_on_error = false,
        .source_override = "\"unterminated",
    });
    defer result.deinit();

    try std.testing.expectEqual(CompileStatus.errors, result.status);
    try std.testing.expect(result.diagnostics.hasErrors());
}
