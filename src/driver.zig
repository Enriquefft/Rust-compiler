const std = @import("std");
const diag = @import("diag/diagnostics.zig");
const source_map = @import("diag/source_map.zig");
const lexer = @import("frontend/lexer.zig");

pub const CompileStatus = enum {
    success,
    errors,
};

pub const CompileResult = struct {
    diagnostics: diag.Diagnostics,
    source_map: source_map.SourceMap,
    status: CompileStatus,

    pub fn deinit(self: *CompileResult) void {
        self.diagnostics.deinit();
        self.source_map.deinit();
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

    // TODO: Frontend parsing to AST per Architecture.md.
    // TODO: Lowering to HIR and semantic analysis.
    // TODO: MIR construction and optimization passes.
    // TODO: Backend code generation and emission.

    const status: CompileStatus = if (diagnostics.hasErrors()) .errors else .success;

    if (status == .errors and options.emit_diagnostics) {
        try diagnostics.emitAll(&sm);
    }

    if (status == .errors and options.exit_on_error) {
        diagnostics.deinit();
        sm.deinit();
        std.process.exit(1);
    }

    return .{ .diagnostics = diagnostics, .source_map = sm, .status = status };
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
