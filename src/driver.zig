const std = @import("std");
const diag = @import("diag/diagnostics.zig");
const source_map = @import("diag/source_map.zig");
const lexer = @import("frontend/lexer.zig");
const parser = @import("frontend/parser.zig");
const ast = @import("frontend/ast.zig");
const tokens = @import("frontend/tokens.zig");
const ast_printer = @import("frontend/ast_printer.zig");
const hir = @import("hir/hir.zig");
const hir_printer = @import("hir/hir_printer.zig");
const mir = @import("mir/mir.zig");
const mir_lower = @import("mir/lower.zig");
const mir_passes = @import("mir/passes/passes.zig");
const mir_printer = @import("mir/mir_printer.zig");
const backend = @import("backend/backend.zig");
const ChildProcess = std.process.Child;

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
    backend_artifact: ?backend.Artifact = null,

    pub fn deinit(self: *CompileResult) void {
        self.diagnostics.deinit();
        self.source_map.deinit();
        self.ast_arena.deinit();
        self.hir.deinit();
        self.mir.deinit();
        if (self.backend_artifact) |*artifact| {
            artifact.deinit();
        }
    }
};

pub const OptLevel = enum { none, basic };
pub const Emit = enum { assembly, object };

pub const CompileOptions = struct {
    allocator: std.mem.Allocator,
    input_path: []const u8,
    output_path: []const u8,
    opt_level: OptLevel = .basic,
    emit: Emit = .assembly,
    emit_diagnostics: bool = true,
    exit_on_error: bool = true,
    source_override: ?[]const u8 = null,
    visualize_tokens: bool = false,
    visualize_ast: bool = false,
    visualize_hir: bool = false,
    visualize_mir: bool = false,
    visualize_passes: bool = false,
};
pub fn compileFile(options: CompileOptions) !CompileResult {
    // Create a fresh source map. This tracks all loaded files,
    // their contents, and their mapping between byte offsets and line/column.
    var sm = source_map.SourceMap.init(options.allocator);

    // Central diagnostics accumulator. All stages push errors/warnings here.
    var diagnostics = diag.Diagnostics.init(options.allocator);

    // Load the source file:
    // - If the caller provided a source_override buffer, use it directly.
    // - Otherwise, read the file contents from disk into memory.
    const contents = options.source_override orelse try std.fs.cwd().readFileAlloc(
        options.allocator,
        options.input_path,
        std.math.maxInt(usize),
    );

    // If we allocated the file contents ourselves (no override), free it later.
    defer if (options.source_override == null)
        options.allocator.free(contents);

    // Register this file with the source map. This assigns it a file_id
    // used throughout lexing, parsing, and diagnostics.
    const file_id = try sm.addFile(options.input_path, contents);

    // Run the lexer. Produces a slice of tokens, stored in temporary memory.
    // `token_slice` must be freed manually.
    const token_slice = try lexer.lex(
        options.allocator,
        file_id,
        contents,
        &diagnostics,
    );
    defer options.allocator.free(token_slice);

    // Optional debugging: print tokens to stdout.
    if (options.visualize_tokens) {
        printTokens(token_slice);
    }

    // Create an arena for AST allocation. All AST nodes live here.
    var ast_arena = std.heap.ArenaAllocator.init(options.allocator);

    // Parse the token stream into an AST representing the entire crate.
    // The AST is allocated in the arena so it is freed all at once.
    const crate = parser.parseCrate(ast_arena.allocator(), token_slice, &diagnostics);

    // Optional debugging: pretty-print the AST structure.
    if (options.visualize_ast) {
        try ast_printer.printCrateTree(options.allocator, crate);
    }

    // Lower the AST into HIR (High-level Intermediate Representation).
    // HIR is a simplified, more uniform IR than the raw AST, usually
    // desugared and annotated with structural information.
    var hir_crate = try hir.lowerFromAst(
        options.allocator,
        crate,
        &diagnostics,
    );

    // Declare the MIR crate now. May be filled later or left empty on error.
    var mir_crate: mir.MirCrate = undefined;

    // === NAME RESOLUTION ===
    // Only proceed if previous stages were diagnostic-free.
    if (!diagnostics.hasErrors()) {
        try hir.performNameResolution(&hir_crate, &diagnostics);
    }

    // === TYPE CHECKING ===
    if (!diagnostics.hasErrors()) {
        try hir.performTypeCheck(&hir_crate, &diagnostics);
    }

    // === OWNERSHIP / BORROW CHECKING ===
    if (!diagnostics.hasErrors()) {
        try hir.performOwnershipCheck(&hir_crate, &diagnostics);
    }

    // Optional debugging: pretty-print the HIR structure.
    if (options.visualize_hir) {
        try hir_printer.printCrateTree(options.allocator, &hir_crate);
    }

    // === HIR → MIR LOWERING ===
    if (!diagnostics.hasErrors()) {
        mir_crate = try mir_lower.lowerFromHir(
            options.allocator,
            &hir_crate,
            &diagnostics,
        );
    } else {
        // Initialize an empty MIR crate to keep structure consistent
        // even if compilation failed earlier.
        mir_crate = mir.MirCrate.init(options.allocator);
    }

    // === MIR OPTIMIZATION PASSES ===
    // Run if optimizations requested and no errors so far.
    if (!diagnostics.hasErrors() and options.opt_level == .basic) {
        try mir_passes.runAll(
            options.allocator,
            &mir_crate,
            &diagnostics,
            options.visualize_passes,
        );
    }

    // Optional debugging: pretty-print the MIR structure.
    if (options.visualize_mir) {
        try mir_printer.printCrateTree(options.allocator, &mir_crate);
    }

    // === BACKEND CODE GENERATION ===
    // Stores assembly/object output; may remain null if codegen skipped.
    var backend_artifact: ?backend.Artifact = null;

    if (!diagnostics.hasErrors()) {
        backend_artifact = try backend.codegen(
            &mir_crate,
            options.allocator,
            &diagnostics,
        );
    }

    // === OUTPUT EMISSION ===
    if (!diagnostics.hasErrors()) {
        if (backend_artifact) |artifact| {
            switch (options.emit) {
                .assembly =>
                // Write assembly text into the output file.
                try writeArtifact(options.output_path, artifact.assembly),

                .object => emitObject(
                    options.output_path,
                    artifact.assembly,
                    options.allocator,
                    &diagnostics,
                    .{ .file_id = file_id, .start = 0, .end = 0 },
                ),
            }
        }
    }

    // Produce final compile status based on whether diagnostics collected errors.
    const status: CompileStatus =
        if (diagnostics.hasErrors()) .errors else .success;

    // If compilation failed and diagnostics should be printed, emit them now.
    if (status == .errors and options.emit_diagnostics) {
        try diagnostics.emitAll(&sm);
    }

    // Optional behavior: exit the whole process on error.
    if (status == .errors and options.exit_on_error) {
        // Clean up backend artifact if created.
        if (backend_artifact) |*artifact| {
            artifact.deinit();
        }

        // Clean up all allocated compiler structures.
        diagnostics.deinit();
        sm.deinit();
        ast_arena.deinit();
        hir_crate.deinit();
        mir_crate.deinit();

        // Terminate program with nonzero code.
        std.process.exit(1);
    }

    // Return a full CompileResult containing all internal structures,
    // regardless of success or failure. The caller owns all of these and
    // must free them appropriately when done.
    return .{
        .diagnostics = diagnostics,
        .source_map = sm,
        .status = status,
        .ast_arena = ast_arena,
        .ast = crate,
        .hir = hir_crate,
        .mir = mir_crate,
        .backend_artifact = backend_artifact,
    };
}

fn writeArtifact(path: []const u8, contents: []const u8) !void {
    var file = try std.fs.cwd().createFile(path, .{ .truncate = true });
    defer file.close();
    try file.writeAll(contents);
}

fn emitObject(
    path: []const u8,
    assembly: []const u8,
    allocator: std.mem.Allocator,
    diagnostics: *diag.Diagnostics,
    span: source_map.Span,
) void {
    var stderr_buf = std.ArrayList(u8).empty;
    defer stderr_buf.deinit(allocator);

    const result = writeObjectFromAssembly(path, assembly, allocator, &stderr_buf) catch |err| {
        const detail = if (stderr_buf.items.len > 0)
            std.mem.trimRight(u8, stderr_buf.items, &[_]u8{'\n'})
        else
            null;

        const message = if (detail) |info|
            std.fmt.allocPrint(allocator, "failed to emit object ({s}): {s}", .{ @errorName(err), info }) catch "failed to emit object"
        else
            std.fmt.allocPrint(allocator, "failed to emit object ({s})", .{@errorName(err)}) catch "failed to emit object";
        defer if (message.ptr != "failed to emit object"[0..].ptr) allocator.free(message);

        diagnostics.reportError(span, message);
        return;
    };
    defer result.cleanup(allocator);

    if (result.term != .Exited or result.term.Exited != 0) {
        const message = if (result.stderr.len > 0)
            std.fmt.allocPrint(allocator, "object writer exited with {d}: {s}", .{ result.term_code(), result.stderr }) catch "object writer exited with an error"
        else
            std.fmt.allocPrint(allocator, "object writer exited with {d}", .{result.term_code()}) catch "object writer exited with an error";
        defer if (message.ptr != "object writer exited with an error"[0..].ptr) allocator.free(message);

        diagnostics.reportError(span, message);
        return;
    }
}

const ObjectWriteResult = struct {
    term: std.process.Child.Term,
    stdout: []u8,
    stderr: []u8,

    fn cleanup(self: ObjectWriteResult, allocator: std.mem.Allocator) void {
        allocator.free(self.stdout);
        allocator.free(self.stderr);
    }

    fn term_code(self: ObjectWriteResult) u32 {
        return switch (self.term) {
            .Exited => |code| code,
            .Signal => |sig| sig,
            .Stopped => |code| code,
            .Unknown => |code| code,
        };
    }
};

fn writeObjectFromAssembly(
    path: []const u8,
    assembly: []const u8,
    allocator: std.mem.Allocator,
    stderr_buf: *std.ArrayList(u8),
) !ObjectWriteResult {
    var child = ChildProcess.init(
        &.{ "zig", "cc", "-x", "assembler", "-c", "-", "-o", path },
        allocator,
    );
    child.stdin_behavior = .Pipe;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();
    errdefer {
        _ = child.kill() catch {};
        _ = child.wait() catch {};
    }

    if (child.stdin) |stdin_file| {
        try stdin_file.writeAll(assembly);
        stdin_file.close();
        child.stdin = null;
    }

    var stdout_buf = std.ArrayList(u8).empty;
    defer stdout_buf.deinit(allocator);

    try child.collectOutput(allocator, &stdout_buf, stderr_buf, std.math.maxInt(usize));

    const term = try child.wait();

    const stdout_owned = try stdout_buf.toOwnedSlice(allocator);
    errdefer allocator.free(stdout_owned);
    const stderr_owned = try stderr_buf.toOwnedSlice(allocator);
    errdefer allocator.free(stderr_owned);

    return .{ .term = term, .stdout = stdout_owned, .stderr = stderr_owned };
}

fn printTokens(tokens_slice: []const tokens.Token) void {
    std.debug.print("Tokens ({}):\n", .{tokens_slice.len});
    for (tokens_slice, 0..) |token, idx| {
        std.debug.print("  [{d:0>3}] {s} -> '{s}' [{d}-{d}]\n", .{
            idx,
            @tagName(token.kind),
            token.lexeme,
            token.span.start,
            token.span.end,
        });
    }
}

test "compileFile succeeds on tokenizable source" {
    const allocator = std.testing.allocator;
    const out_path = "test_output.s";
    std.fs.cwd().deleteFile(out_path) catch {};

    var result = try compileFile(.{
        .allocator = allocator,
        .input_path = "test.rs",
        .output_path = out_path,
        .opt_level = .basic,
        .emit = .assembly,
        .emit_diagnostics = false,
        .exit_on_error = false,
        .source_override = "fn main() {}",
    });
    defer {
        result.deinit();
        std.fs.cwd().deleteFile(out_path) catch {};
    }

    try std.testing.expectEqual(CompileStatus.success, result.status);
    try std.testing.expect(!result.diagnostics.hasErrors());
    try std.testing.expect(result.mir.fns.items.len > 0);
    try std.testing.expect(result.backend_artifact != null);
    const artifact = result.backend_artifact.?;
    try std.testing.expect(artifact.assembly.len > 0);
}

test "compileFile records diagnostics on lexer errors" {
    const allocator = std.testing.allocator;
    const out_path = "lex_error.s";
    std.fs.cwd().deleteFile(out_path) catch {};

    var result = try compileFile(.{
        .allocator = allocator,
        .input_path = "test.rs",
        .output_path = out_path,
        .opt_level = .basic,
        .emit = .assembly,
        .emit_diagnostics = false,
        .exit_on_error = false,
        .source_override = "\"unterminated",
    });
    defer {
        result.deinit();
        std.fs.cwd().deleteFile(out_path) catch {};
    }

    try std.testing.expectEqual(CompileStatus.errors, result.status);
    try std.testing.expect(result.diagnostics.hasErrors());
}

test "compileFile respects opt level none" {
    const allocator = std.testing.allocator;
    const out_path = "opt_none.s";
    std.fs.cwd().deleteFile(out_path) catch {};

    var result = try compileFile(.{
        .allocator = allocator,
        .input_path = "test.rs",
        .output_path = out_path,
        .opt_level = .none,
        .emit = .assembly,
        .emit_diagnostics = false,
        .exit_on_error = false,
        .source_override = "fn main() { 2 + 3; }",
    });
    defer {
        result.deinit();
        std.fs.cwd().deleteFile(out_path) catch {};
    }

    try std.testing.expectEqual(CompileStatus.success, result.status);
    const func = result.mir.fns.items[0];
    try std.testing.expect(func.blocks[0].insts.len > 0);
}

test "compileFile applies basic optimizations" {
    const allocator = std.testing.allocator;
    const out_path = "opt_basic.s";
    std.fs.cwd().deleteFile(out_path) catch {};

    var result = try compileFile(.{
        .allocator = allocator,
        .input_path = "test.rs",
        .output_path = out_path,
        .opt_level = .basic,
        .emit = .assembly,
        .emit_diagnostics = false,
        .exit_on_error = false,
        .source_override = "fn main() { 2 + 3; }",
    });
    defer {
        result.deinit();
        std.fs.cwd().deleteFile(out_path) catch {};
    }

    try std.testing.expectEqual(CompileStatus.success, result.status);
    const func = result.mir.fns.items[0];
    try std.testing.expectEqual(@as(usize, 0), func.blocks[0].insts.len);
}

test "compileFile writes backend artifact to file" {
    const allocator = std.testing.allocator;
    const out_path = "emitted_output.s";
    std.fs.cwd().deleteFile(out_path) catch {};

    var result = try compileFile(.{
        .allocator = allocator,
        .input_path = "test.rs",
        .output_path = out_path,
        .opt_level = .basic,
        .emit = .assembly,
        .emit_diagnostics = false,
        .exit_on_error = false,
        .source_override = "fn main() {}",
    });
    defer {
        std.fs.cwd().deleteFile(out_path) catch {};
        result.deinit();
    }

    try std.testing.expectEqual(CompileStatus.success, result.status);
    const artifact = result.backend_artifact.?;

    const contents = try std.fs.cwd().readFileAlloc(allocator, out_path, std.math.maxInt(usize));
    defer allocator.free(contents);
    try std.testing.expectEqualSlices(u8, artifact.assembly, contents);
}

test "compileFile emits object file" {
    const allocator = std.testing.allocator;
    const out_path = "emitted_output.o";
    std.fs.cwd().deleteFile(out_path) catch {};

    var result = try compileFile(.{
        .allocator = allocator,
        .input_path = "test.rs",
        .output_path = out_path,
        .opt_level = .basic,
        .emit = .object,
        .emit_diagnostics = false,
        .exit_on_error = false,
        .source_override = "fn main() {}",
    });
    defer {
        std.fs.cwd().deleteFile(out_path) catch {};
        result.deinit();
    }

    try std.testing.expectEqual(CompileStatus.success, result.status);
    const contents = try std.fs.cwd().readFileAlloc(allocator, out_path, std.math.maxInt(usize));
    defer allocator.free(contents);
    try std.testing.expect(contents.len >= 4);
    try std.testing.expect(std.mem.eql(u8, contents[0..4], "\x7fELF"));
}

test "compileFile surfaces diagnostics when object emission fails" {
    const allocator = std.testing.allocator;
    const out_path = "missing_dir/out.o";
    std.fs.cwd().deleteFile(out_path) catch {};

    var result = try compileFile(.{
        .allocator = allocator,
        .input_path = "test.rs",
        .output_path = out_path,
        .opt_level = .basic,
        .emit = .object,
        .emit_diagnostics = false,
        .exit_on_error = false,
        .source_override = "fn main() {}",
    });
    defer result.deinit();

    try std.testing.expectEqual(CompileStatus.errors, result.status);
    try std.testing.expect(result.diagnostics.hasErrors());
    try std.testing.expectError(error.FileNotFound, std.fs.cwd().openFile(out_path, .{}));
}

test "compileFile handles unsafe block with statements" {
    const allocator = std.testing.allocator;
    const out_path = "unsafe_stmts.s";
    std.fs.cwd().deleteFile(out_path) catch {};

    var result = try compileFile(.{
        .allocator = allocator,
        .input_path = "test.rs",
        .output_path = out_path,
        .opt_level = .basic,
        .emit = .assembly,
        .emit_diagnostics = false,
        .exit_on_error = false,
        .source_override =
        \\fn main() {
        \\    let mut value: i32 = 10;
        \\    unsafe {
        \\        value = 20;
        \\    }
        \\    println!("{}", value);
        \\}
        ,
    });
    defer {
        result.deinit();
        std.fs.cwd().deleteFile(out_path) catch {};
    }

    // Check AST structure: 3 statements, no tail expression
    const fn_item = result.ast.items[0].data.Fn;
    try std.testing.expectEqual(@as(usize, 3), fn_item.body.stmts.len);
    try std.testing.expect(fn_item.body.result == null);
    try std.testing.expectEqual(CompileStatus.success, result.status);
}
