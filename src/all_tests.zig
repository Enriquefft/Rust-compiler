const std = @import("std");

// Import every module with tests so that `zig build test` exercises them in one place.
test "all modules" {
    _ = @import("driver.zig");
    _ = @import("frontend/ast.zig");
    _ = @import("frontend/lexer.zig");
    _ = @import("frontend/parser.zig");
    _ = @import("frontend/tokens.zig");
    _ = @import("diag/diagnostics.zig");
    _ = @import("diag/source_map.zig");
    _ = @import("backend/backend.zig");
    _ = @import("backend/x86_64/emitter.zig");
    _ = @import("hir/hir.zig");
    _ = @import("hir/name_res.zig");
    _ = @import("hir/typecheck.zig");
    _ = @import("mir/mir.zig");
    _ = @import("mir/lower.zig");
    _ = @import("mir/passes/passes.zig");
    _ = @import("parser_tests.zig");
    _ = @import("main.zig");
    _ = @import("e2e_println.zig");
}
