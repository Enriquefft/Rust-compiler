const std = @import("std");

// Import parser module to run its internal tests with module root anchored at src/.
test "parser module" {
    _ = @import("frontend/parser.zig");
}
