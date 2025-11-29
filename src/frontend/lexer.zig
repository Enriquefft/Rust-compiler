const std = @import("std");
const tokens = @import("tokens.zig");
const Token = tokens.Token;
const TokenKind = tokens.TokenKind;
const keywordKind = tokens.keywordKind;
const diag = @import("../diag/diagnostics.zig");
const source_map = @import("../diag/source_map.zig");
const Span = source_map.Span;
const FileId = source_map.FileId;

pub fn lex(allocator: std.mem.Allocator, file_id: FileId, src: []const u8, diagnostics: *diag.Diagnostics) ![]Token {
    var list = std.ArrayListUnmanaged(Token){};
    defer list.deinit(allocator);

    var i: usize = 0;
    while (i < src.len) {
        const c = src[i];
        switch (c) {
            ' ', '\t', '\r', '\n' => i += 1,
            '/' => {
                if (matchPrefix(src, i, "//")) {
                    i = skipLineComment(src, i + 2);
                } else if (matchPrefix(src, i, "/*")) {
                    i = skipBlockComment(src, i + 2, diagnostics, file_id);
                } else if (matchPrefix(src, i, "/=")) {
                    try appendSimple(&list, allocator, TokenKind.SlashEq, file_id, i, i + 2, src);
                    i += 2;
                } else {
                    try appendSimple(&list, allocator, TokenKind.Slash, file_id, i, i + 1, src);
                    i += 1;
                }
            },
            '(' => {
                try appendSimple(&list, allocator, .LParen, file_id, i, i + 1, src);
                i += 1;
            },
            ')' => {
                try appendSimple(&list, allocator, .RParen, file_id, i, i + 1, src);
                i += 1;
            },
            '{' => {
                try appendSimple(&list, allocator, .LBrace, file_id, i, i + 1, src);
                i += 1;
            },
            '}' => {
                try appendSimple(&list, allocator, .RBrace, file_id, i, i + 1, src);
                i += 1;
            },
            '[' => {
                try appendSimple(&list, allocator, .LBracket, file_id, i, i + 1, src);
                i += 1;
            },
            ']' => {
                try appendSimple(&list, allocator, .RBracket, file_id, i, i + 1, src);
                i += 1;
            },
            ',' => {
                try appendSimple(&list, allocator, .Comma, file_id, i, i + 1, src);
                i += 1;
            },
            ';' => {
                try appendSimple(&list, allocator, .Semicolon, file_id, i, i + 1, src);
                i += 1;
            },
            ':' => {
                if (matchPrefix(src, i, "::")) {
                    try appendSimple(&list, allocator, .DoubleColon, file_id, i, i + 2, src);
                    i += 2;
                } else {
                    try appendSimple(&list, allocator, .Colon, file_id, i, i + 1, src);
                    i += 1;
                }
            },
            '-' => {
                if (matchPrefix(src, i, "->")) {
                    try appendSimple(&list, allocator, .Arrow, file_id, i, i + 2, src);
                    i += 2;
                } else if (matchPrefix(src, i, "-=")) {
                    try appendSimple(&list, allocator, .MinusEq, file_id, i, i + 2, src);
                    i += 2;
                } else {
                    try appendSimple(&list, allocator, .Minus, file_id, i, i + 1, src);
                    i += 1;
                }
            },
            '+' => {
                if (matchPrefix(src, i, "+=")) {
                    try appendSimple(&list, allocator, .PlusEq, file_id, i, i + 2, src);
                    i += 2;
                } else {
                    try appendSimple(&list, allocator, .Plus, file_id, i, i + 1, src);
                    i += 1;
                }
            },
            '*' => {
                if (matchPrefix(src, i, "*=")) {
                    try appendSimple(&list, allocator, .StarEq, file_id, i, i + 2, src);
                    i += 2;
                } else {
                    try appendSimple(&list, allocator, .Star, file_id, i, i + 1, src);
                    i += 1;
                }
            },
            '%' => {
                try appendSimple(&list, allocator, .Percent, file_id, i, i + 1, src);
                i += 1;
            },
            '=' => {
                if (matchPrefix(src, i, "==")) {
                    try appendSimple(&list, allocator, .DoubleEq, file_id, i, i + 2, src);
                    i += 2;
                } else {
                    try appendSimple(&list, allocator, .Eq, file_id, i, i + 1, src);
                    i += 1;
                }
            },
            '!' => {
                if (matchPrefix(src, i, "!=")) {
                    try appendSimple(&list, allocator, .BangEq, file_id, i, i + 2, src);
                    i += 2;
                } else {
                    try appendSimple(&list, allocator, .Bang, file_id, i, i + 1, src);
                    i += 1;
                }
            },
            '<' => {
                if (matchPrefix(src, i, "<=")) {
                    try appendSimple(&list, allocator, .Le, file_id, i, i + 2, src);
                    i += 2;
                } else {
                    try appendSimple(&list, allocator, .Lt, file_id, i, i + 1, src);
                    i += 1;
                }
            },
            '>' => {
                if (matchPrefix(src, i, ">=")) {
                    try appendSimple(&list, allocator, .Ge, file_id, i, i + 2, src);
                    i += 2;
                } else {
                    try appendSimple(&list, allocator, .Gt, file_id, i, i + 1, src);
                    i += 1;
                }
            },
            '.' => {
                if (matchPrefix(src, i, "..=")) {
                    try appendSimple(&list, allocator, .DotDotEq, file_id, i, i + 3, src);
                    i += 3;
                } else if (matchPrefix(src, i, "..")) {
                    try appendSimple(&list, allocator, .DotDot, file_id, i, i + 2, src);
                    i += 2;
                } else {
                    try appendSimple(&list, allocator, .Dot, file_id, i, i + 1, src);
                    i += 1;
                }
            },
            '&' => {
                if (matchPrefix(src, i, "&&")) {
                    try appendSimple(&list, allocator, .DoubleAmp, file_id, i, i + 2, src);
                    i += 2;
                } else {
                    try appendSimple(&list, allocator, .Amp, file_id, i, i + 1, src);
                    i += 1;
                }
            },
            '|' => {
                if (matchPrefix(src, i, "||")) {
                    try appendSimple(&list, allocator, .DoublePipe, file_id, i, i + 2, src);
                    i += 2;
                } else {
                    try appendSimple(&list, allocator, .Pipe, file_id, i, i + 1, src);
                    i += 1;
                }
            },
            '0'...'9' => {
                const start = i;
                var end = scanNumber(src, i);
                const is_float = end < src.len and src[end] == '.' and end + 1 < src.len and std.ascii.isDigit(src[end + 1]);
                if (is_float) {
                    end = scanFloatFraction(src, end + 1);
                    try appendSimple(&list, allocator, .FloatLit, file_id, start, end, src);
                } else {
                    // Check for integer suffix (u8, u16, u32, u64, usize, i8, i16, i32, i64, isize)
                    end = scanIntegerSuffix(src, end);
                    try appendSimple(&list, allocator, .IntLit, file_id, start, end, src);
                }
                i = end;
            },
            'a'...'z', 'A'...'Z', '_' => {
                const start = i;
                i = scanIdent(src, i);
                const lexeme = src[start..i];
                const kind = keywordKind(lexeme) orelse .Identifier;
                const final_kind = switch (kind) {
                    .KwTrue, .KwFalse => TokenKind.BoolLit,
                    else => kind,
                };
                try appendSimple(&list, allocator, final_kind, file_id, start, i, src);
            },
            '"' => {
                const start = i;
                i += 1;
                while (i < src.len and src[i] != '"') : (i += 1) {
                    if (src[i] == '\\' and i + 1 < src.len) i += 1; // skip escaped char
                }
                if (i >= src.len) {
                    diagnostics.reportError(.{ .file_id = file_id, .start = start, .end = src.len }, "unterminated string literal");
                    break;
                }
                i += 1; // closing quote
                try appendSimple(&list, allocator, .StringLit, file_id, start, i, src);
            },
            '\'' => {
                const start = i;
                if (i + 2 >= src.len or src[i + 2] != '\'') {
                    diagnostics.reportError(.{ .file_id = file_id, .start = start, .end = @min(i + 3, src.len) }, "invalid char literal");
                    i += 1;
                    continue;
                }
                i += 3; // '\'', char, '\''
                try appendSimple(&list, allocator, .CharLit, file_id, start, i, src);
            },
            else => {
                diagnostics.reportError(.{ .file_id = file_id, .start = i, .end = i + 1 }, "unexpected character");
                i += 1;
            },
        }
    }
    return try list.toOwnedSlice(allocator);
}

fn appendSimple(list: *std.ArrayListUnmanaged(Token), allocator: std.mem.Allocator, kind: TokenKind, file_id: FileId, start: usize, end: usize, src: []const u8) !void {
    try list.append(allocator, .{ .kind = kind, .lexeme = src[start..end], .span = .{ .file_id = file_id, .start = start, .end = end } });
}

fn scanIdent(src: []const u8, start: usize) usize {
    var i = start;
    while (i < src.len) : (i += 1) {
        const c = src[i];
        if (!(std.ascii.isAlphanumeric(c) or c == '_')) break;
    }
    return i;
}

fn scanNumber(src: []const u8, start: usize) usize {
    var i = start;
    while (i < src.len and std.ascii.isDigit(src[i])) : (i += 1) {}
    return i;
}

fn scanFloatFraction(src: []const u8, start: usize) usize {
    var i = start;
    while (i < src.len and std.ascii.isDigit(src[i])) : (i += 1) {}
    return i;
}

/// Scans for integer literal suffixes like u8, u16, u32, u64, usize, i8, i16, i32, i64, isize.
/// Returns the end position after any valid suffix.
fn scanIntegerSuffix(src: []const u8, start: usize) usize {
    // Valid integer suffixes
    const suffixes = [_][]const u8{
        "usize", "isize", // Must check longer suffixes first
        "u64", "u32", "u16", "u8",
        "i64", "i32", "i16", "i8",
    };

    for (suffixes) |suffix| {
        if (matchPrefix(src, start, suffix)) {
            // Make sure the suffix is not part of a longer identifier
            const end = start + suffix.len;
            if (end >= src.len or !std.ascii.isAlphanumeric(src[end]) and src[end] != '_') {
                return end;
            }
        }
    }
    return start;
}

fn matchPrefix(src: []const u8, idx: usize, pat: []const u8) bool {
    if (idx + pat.len > src.len) return false;
    return std.mem.eql(u8, src[idx .. idx + pat.len], pat);
}

fn skipLineComment(src: []const u8, start: usize) usize {
    var i = start;
    while (i < src.len and src[i] != '\n') : (i += 1) {}
    return i;
}

fn skipBlockComment(src: []const u8, start: usize, diagnostics: *diag.Diagnostics, file_id: FileId) usize {
    var depth: usize = 1;
    var i = start;
    while (i < src.len) : (i += 1) {
        if (matchPrefix(src, i, "/*")) {
            depth += 1;
            i += 1;
        } else if (matchPrefix(src, i, "*/")) {
            depth -= 1;
            i += 1;
            if (depth == 0) return i + 1;
        }
    }
    diagnostics.reportError(.{ .file_id = file_id, .start = start - 2, .end = src.len }, "unterminated block comment");
    return src.len;
}

test "lex identifiers and keywords" {
    const allocator = std.testing.allocator;
    var diags = diag.Diagnostics.init(allocator);
    defer diags.deinit();

    const file_id: FileId = 0;
    const toks = try lex(allocator, file_id, "fn main mut x", &diags);
    defer allocator.free(toks);

    try std.testing.expectEqual(@as(usize, 4), toks.len);
    try std.testing.expectEqual(TokenKind.KwFn, toks[0].kind);
    try std.testing.expectEqualSlices(u8, "fn", toks[0].lexeme);
    try std.testing.expectEqual(TokenKind.Identifier, toks[1].kind);
    try std.testing.expectEqual(TokenKind.KwMut, toks[2].kind);
    try std.testing.expectEqual(TokenKind.Identifier, toks[3].kind);
}

test "lex const keyword" {
    const allocator = std.testing.allocator;
    var diags = diag.Diagnostics.init(allocator);
    defer diags.deinit();

    const toks = try lex(allocator, 0, "const MAX: usize = 100;", &diags);
    defer allocator.free(toks);

    try std.testing.expectEqual(@as(usize, 7), toks.len);
    try std.testing.expectEqual(TokenKind.KwConst, toks[0].kind);
    try std.testing.expectEqualSlices(u8, "const", toks[0].lexeme);
    try std.testing.expectEqual(TokenKind.Identifier, toks[1].kind);
    try std.testing.expectEqual(TokenKind.Colon, toks[2].kind);
    try std.testing.expectEqual(TokenKind.Identifier, toks[3].kind);
    try std.testing.expectEqual(TokenKind.Eq, toks[4].kind);
    try std.testing.expectEqual(TokenKind.IntLit, toks[5].kind);
    try std.testing.expectEqual(TokenKind.Semicolon, toks[6].kind);
}

test "lex unsafe keyword" {
    const allocator = std.testing.allocator;
    var diags = diag.Diagnostics.init(allocator);
    defer diags.deinit();

    const toks = try lex(allocator, 0, "unsafe { x }", &diags);
    defer allocator.free(toks);

    try std.testing.expectEqual(@as(usize, 4), toks.len);
    try std.testing.expectEqual(TokenKind.KwUnsafe, toks[0].kind);
    try std.testing.expectEqualSlices(u8, "unsafe", toks[0].lexeme);
    try std.testing.expectEqual(TokenKind.LBrace, toks[1].kind);
    try std.testing.expectEqual(TokenKind.Identifier, toks[2].kind);
    try std.testing.expectEqual(TokenKind.RBrace, toks[3].kind);
}

test "lex numbers and operators" {
    const allocator = std.testing.allocator;
    var diags = diag.Diagnostics.init(allocator);
    defer diags.deinit();

    const toks = try lex(allocator, 0, "1 + 2.5 != 3", &diags);
    defer allocator.free(toks);

    try std.testing.expectEqual(TokenKind.IntLit, toks[0].kind);
    try std.testing.expectEqual(TokenKind.Plus, toks[1].kind);
    try std.testing.expectEqual(TokenKind.FloatLit, toks[2].kind);
    try std.testing.expectEqual(TokenKind.BangEq, toks[3].kind);
    try std.testing.expectEqual(TokenKind.IntLit, toks[4].kind);
}

test "lex comments are skipped" {
    const allocator = std.testing.allocator;
    var diags = diag.Diagnostics.init(allocator);
    defer diags.deinit();

    const src = "let x = 1; // comment\n/* block */ let y = 2;";
    const toks = try lex(allocator, 0, src, &diags);
    defer allocator.free(toks);

    try std.testing.expectEqual(@as(usize, 10), toks.len);
    try std.testing.expectEqual(TokenKind.KwLet, toks[0].kind);
    try std.testing.expectEqual(TokenKind.Identifier, toks[1].kind);
    try std.testing.expectEqual(TokenKind.Eq, toks[2].kind);
    try std.testing.expectEqual(TokenKind.IntLit, toks[3].kind);
    try std.testing.expectEqual(TokenKind.Semicolon, toks[4].kind);
}

const Fixture = struct {
    file_name: []const u8,
    expected_kinds: []const TokenKind,
};

fn runFixture(allocator: std.mem.Allocator, fixture: Fixture) !void {
    const path = try std.fs.path.join(allocator, &[_][]const u8{ "codes", fixture.file_name });
    defer allocator.free(path);

    const contents = try std.fs.cwd().readFileAlloc(allocator, path, std.math.maxInt(usize));
    defer allocator.free(contents);

    var diags = diag.Diagnostics.init(allocator);
    defer diags.deinit();

    const tokens_result = try lex(allocator, 0, contents, &diags);
    defer allocator.free(tokens_result);

    try std.testing.expect(!diags.hasErrors());
    try std.testing.expectEqual(fixture.expected_kinds.len, tokens_result.len);

    for (fixture.expected_kinds, 0..) |expected, idx| {
        try std.testing.expectEqual(expected, tokens_result[idx].kind);
    }
}

test "lex samples from codes inputs" {
    const allocator = std.testing.allocator;

    const fixtures = [_]Fixture{
        .{
            .file_name = "input1.rs",
            .expected_kinds = &[_]TokenKind{
                .KwFn,
                .Identifier,
                .LParen,
                .RParen,
                .LBrace,
                .KwLet,
                .KwMut,
                .Identifier,
                .Colon,
                .Identifier,
                .Semicolon,
                .KwLet,
                .KwMut,
                .Identifier,
                .Colon,
                .Identifier,
                .Semicolon,
                .KwLet,
                .KwMut,
                .Identifier,
                .Colon,
                .Identifier,
                .Semicolon,
                .Identifier,
                .Eq,
                .IntLit,
                .Semicolon,
                .Identifier,
                .Eq,
                .IntLit,
                .Semicolon,
                .Identifier,
                .Eq,
                .IntLit,
                .Semicolon,
                .Identifier,
                .Eq,
                .IntLit,
                .Semicolon,
                .Identifier,
                .Bang,
                .LParen,
                .StringLit,
                .Comma,
                .Identifier,
                .RParen,
                .Semicolon,
                .Identifier,
                .Bang,
                .LParen,
                .StringLit,
                .Comma,
                .Identifier,
                .RParen,
                .Semicolon,
                .Identifier,
                .Bang,
                .LParen,
                .StringLit,
                .Comma,
                .Identifier,
                .RParen,
                .Semicolon,
                .RBrace,
            },
        },
        .{
            .file_name = "input2.rs",
            .expected_kinds = &[_]TokenKind{
                .KwFn,
                .Identifier,
                .LParen,
                .RParen,
                .LBrace,
                .KwLet,
                .KwMut,
                .Identifier,
                .Colon,
                .Identifier,
                .Semicolon,
                .KwLet,
                .KwMut,
                .Identifier,
                .Colon,
                .Identifier,
                .Semicolon,
                .Identifier,
                .Eq,
                .IntLit,
                .Semicolon,
                .Identifier,
                .Eq,
                .IntLit,
                .Semicolon,
                .KwIf,
                .Identifier,
                .Gt,
                .Identifier,
                .LBrace,
                .Identifier,
                .Bang,
                .LParen,
                .StringLit,
                .Comma,
                .Identifier,
                .RParen,
                .Semicolon,
                .RBrace,
                .KwElse,
                .LBrace,
                .Identifier,
                .Bang,
                .LParen,
                .StringLit,
                .Comma,
                .Identifier,
                .RParen,
                .Semicolon,
                .RBrace,
                .RBrace,
            },
        },
        .{
            .file_name = "input3.rs",
            .expected_kinds = &[_]TokenKind{
                .KwFn,
                .Identifier,
                .LParen,
                .RParen,
                .LBrace,
                .KwLet,
                .KwMut,
                .Identifier,
                .Colon,
                .Identifier,
                .Semicolon,
                .Identifier,
                .Eq,
                .IntLit,
                .Semicolon,
                .KwFor,
                .Identifier,
                .KwIn,
                .IntLit,
                .DotDot,
                .IntLit,
                .LBrace,
                .Identifier,
                .PlusEq,
                .Identifier,
                .Semicolon,
                .RBrace,
                .Identifier,
                .Bang,
                .LParen,
                .StringLit,
                .Comma,
                .Identifier,
                .RParen,
                .Semicolon,
                .RBrace,
            },
        },
        .{
            .file_name = "input4.rs",
            .expected_kinds = &[_]TokenKind{
                .KwFn,
                .Identifier,
                .LParen,
                .Identifier,
                .Colon,
                .Identifier,
                .Comma,
                .Identifier,
                .Colon,
                .Identifier,
                .RParen,
                .Arrow,
                .Identifier,
                .LBrace,
                .Identifier,
                .Plus,
                .Identifier,
                .RBrace,
                .KwFn,
                .Identifier,
                .LParen,
                .RParen,
                .LBrace,
                .KwLet,
                .KwMut,
                .Identifier,
                .Colon,
                .Identifier,
                .Semicolon,
                .KwLet,
                .KwMut,
                .Identifier,
                .Colon,
                .Identifier,
                .Semicolon,
                .Identifier,
                .Eq,
                .IntLit,
                .Semicolon,
                .Identifier,
                .Eq,
                .IntLit,
                .Semicolon,
                .Identifier,
                .Bang,
                .LParen,
                .StringLit,
                .Comma,
                .Identifier,
                .LParen,
                .Identifier,
                .Comma,
                .Identifier,
                .RParen,
                .RParen,
                .Semicolon,
                .RBrace,
            },
        },
        .{
            .file_name = "macros_test1.rs",
            .expected_kinds = &[_]TokenKind{
                .KwFn,
                .Identifier,
                .LParen,
                .RParen,
                .LBrace,
                // println!("Hello, World!");
                .Identifier,
                .Bang,
                .LParen,
                .StringLit,
                .RParen,
                .Semicolon,
                .RBrace,
            },
        },
    };

    for (fixtures) |fixture| {
        try runFixture(allocator, fixture);
    }
}
