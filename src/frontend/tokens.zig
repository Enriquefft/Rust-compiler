const std = @import("std");
const source_map = @import("../diag/source_map.zig");

pub const TokenKind = enum {
    Identifier,
    IntLit,
    FloatLit,
    BoolLit,
    StringLit,
    CharLit,

    // Keywords
    KwFn,
    KwStruct,
    KwImpl,
    KwType,
    KwLet,
    KwMut,
    KwIf,
    KwElse,
    KwWhile,
    KwFor,
    KwIn,
    KwReturn,
    KwAs,
    KwSelf,
    KwTrue,
    KwFalse,
    KwConst,
    KwUnsafe,

    // Punctuation / operators
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Semicolon,
    Colon,
    DoubleColon,
    Arrow,
    Dot,
    DotDot,
    DotDotEq,
    Bang,
    Amp,
    DoubleAmp,
    Pipe,
    DoublePipe,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Eq,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    DoubleEq,
    BangEq,
    Lt,
    Le,
    Gt,
    Ge,
};

pub const Token = struct {
    kind: TokenKind,
    lexeme: []const u8, // slice into original source
    span: source_map.Span,
};

pub fn keywordKind(ident: []const u8) ?TokenKind {
    const keywords = [_]struct { name: []const u8, kind: TokenKind }{
        .{ .name = "fn", .kind = .KwFn },
        .{ .name = "struct", .kind = .KwStruct },
        .{ .name = "impl", .kind = .KwImpl },
        .{ .name = "type", .kind = .KwType },
        .{ .name = "let", .kind = .KwLet },
        .{ .name = "mut", .kind = .KwMut },
        .{ .name = "if", .kind = .KwIf },
        .{ .name = "else", .kind = .KwElse },
        .{ .name = "while", .kind = .KwWhile },
        .{ .name = "for", .kind = .KwFor },
        .{ .name = "in", .kind = .KwIn },
        .{ .name = "return", .kind = .KwReturn },
        .{ .name = "as", .kind = .KwAs },
        .{ .name = "self", .kind = .KwSelf },
        .{ .name = "true", .kind = .KwTrue },
        .{ .name = "false", .kind = .KwFalse },
        .{ .name = "const", .kind = .KwConst },
        .{ .name = "unsafe", .kind = .KwUnsafe },
    };

    for (keywords) |kw| {
        if (std.mem.eql(u8, ident, kw.name)) return kw.kind;
    }
    return null;
}
