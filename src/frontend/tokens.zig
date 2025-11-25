const std = @import("std");

/// Represents the different types of tokens that can be identified in a Rust program
pub const TokenType = enum {
    // Literals
    Integer,
    Float,
    Boolean,
    String,
    Char,

    // Keywords
    FN,
    Return,
    Let,
    Mut,
    Static,
    Unsafe,
    While,
    For,
    In,
    If,
    Else,
    Type,

    // Operators
    Arrow,
    Assign,
    Plus,
    Minus,
    Multiply,
    Divide,
    Power,
    Modulo,
    LogicalOr,
    LogicalAnd,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual,

    // Punctuation
    LeftBrace,
    RightBrace,
    DoubleColon,
    Comma,
    DotComma,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,

    // Special
    EndOfFile,
    Identifier,
    DocString,
    Invalid,

    Ampersand,
    Loop,
    Exclamation,

    PathSep,
    Super,
    Self,
    Crate,
    Dollar,
};

/// A token with its type and optional lexeme. Used by the parser to interpret the expression.
pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8 = "",

    /// Formats the token for display.
    pub fn format(self: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const name = switch (self.token_type) {
            TokenType.Integer => "Integer",
            TokenType.Float => "Float",
            TokenType.Boolean => "Boolean",
            TokenType.String => "String",
            TokenType.Char => "Char",
            TokenType.FN => "fn",
            TokenType.Return => "return",
            TokenType.Let => "let",
            TokenType.Mut => "mut",
            TokenType.Static => "static",
            TokenType.Unsafe => "unsafe",
            TokenType.While => "while",
            TokenType.Loop => "loop",
            TokenType.For => "for",
            TokenType.In => "in",
            TokenType.If => "if",
            TokenType.Else => "else",
            TokenType.Type => "Type",
            TokenType.Arrow => "->",
            TokenType.Assign => "=",
            TokenType.Plus => "+",
            TokenType.Minus => "-",
            TokenType.Multiply => "*",
            TokenType.Divide => "/",
            TokenType.Power => "^",
            TokenType.Modulo => "%",
            TokenType.Exclamation => "!",
            TokenType.LogicalOr => "||",
            TokenType.LogicalAnd => "&&",
            TokenType.LessThan => "<",
            TokenType.LessThanOrEqual => "<=",
            TokenType.GreaterThan => ">",
            TokenType.GreaterThanOrEqual => ">=",
            TokenType.Equal => "==",
            TokenType.NotEqual => "!=",
            TokenType.LeftBrace => "{",
            TokenType.RightBrace => "}",
            TokenType.DoubleColon => ":",
            TokenType.Comma => ",",
            TokenType.DotComma => ";",
            TokenType.LeftParen => "(",
            TokenType.RightParen => ")",
            TokenType.EndOfFile => "EOF",
            TokenType.Identifier => "Identifier",
            TokenType.DocString => "DocString",
            TokenType.Invalid => "Invalid",

            TokenType.Ampersand => "&",

            TokenType.PathSep => "::",
            TokenType.Super => "super",
            TokenType.Self => "self",
            TokenType.Crate => "crate",
            TokenType.Dollar => "$",
            TokenType.LeftBracket => "[",
            TokenType.RightBracket => "]",
        };

        if (self.lexeme.len > 0) {
            try writer.print("{s}: \"{s}\"\n", .{ name, self.lexeme });
        } else {
            try writer.print("{s}\n", .{name});
        }
    }
};
