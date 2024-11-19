const std = @import("std");

/// Error set for the Lexer
pub const LexerError = error{ UnexpectedCharacter, UnterminatedString, UnterminatedChar, InvalidEscapeSequence, UnexpectedEndOfFile, UnterminatedComment };

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

/// The `Lexer` structure processes Rust code into a series of tokens.
pub const Lexer = struct {
    input: []const u8,
    current: usize = 0,
    allocator: std.mem.Allocator,

    /// Initializes a new Lexer.
    pub fn init(input: []const u8, allocator: std.mem.Allocator) Lexer {
        return Lexer{
            .input = input,
            .allocator = allocator,
        };
    }

    /// Retrieves a list of tokens from the input.
    ///
    /// # Returns
    /// A list of `Token` representing the parts of the expression.
    pub fn tokenize(self: *Lexer) ![]Token {
        var tokens = std.ArrayList(Token).init(self.allocator);
        while (true) {
            const token = try self.next_token();
            try tokens.append(token);
            if (token.token_type == .EndOfFile) break;
        }
        return try tokens.toOwnedSlice();
    }

    /// Retrieves the next token from the input.
    ///
    /// # Returns
    /// A `Token` representing the next part of the expression.
    pub fn next_token(self: *Lexer) !Token {
        self.skip_whitespace();

        if (self.current >= self.input.len) {
            return Token{ .token_type = .EndOfFile };
        }

        const c = self.input[self.current];
        self.current += 1;

        return switch (c) {
            '{' => Token{ .token_type = .LeftBrace },
            '}' => Token{ .token_type = .RightBrace },
            ':' => Token{ .token_type = .DoubleColon },
            ',' => Token{ .token_type = .Comma },
            ';' => Token{ .token_type = .DotComma },
            '(' => Token{ .token_type = .LeftParen },
            ')' => Token{ .token_type = .RightParen },
            '+' => Token{ .token_type = .Plus },
            '-' => try self.parse_minus(),
            '*' => Token{ .token_type = .Multiply },
            '/' => try self.parse_divide(),
            '^' => Token{ .token_type = .Power },
            '%' => Token{ .token_type = .Modulo },
            '!' => try self.parse_exclamation(),
            '"' => try self.parse_string(),
            '\'' => try self.parse_char(),
            '=' => try self.parse_equal(),
            '<' => try self.parse_less_than(),
            '>' => try self.parse_greater_than(),
            else => try self.parse_identifier_or_number(c),
        };
    }

    /// Skips any whitespace characters.
    fn skip_whitespace(self: *Lexer) void {
        while (self.current < self.input.len and is_whitespace(self.input[self.current])) {
            self.current += 1;
        }
    }

    /// Parses tokens that start with '-'.
    fn parse_minus(self: *Lexer) !Token {
        if (self.current < self.input.len and self.input[self.current] == '>') {
            self.current += 1;
            return Token{ .token_type = .Arrow };
        }
        return Token{ .token_type = .Minus };
    }

    /// Parses tokens that start with '/'.
    fn parse_divide(self: *Lexer) !Token {
        if (self.current >= self.input.len) {
            return Token{ .token_type = .Divide };
        }
        const next = self.input[self.current];
        if (next == '/') {
            self.current += 1;
            if (self.current < self.input.len and self.input[self.current] == '/') {
                self.current += 1;
                return try self.parse_doc_string();
            }
            return self.skip_single_line_comment();
        }
        if (next == '*') {
            self.current += 1;
            return try self.skip_multi_line_comment();
        }
        return Token{ .token_type = .Divide };
    }

    /// Parses tokens that start with '!'.
    fn parse_exclamation(self: *Lexer) !Token {
        if (self.current < self.input.len and self.input[self.current] == '=') {
            self.current += 1;
            return Token{ .token_type = .NotEqual };
        }
        return Token{ .token_type = .Exclamation };
    }

    /// Parses tokens that start with '='.
    fn parse_equal(self: *Lexer) !Token {
        if (self.current < self.input.len and self.input[self.current] == '=') {
            self.current += 1;
            return Token{ .token_type = .Equal };
        }
        return Token{ .token_type = .Assign };
    }

    /// Parses tokens that start with '<'.
    fn parse_less_than(self: *Lexer) !Token {
        if (self.current < self.input.len and self.input[self.current] == '=') {
            self.current += 1;
            return Token{ .token_type = .LessThanOrEqual };
        }
        return Token{ .token_type = .LessThan };
    }

    /// Parses tokens that start with '>'.
    fn parse_greater_than(self: *Lexer) !Token {
        if (self.current < self.input.len and self.input[self.current] == '=') {
            self.current += 1;
            return Token{ .token_type = .GreaterThanOrEqual };
        }
        return Token{ .token_type = .GreaterThan };
    }

    /// Parses string literals.
    fn parse_string(self: *Lexer) !Token {
        const start = self.current;
        while (self.current < self.input.len and self.input[self.current] != '"') {
            if (self.input[self.current] == '\\') {
                self.current += 1; // Skip escape character
                if (self.current >= self.input.len) {
                    return LexerError.UnterminatedString;
                }
            }
            self.current += 1;
        }
        if (self.current >= self.input.len) {
            return LexerError.UnterminatedString;
        }
        const string_value = self.input[start..self.current];
        self.current += 1; // Skip closing quote
        return Token{ .token_type = .String, .lexeme = string_value };
    }

    /// Parses character literals.
    fn parse_char(self: *Lexer) !Token {
        if (self.current >= self.input.len) {
            return LexerError.UnterminatedChar;
        }
        const char_start = self.current;
        if (self.input[self.current] == '\\') {
            self.current += 1; // Skip escape character
            if (self.current >= self.input.len) {
                return LexerError.InvalidEscapeSequence;
            }
        }
        self.current += 1;
        if (self.current >= self.input.len or self.input[self.current] != '\'') {
            return LexerError.UnterminatedChar;
        }
        const char_value = self.input[char_start..self.current];
        self.current += 1; // Skip closing quote
        return Token{ .token_type = .Char, .lexeme = char_value };
    }

    /// Parses doc strings (/// comments).
    fn parse_doc_string(self: *Lexer) !Token {
        self.skip_whitespace();
        const start = self.current;
        while (self.current < self.input.len and self.input[self.current] != '\n') {
            self.current += 1;
        }
        const doc_string = self.input[start..self.current];
        return Token{ .token_type = .DocString, .lexeme = doc_string };
    }

    /// Skips single-line comments.
    fn skip_single_line_comment(self: *Lexer) LexerError!Token {
        while (self.current < self.input.len and self.input[self.current] != '\n') {
            self.current += 1;
        }
        return self.next_token();
    }

    /// Skips multi-line comments.
    fn skip_multi_line_comment(self: *Lexer) LexerError!Token {
        while (self.current < self.input.len - 1) {
            if (self.input[self.current] == '*' and self.input[self.current + 1] == '/') {
                self.current += 2;
                return self.next_token();
            }
            self.current += 1;
        }
        return LexerError.UnterminatedComment;
    }

    /// Parses identifiers or numbers.
    fn parse_identifier_or_number(self: *Lexer, first_char: u8) !Token {
        if (is_digit(first_char)) {
            self.current -= 1;
            return try self.parse_number();
        }
        if (is_alpha_num(first_char)) {
            self.current -= 1;
            return self.parse_identifier();
        }
        return LexerError.UnexpectedCharacter;
    }

    /// Parses identifiers and keywords.
    fn parse_identifier(self: *Lexer) !Token {
        const start = self.current;
        while (self.current < self.input.len and is_alpha_num(self.input[self.current])) {
            self.current += 1;
        }
        const word = self.input[start..self.current];

        // Replace the switch with if-else statements
        if (std.mem.eql(u8, word, "fn")) {
            return Token{ .token_type = .FN };
        } else if (std.mem.eql(u8, word, "return")) {
            return Token{ .token_type = .Return };
        } else if (std.mem.eql(u8, word, "let")) {
            return Token{ .token_type = .Let };
        } else if (std.mem.eql(u8, word, "mut")) {
            return Token{ .token_type = .Mut };
        } else if (std.mem.eql(u8, word, "static")) {
            return Token{ .token_type = .Static };
        } else if (std.mem.eql(u8, word, "unsafe")) {
            return Token{ .token_type = .Unsafe };
        } else if (std.mem.eql(u8, word, "while")) {
            return Token{ .token_type = .While };
        } else if (std.mem.eql(u8, word, "for")) {
            return Token{ .token_type = .For };
        } else if (std.mem.eql(u8, word, "in")) {
            return Token{ .token_type = .In };
        } else if (std.mem.eql(u8, word, "if")) {
            return Token{ .token_type = .If };
        } else if (std.mem.eql(u8, word, "else")) {
            return Token{ .token_type = .Else };
        } else if (std.mem.eql(u8, word, "i32") or
            std.mem.eql(u8, word, "i64") or
            std.mem.eql(u8, word, "f32") or
            std.mem.eql(u8, word, "f64") or
            std.mem.eql(u8, word, "bool") or
            std.mem.eql(u8, word, "str"))
        {
            return Token{ .token_type = .Type, .lexeme = word };
        } else if (std.mem.eql(u8, word, "true") or
            std.mem.eql(u8, word, "false"))
        {
            return Token{ .token_type = .Boolean, .lexeme = word };
        } else {
            return Token{ .token_type = .Identifier, .lexeme = word };
        }
    }

    /// Parses numeric literals (integers and floats).
    fn parse_number(self: *Lexer) !Token {
        const start = self.current;
        while (self.current < self.input.len and is_digit(self.input[self.current])) {
            self.current += 1;
        }

        var token_type = TokenType.Integer;
        if (self.current < self.input.len and self.input[self.current] == '.') {
            self.current += 1;
            if (self.current < self.input.len and is_digit(self.input[self.current])) {
                while (self.current < self.input.len and is_digit(self.input[self.current])) {
                    self.current += 1;
                }
                token_type = TokenType.Float;
            } else {
                return LexerError.UnexpectedCharacter;
            }
        }

        const number = self.input[start..self.current];
        return Token{ .token_type = token_type, .lexeme = number };
    }
};

/// Checks if a character is a letter.
fn is_letter(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z');
}

/// Checks if a character is a digit.
fn is_digit(c: u8) bool {
    return c >= '0' and c <= '9';
}

/// Checks if a character is alphanumeric or an underscore.
fn is_alpha_num(c: u8) bool {
    return is_letter(c) or is_digit(c) or c == '_';
}

/// Checks if a character is whitespace.
fn is_whitespace(c: u8) bool {
    return c == ' ' or c == '\n' or c == '\t' or c == '\r';
}

// ===================== TESTS =====================

test "simple integer literal" {
    const input = "42;";
    const expected_tokens = [_]Token{
        Token{ .token_type = .Integer, .lexeme = "42" },
        Token{ .token_type = .DotComma },
        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "variable declaration with integer assignment" {
    const input = "let x = 10;";
    const expected_tokens = [_]Token{
        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "10" },
        Token{ .token_type = .DotComma },
        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "variable declaration with type and float assignment" {
    const input = "let y: f64 = 3.14;";
    const expected_tokens = [_]Token{
        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "f64" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Float, .lexeme = "3.14" },
        Token{ .token_type = .DotComma },
        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "empty function declaration" {
    const input = "fn main() {}";
    const expected_tokens = [_]Token{
        Token{ .token_type = .FN },
        Token{ .token_type = .Identifier, .lexeme = "main" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .LeftBrace },
        Token{ .token_type = .RightBrace },
        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "function with parameters and return type" {
    const input = "fn add(a: i32, b: i32) -> i32 { return a + b; }";
    const expected_tokens = [_]Token{
        Token{ .token_type = .FN },
        Token{ .token_type = .Identifier, .lexeme = "add" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .Identifier, .lexeme = "a" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .Comma },
        Token{ .token_type = .Identifier, .lexeme = "b" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .Arrow },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .LeftBrace },
        Token{ .token_type = .Return },
        Token{ .token_type = .Identifier, .lexeme = "a" },
        Token{ .token_type = .Plus },
        Token{ .token_type = .Identifier, .lexeme = "b" },
        Token{ .token_type = .DotComma },
        Token{ .token_type = .RightBrace },
        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "string literal assignment" {
    const input = "let s = \"Hello, world!\";";
    const expected_tokens = [_]Token{
        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "s" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .String, .lexeme = "Hello, world!" },
        Token{ .token_type = .DotComma },
        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "boolean literal assignment" {
    const input = "let flag = true;";
    const expected_tokens = [_]Token{
        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "flag" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Boolean, .lexeme = "true" },
        Token{ .token_type = .DotComma },
        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "function call in expression" {
    const input = "let result = factorial(5);";
    const expected_tokens = [_]Token{
        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "result" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Identifier, .lexeme = "factorial" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .Integer, .lexeme = "5" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },
        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "complex expression" {
    const input = "x = (a + b) * c / d - e;";
    const expected_tokens = [_]Token{
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .Identifier, .lexeme = "a" },
        Token{ .token_type = .Plus },
        Token{ .token_type = .Identifier, .lexeme = "b" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .Multiply },
        Token{ .token_type = .Identifier, .lexeme = "c" },
        Token{ .token_type = .Divide },
        Token{ .token_type = .Identifier, .lexeme = "d" },
        Token{ .token_type = .Minus },
        Token{ .token_type = .Identifier, .lexeme = "e" },
        Token{ .token_type = .DotComma },
        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "function with local variable and return" {
    const input = "fn compute(x: i32, y: i32) -> i32 { let result = x * y; return result; }";
    const expected_tokens = [_]Token{
        Token{ .token_type = .FN },
        Token{ .token_type = .Identifier, .lexeme = "compute" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .Comma },
        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .Arrow },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .LeftBrace },
        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "result" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .Multiply },
        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .DotComma },
        Token{ .token_type = .Return },
        Token{ .token_type = .Identifier, .lexeme = "result" },
        Token{ .token_type = .DotComma },
        Token{ .token_type = .RightBrace },
        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "Test for code snippet 1" {
    const input =
        \\fn foo(x: i32) {
        \\    println!("{}", x + 1);
        \\}
        \\fn main() {
        \\    let x = 20;
        \\    println!("{}", x);
        \\    foo(x);
        \\    println!("{}", x);
        \\}
    ;

    const expected_tokens = [_]Token{
        Token{ .token_type = .FN },
        Token{ .token_type = .Identifier, .lexeme = "foo" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .LeftBrace },

        Token{ .token_type = .Identifier, .lexeme = "println" },
        Token{ .token_type = .Exclamation },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .String, .lexeme = "{}" },
        Token{ .token_type = .Comma },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .Plus },
        Token{ .token_type = .Integer, .lexeme = "1" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .RightBrace },

        Token{ .token_type = .FN },
        Token{ .token_type = .Identifier, .lexeme = "main" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .LeftBrace },

        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "20" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Identifier, .lexeme = "println" },
        Token{ .token_type = .Exclamation },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .String, .lexeme = "{}" },
        Token{ .token_type = .Comma },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Identifier, .lexeme = "foo" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Identifier, .lexeme = "println" },
        Token{ .token_type = .Exclamation },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .String, .lexeme = "{}" },
        Token{ .token_type = .Comma },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .RightBrace },

        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "Test for code snippet 2" {
    const input =
        \\static mut A: i32 = 0;
        \\static mut B: i32 = 0;
        \\fn main() {
        \\    let y: i32;
        \\    unsafe {
        \\        A = 3;
        \\        B = 4;
        \\        y = A + B;
        \\    }
        \\    println!("{}", y);
        \\}
    ;

    const expected_tokens = [_]Token{
        // Global variables
        Token{ .token_type = .Static },
        Token{ .token_type = .Mut },
        Token{ .token_type = .Identifier, .lexeme = "A" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "0" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Static },
        Token{ .token_type = .Mut },
        Token{ .token_type = .Identifier, .lexeme = "B" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "0" },
        Token{ .token_type = .DotComma },

        // Main function
        Token{ .token_type = .FN },
        Token{ .token_type = .Identifier, .lexeme = "main" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .LeftBrace },

        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Unsafe },
        Token{ .token_type = .LeftBrace },

        Token{ .token_type = .Identifier, .lexeme = "A" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "3" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Identifier, .lexeme = "B" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "4" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Identifier, .lexeme = "A" },
        Token{ .token_type = .Plus },
        Token{ .token_type = .Identifier, .lexeme = "B" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .RightBrace },

        Token{ .token_type = .Identifier, .lexeme = "println" },
        Token{ .token_type = .Exclamation },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .String, .lexeme = "{}" },
        Token{ .token_type = .Comma },
        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .RightBrace },

        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "Test for code snippet 3" {
    const input =
        \\static mut A: i32 = 0;
        \\static mut B: i32 = 0;
        \\fn suma(mut x: i32) -> i32 {
        \\    let mut accum = 0;
        \\    println!("{}", 15);
        \\    while x > 0 {
        \\        accum = accum + x;
        \\        x = x - 1;
        \\    }
        \\    println!("{}", 16);
        \\    accum
        \\}
        \\fn main() {
        \\    let y: i32;
        \\    unsafe {
        \\        A = 10;
        \\        B = 11;
        \\    }
        \\    println!("{}", 14);
        \\    y = suma(4);
        \\    println!("{}", y);
        \\}
    ;

    const expected_tokens = [_]Token{
        // Global variables
        Token{ .token_type = .Static },
        Token{ .token_type = .Mut },
        Token{ .token_type = .Identifier, .lexeme = "A" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "0" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Static },
        Token{ .token_type = .Mut },
        Token{ .token_type = .Identifier, .lexeme = "B" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "0" },
        Token{ .token_type = .DotComma },

        // Function declaration
        Token{ .token_type = .FN },
        Token{ .token_type = .Identifier, .lexeme = "suma" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .Mut },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .Arrow },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .LeftBrace },

        // Function body
        Token{ .token_type = .Let },
        Token{ .token_type = .Mut },
        Token{ .token_type = .Identifier, .lexeme = "accum" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "0" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Identifier, .lexeme = "println" },
        Token{ .token_type = .Exclamation },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .String, .lexeme = "{}" },
        Token{ .token_type = .Comma },
        Token{ .token_type = .Integer, .lexeme = "15" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        // While loop
        Token{ .token_type = .While },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .GreaterThan },
        Token{ .token_type = .Integer, .lexeme = "0" },
        Token{ .token_type = .LeftBrace },

        // Loop body
        Token{ .token_type = .Identifier, .lexeme = "accum" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Identifier, .lexeme = "accum" },
        Token{ .token_type = .Plus },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .Minus },
        Token{ .token_type = .Integer, .lexeme = "1" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .RightBrace },

        Token{ .token_type = .Identifier, .lexeme = "println" },
        Token{ .token_type = .Exclamation },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .String, .lexeme = "{}" },
        Token{ .token_type = .Comma },
        Token{ .token_type = .Integer, .lexeme = "16" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        // Return lexeme
        Token{ .token_type = .Identifier, .lexeme = "accum" },
        Token{ .token_type = .RightBrace },

        // Main function
        Token{ .token_type = .FN },
        Token{ .token_type = .Identifier, .lexeme = "main" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .LeftBrace },

        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Unsafe },
        Token{ .token_type = .LeftBrace },

        Token{ .token_type = .Identifier, .lexeme = "A" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "10" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Identifier, .lexeme = "B" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "11" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .RightBrace },

        Token{ .token_type = .Identifier, .lexeme = "println" },
        Token{ .token_type = .Exclamation },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .String, .lexeme = "{}" },
        Token{ .token_type = .Comma },
        Token{ .token_type = .Integer, .lexeme = "14" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Identifier, .lexeme = "suma" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .Integer, .lexeme = "4" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Identifier, .lexeme = "println" },
        Token{ .token_type = .Exclamation },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .String, .lexeme = "{}" },
        Token{ .token_type = .Comma },
        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .RightBrace },

        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "Test for code snippet 4" {
    const input =
        \\static mut A: i32 = 0;
        \\static mut B: i32 = 0;
        \\fn sumarec(x: i32) -> i32 {
        \\    println!("{}", x);
        \\    if x < 1 {
        \\        return 0;
        \\    }
        \\    return x + sumarec(x - 1);
        \\}
        \\fn main() {
        \\    let y = sumarec(4);
        \\    println!("{}", y);
        \\}
    ;

    const expected_tokens = [_]Token{
        // Global variables
        Token{ .token_type = .Static },
        Token{ .token_type = .Mut },
        Token{ .token_type = .Identifier, .lexeme = "A" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "0" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Static },
        Token{ .token_type = .Mut },
        Token{ .token_type = .Identifier, .lexeme = "B" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "0" },
        Token{ .token_type = .DotComma },

        // Function declaration
        Token{ .token_type = .FN },
        Token{ .token_type = .Identifier, .lexeme = "sumarec" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .DoubleColon },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .Arrow },
        Token{ .token_type = .Type, .lexeme = "i32" },
        Token{ .token_type = .LeftBrace },

        // Function body
        Token{ .token_type = .Identifier, .lexeme = "println" },
        Token{ .token_type = .Exclamation },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .String, .lexeme = "{}" },
        Token{ .token_type = .Comma },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .If },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .LessThan },
        Token{ .token_type = .Integer, .lexeme = "1" },
        Token{ .token_type = .LeftBrace },

        Token{ .token_type = .Return },
        Token{ .token_type = .Integer, .lexeme = "0" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .RightBrace },

        Token{ .token_type = .Return },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .Plus },
        Token{ .token_type = .Identifier, .lexeme = "sumarec" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .Minus },
        Token{ .token_type = .Integer, .lexeme = "1" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .RightBrace },

        // Main function
        Token{ .token_type = .FN },
        Token{ .token_type = .Identifier, .lexeme = "main" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .LeftBrace },

        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Identifier, .lexeme = "sumarec" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .Integer, .lexeme = "4" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Identifier, .lexeme = "println" },
        Token{ .token_type = .Exclamation },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .String, .lexeme = "{}" },
        Token{ .token_type = .Comma },
        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .RightBrace },

        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "Test for code snippet with single-line comments" {
    const input =
        \\fn main() {
        \\    let x = 10; // initialize x to 10
        \\    let y = 20; // initialize y to 20
        \\    let z = x + y; // sum x and y
        \\    println!("{}", z); // print the result
        \\}
    ;

    const expected_tokens = [_]Token{
        Token{ .token_type = .FN },
        Token{ .token_type = .Identifier, .lexeme = "main" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .LeftBrace },

        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "10" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "20" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "z" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .Plus },
        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Identifier, .lexeme = "println" },
        Token{ .token_type = .Exclamation },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .String, .lexeme = "{}" },
        Token{ .token_type = .Comma },
        Token{ .token_type = .Identifier, .lexeme = "z" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .RightBrace },

        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "Test for code snippet with multi-line comments" {
    const input =
        \\fn main() {
        \\    /* This is a multi-line comment
        \\       that spans multiple lines */
        \\    let x = 10;
        \\    /* Another comment */
        \\    let y = 20;
        \\    let z = x + y;
        \\    println!("{}", z);
        \\}
    ;

    const expected_tokens = [_]Token{
        Token{ .token_type = .FN },
        Token{ .token_type = .Identifier, .lexeme = "main" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .LeftBrace },

        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "10" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "20" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "z" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .Plus },
        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Identifier, .lexeme = "println" },
        Token{ .token_type = .Exclamation },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .String, .lexeme = "{}" },
        Token{ .token_type = .Comma },
        Token{ .token_type = .Identifier, .lexeme = "z" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .RightBrace },

        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "Test for code snippet with docstring comments" {
    const input =
        \\/// This is the main function
        \\fn main() {
        \\    let x = 10;
        \\    let y = 20;
        \\    let z = x + y;
        \\    println!("{}", z);
        \\}
    ;

    const expected_tokens = [_]Token{
        Token{ .token_type = .DocString, .lexeme = "This is the main function" },

        Token{ .token_type = .FN },
        Token{ .token_type = .Identifier, .lexeme = "main" },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .LeftBrace },

        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "10" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Integer, .lexeme = "20" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Let },
        Token{ .token_type = .Identifier, .lexeme = "z" },
        Token{ .token_type = .Assign },
        Token{ .token_type = .Identifier, .lexeme = "x" },
        Token{ .token_type = .Plus },
        Token{ .token_type = .Identifier, .lexeme = "y" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .Identifier, .lexeme = "println" },
        Token{ .token_type = .Exclamation },
        Token{ .token_type = .LeftParen },
        Token{ .token_type = .String, .lexeme = "{}" },
        Token{ .token_type = .Comma },
        Token{ .token_type = .Identifier, .lexeme = "z" },
        Token{ .token_type = .RightParen },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .RightBrace },

        Token{ .token_type = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

fn test_lexer(input: []const u8, expected_tokens: []const Token) !void {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init(input, allocator);
    const tokens = try lexer.tokenize();
    defer allocator.free(tokens);

    try std.testing.expectEqual(expected_tokens.len, tokens.len);

    for (tokens, 0..) |token, i| {
        const expected = expected_tokens[i];
        // Compare token types
        try std.testing.expectEqual(expected.token_type, token.token_type);
        // Compare lexemes
        try std.testing.expect(std.mem.eql(u8, token.lexeme, expected.lexeme));
    }
}

pub fn compareToken(actual: Token, expected: Token) !void {

    // Compare token types
    try std.testing.expectEqual(expected.token_type, actual.token_type);
    // Compare lexemes
    try std.testing.expect(std.mem.eql(u8, actual.lexeme, expected.lexeme));
}
