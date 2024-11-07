const std = @import("std");

pub const log_level: std.log.Level = .info;

/// Represents the different types of tokens that can be identified in a rust program
const TokenType = enum {
    /// Represents an integer number.
    Integer,

    /// Represents a floating-point number.
    Float,

    /// Represents a boolean.
    Boolean,

    /// Represents a string.
    String,

    // functions
    /// 'fn'
    FN,
    /// 'return'
    FunReturn,
    /// '->' used to declare a function return type.
    Arrow,

    While,
    For,
    In,

    If,
    Else,

    /// {} used to declare a block of code.
    LeftBrace,
    RightBrace,

    Unsafe,

    // assignments
    /// 'let' used to declare a variable.
    Let,

    Mut,

    Static,

    Ampersand,

    Exclamation,

    /// ':' used to declare a variable type.
    DoubleColon,
    /// '=' used to assign a value to a variable.
    Assign,

    /// Comma (,)
    Comma,
    /// dot-comma (;)
    DotComma,
    /// Represents the '(' character.
    LeftParen,
    /// Represents the ')' character.
    RightParen,

    // Match operators
    /// Represents the '+' operator.
    Plus,
    /// Represents the '-' operator.
    Minus,
    /// Represents the '*' operator.
    Multiply,
    /// Represents the '/' operator.
    Divide,
    /// Represents the '^' operator.
    Power,
    /// Represents the '%' operator.
    Modulo,
    /// Represents the '!' operator.
    Factorial,

    // Comparison operators
    /// Represents the '<' operator.
    LessThan,
    /// Represents the '<=' operator.
    LessThanOrEqual,
    /// Represents the '>' operator.
    GreaterThan,
    /// Represents the '>=' operator.
    GreaterThanOrEqual,
    /// Represents the '==' operator.
    Equal,
    /// Represents the '!=' operator.
    Different,

    /// Represents the end of the input.
    EndOfFile,
    /// Represents an invalid or unrecognized token.
    Invalid,

    // keywords

    /// Represents a variable type
    Type,

    /// ID
    Identifier,
};

/// A token with its type and optional value. Used by the parser to interpret the expression.
pub const Token = struct {
    kind: TokenType,
    value: []const u8 = "",

    fn print_value(self: Token, writer: anytype, name: []const u8) !void {
        try writer.print("{s}: {s}\n", .{ name, self.value });
    }

    /// Formats the token for display.
    pub fn format(self: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.kind) {
            // Numeric tokens
            TokenType.Integer => try print_value(self, writer, "Integer"),

            TokenType.Float => try print_value(self, writer, "Float"),
            // Literal tokens
            TokenType.Boolean => try print_value(self, writer, "Boolean"),

            TokenType.String => try print_value(self, writer, "String"),
            // Function-related tokens
            TokenType.FN => try writer.print("fun", .{}),
            TokenType.FunReturn => try writer.print("return", .{}),
            TokenType.Arrow => try writer.print("->", .{}),

            TokenType.Unsafe => try writer.print("unsafe", .{}),
            TokenType.While => try writer.print("while", .{}),
            TokenType.For => try writer.print("for", .{}),
            TokenType.In => try writer.print("in", .{}),
            TokenType.If => try writer.print("if", .{}),
            TokenType.Else => try writer.print("else", .{}),

            // TODO: print the correct value ({})
            TokenType.LeftBrace => try writer.print("LB", .{}),
            TokenType.RightBrace => try writer.print("RB", .{}),
            // Assignment tokens
            TokenType.Let => try writer.print("let", .{}),
            TokenType.Mut => try writer.print("mut", .{}),
            TokenType.Static => try writer.print("static", .{}),
            TokenType.DoubleColon => try writer.print(":", .{}),
            TokenType.Assign => try writer.print("=", .{}),
            TokenType.Ampersand => try writer.print("&", .{}),
            TokenType.Exclamation => try writer.print("!", .{}),
            // Punctuation tokens
            TokenType.Comma => try writer.print(",", .{}),
            TokenType.DotComma => try writer.print(";", .{}),
            TokenType.LeftParen => try writer.print("(", .{}),
            TokenType.RightParen => try writer.print(")", .{}),
            // Mathematical operators
            TokenType.Plus => try writer.print("+", .{}),
            TokenType.Minus => try writer.print("-", .{}),
            TokenType.Multiply => try writer.print("*", .{}),
            TokenType.Divide => try writer.print("/", .{}),
            TokenType.Power => try writer.print("^", .{}),
            TokenType.Modulo => try writer.print("%", .{}),
            TokenType.Factorial => try writer.print("!", .{}),
            // Comparison operators
            TokenType.LessThan => try writer.print("<", .{}),
            TokenType.LessThanOrEqual => try writer.print("<=", .{}),
            TokenType.GreaterThan => try writer.print(">", .{}),
            TokenType.GreaterThanOrEqual => try writer.print(">=", .{}),
            TokenType.Equal => try writer.print("==", .{}),
            TokenType.Different => try writer.print("!=", .{}),
            // Identifier and type tokens
            TokenType.Identifier => try print_value(self, writer, "ID"),

            TokenType.Type => try print_value(self, writer, "Type"),
            // End of file and invalid tokens
            TokenType.EndOfFile => try writer.print("EOF", .{}),
            TokenType.Invalid => try writer.print("Invalid token", .{}),
        }
    }
};

/// The `Lexer` structure processes a mathematical expression into a series of tokens.
pub const Lexer = struct {
    input: []const u8,
    current: usize = 0,

    fn parse_keyword(self: *Lexer) Token {
        // read word with whitespace

        const start = self.current;

        while (self.current < self.input.len and !is_whitespace(self.input[self.current]) and is_alpha(self.input[self.current])) {
            self.current += 1;
        }

        const word = self.input[start..self.current];

        if (std.mem.eql(u8, word, "fn")) {
            return Token{ .kind = .FN };
        }
        if (std.mem.eql(u8, word, "return")) {
            return Token{ .kind = .FunReturn };
        }
        if (std.mem.eql(u8, word, "let")) {
            return Token{ .kind = .Let };
        }
        if (std.mem.eql(u8, word, "mut")) {
            return Token{ .kind = .Mut };
        }
        if (std.mem.eql(u8, word, "static")) {
            return Token{ .kind = .Static };
        }

        if (std.mem.eql(u8, word, "unsafe")) {
            return Token{ .kind = .Unsafe };
        }

        if (std.mem.eql(u8, word, "while")) {
            return Token{ .kind = .While };
        }
        if (std.mem.eql(u8, word, "for")) {
            return Token{ .kind = .For };
        }
        if (std.mem.eql(u8, word, "in")) {
            return Token{ .kind = .In };
        }
        if (std.mem.eql(u8, word, "if")) {
            return Token{ .kind = .If };
        }
        if (std.mem.eql(u8, word, "else")) {
            return Token{ .kind = .Else };
        }

        // types (i32, i64, f32, f64, bool, str)
        if (std.mem.eql(u8, word, "i32")) {
            return Token{ .kind = .Type, .value = word };
        }
        if (std.mem.eql(u8, word, "f32")) {
            return Token{ .kind = .Type, .value = word };
        }
        if (std.mem.eql(u8, word, "i64")) {
            return Token{ .kind = .Type, .value = word };
        }
        if (std.mem.eql(u8, word, "f64")) {
            return Token{ .kind = .Type, .value = word };
        }
        if (std.mem.eql(u8, word, "bool")) {
            return Token{ .kind = .Type, .value = word };
        }
        if (std.mem.eql(u8, word, "str")) {
            return Token{ .kind = .Type, .value = word };
        }

        // boolean values
        if (std.mem.eql(u8, word, "true")) {
            return Token{ .kind = .Boolean, .value = word };
        }
        if (std.mem.eql(u8, word, "false")) {
            return Token{ .kind = .Boolean, .value = word };
        }

        return Token{ .kind = .Identifier, .value = word };
    }

    /// parses a possible 1-3 char symbol
    fn parse_small_char(self: *Lexer, prev: u8) Token {
        // test for any of:
        // =, ==, -, ->, !, !=, <, <=, >, >=

        // exceeded input
        if (self.current >= self.input.len) {
            return Token{ .kind = .EndOfFile };
        }

        const next = self.input[self.current];

        if (prev == '=') {
            if (next == '=') {
                self.current += 1;
                return Token{ .kind = .Equal };
            }
            return Token{ .kind = .Assign };
        }
        if (prev == '-') {
            if (next == '>') {
                self.current += 1;
                return Token{ .kind = .Arrow };
            }
            return Token{ .kind = .Minus };
        }
        if (prev == '!') {
            if (next == '=') {
                self.current += 1;
                return Token{ .kind = .Different };
            }
            return Token{ .kind = .Factorial };
        }
        if (prev == '<') {
            if (next == '=') {
                self.current += 1;
                return Token{ .kind = .LessThanOrEqual };
            }
            return Token{ .kind = .LessThan };
        }
        if (prev == '>') {
            if (next == '=') {
                self.current += 1;
                return Token{ .kind = .GreaterThanOrEqual };
            }
            return Token{ .kind = .GreaterThan };
        }
        return Token{ .kind = .Invalid };
    }

    fn lookup_integer(self: *Lexer) void {
        while (self.current < self.input.len and is_digit(self.input[self.current])) {
            self.current += 1;
        }
    }

    fn parse_digit(self: *Lexer) Token {
        const start = self.current;

        lookup_integer(self);

        const int_value = self.input[start..self.current];

        if (self.current >= self.input.len) {
            return Token{ .kind = .Integer, .value = int_value };
        }

        if (!is_dot(self.input[self.current])) {
            return Token{ .kind = .Integer, .value = int_value };
        }
        // dot found
        self.current += 1;
        lookup_integer(self);

        const float_value = self.input[start..self.current];
        return Token{ .kind = .Float, .value = float_value };
    }

    fn parse_string(self: *Lexer) Token {
        const start = self.current;
        while (self.current < self.input.len and self.input[self.current] != '"') {
            self.current += 1;
        }
        if (self.current >= self.input.len) {
            return Token{ .kind = .Invalid };
        }
        self.current += 1;

        const string_value = self.input[start .. self.current - 1];
        return Token{ .kind = .String, .value = string_value };
    }

    /// Retrieves the next token from the input.
    ///
    /// # Returns
    /// A `Token` representing the next part of the expression.
    pub fn next_token(self: *Lexer) Token {
        // Skip any whitespace

        while (self.current < self.input.len and is_whitespace(self.input[self.current])) {
            self.current += 1;
        }

        if (self.current >= self.input.len) {
            return Token{ .kind = .EndOfFile };
        }

        const c = self.input[self.current];
        self.current += 1;

        switch (c) {
            // match single characters
            '{' => return Token{ .kind = .LeftBrace },
            '}' => return Token{ .kind = .RightBrace },
            ':' => return Token{ .kind = .DoubleColon },
            ',' => return Token{ .kind = .Comma },
            ';' => return Token{ .kind = .DotComma },
            '(' => return Token{ .kind = .LeftParen },
            ')' => return Token{ .kind = .RightParen },

            '+' => return Token{ .kind = .Plus },
            '*' => return Token{ .kind = .Multiply },
            '/' => return Token{ .kind = .Divide },
            '^' => return Token{ .kind = .Power },
            '%' => return Token{ .kind = .Modulo },
            '&' => return Token{ .kind = .Ampersand },
            '!' => return Token{ .kind = .Exclamation },
            '"' => return parse_string(self),
            // '=' => return Token{ .kind = .Assign }, // overlap with ==
            // '-' => return Token{ .kind = .Minus }, // overlap with ->
            // '!' => return Token{ .kind = .Factorial }, // overlap with !=
            // '<' => return Token{ .kind = .LessThan }, // overlap with <=
            // '>' => return Token{ .kind = .GreaterThan }, // overlap with >=
            else => {
                if (is_small_char_begin(c)) {
                    return parse_small_char(self, c);
                }
                if (is_digit(c)) {
                    self.current -= 1;
                    return parse_digit(self);
                }
                if (is_letter(c)) {
                    self.current -= 1;
                    return parse_keyword(self);
                }

                return Token{ .kind = .Invalid };
            },
        }
    }
};

/// checks if a char leads to a 2-3 char symbol
/// # Parameters
/// - `c`: The character to check.
///
/// # Returns
/// `true` if the character could be a small char, `false` otherwise.
fn is_small_char_begin(c: u8) bool {
    return c == '<' or c == '>' or c == '=' or c == '!' or c == '-';
}

/// Checks if a character is a digit.
///
/// # Parameters
/// - `c`: The character to check.
///
/// # Returns
/// `true` if the character is a digit, `false` otherwise.
fn is_digit(c: u8) bool {
    return c >= '0' and c <= '9';
}

/// Checks if a character is a letter.
///
/// # Parameters
/// - `c`: The character to check.
///
/// # Returns
/// `true` if the character is a letter, `false` otherwise.
fn is_letter(c: u9) bool {
    return c >= 'a' and c <= 'z' or c >= 'A' and c <= 'Z';
}

// is dot
/// Checks if a character is a dot.
///
/// # Parameters
/// - `c`: The character to check.
///
/// # Returns
/// `true` if the character is a dot, `false` otherwise.
fn is_dot(c: u8) bool {
    return c == '.';
}

/// Checks if a character is a letter, digit or underscore.
///
/// # Parameters
/// - `c`: The character to check.
///
/// # Returns
/// `true` if the character is a letter, digit or underscore, `false` otherwise.
fn is_alpha(c: u8) bool {
    return is_letter(c) or is_digit(c) or c == '_';
}

/// Checks if a character is whitespace.
///
/// # Parameters
/// - `c`: The character to check.
///
/// # Returns
/// `true` if the character is a whitespace character, `false` otherwise.
fn is_whitespace(c: u8) bool {
    return c == ' ' or c == '\n' or c == '\t' or c == '\r';
}

test "simple integer literal" {
    const input = "42;";
    const expected_tokens = [_]Token{
        Token{ .kind = .Integer, .value = "42" },
        Token{ .kind = .DotComma },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "variable declaration with integer assignment" {
    const input = "let x = 10;";
    const expected_tokens = [_]Token{
        Token{ .kind = .Let },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .Assign },
        Token{ .kind = .Integer, .value = "10" },
        Token{ .kind = .DotComma },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "variable declaration with type and float assignment" {
    const input = "let y: f64 = 3.14;";
    const expected_tokens = [_]Token{
        Token{ .kind = .Let },
        Token{ .kind = .Identifier, .value = "y" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "f64" },
        Token{ .kind = .Assign },
        Token{ .kind = .Float, .value = "3.14" },
        Token{ .kind = .DotComma },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "empty function declaration" {
    const input = "fn main() {}";
    const expected_tokens = [_]Token{
        Token{ .kind = .FN },
        Token{ .kind = .Identifier, .value = "main" },
        Token{ .kind = .LeftParen },
        Token{ .kind = .RightParen },
        Token{ .kind = .LeftBrace },
        Token{ .kind = .RightBrace },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "function with parameters and return type" {
    const input = "fn add(a: i32, b: i32) -> i32 { return a + b; }";
    const expected_tokens = [_]Token{
        Token{ .kind = .FN },
        Token{ .kind = .Identifier, .value = "add" },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Identifier, .value = "a" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .Comma },
        Token{ .kind = .Identifier, .value = "b" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .RightParen },
        Token{ .kind = .Arrow },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .LeftBrace },
        Token{ .kind = .FunReturn },
        Token{ .kind = .Identifier, .value = "a" },
        Token{ .kind = .Plus },
        Token{ .kind = .Identifier, .value = "b" },
        Token{ .kind = .DotComma },
        Token{ .kind = .RightBrace },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "string literal assignment" {
    const input = "let s = \"Hello, world!\";";
    const expected_tokens = [_]Token{
        Token{ .kind = .Let },
        Token{ .kind = .Identifier, .value = "s" },
        Token{ .kind = .Assign },
        Token{ .kind = .String, .value = "Hello, world!" },
        Token{ .kind = .DotComma },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "boolean literal assignment" {
    const input = "let flag = true;";
    const expected_tokens = [_]Token{
        Token{ .kind = .Let },
        Token{ .kind = .Identifier, .value = "flag" },
        Token{ .kind = .Assign },
        Token{ .kind = .Boolean, .value = "true" },
        Token{ .kind = .DotComma },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "function call in expression" {
    const input = "let result = factorial(5);";
    const expected_tokens = [_]Token{
        Token{ .kind = .Let },
        Token{ .kind = .Identifier, .value = "result" },
        Token{ .kind = .Assign },
        Token{ .kind = .Identifier, .value = "factorial" },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Integer, .value = "5" },
        Token{ .kind = .RightParen },
        Token{ .kind = .DotComma },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "complex expression" {
    const input = "x = (a + b) * c / d - e;";
    const expected_tokens = [_]Token{
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .Assign },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Identifier, .value = "a" },
        Token{ .kind = .Plus },
        Token{ .kind = .Identifier, .value = "b" },
        Token{ .kind = .RightParen },
        Token{ .kind = .Multiply },
        Token{ .kind = .Identifier, .value = "c" },
        Token{ .kind = .Divide },
        Token{ .kind = .Identifier, .value = "d" },
        Token{ .kind = .Minus },
        Token{ .kind = .Identifier, .value = "e" },
        Token{ .kind = .DotComma },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}
test "function with local variable and return" {
    const input = "fn compute(x: i32, y: i32) -> i32 { let result = x * y; return result; }";
    const expected_tokens = [_]Token{
        Token{ .kind = .FN },
        Token{ .kind = .Identifier, .value = "compute" },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .Comma },
        Token{ .kind = .Identifier, .value = "y" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .RightParen },
        Token{ .kind = .Arrow },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .LeftBrace },
        Token{ .kind = .Let },
        Token{ .kind = .Identifier, .value = "result" },
        Token{ .kind = .Assign },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .Multiply },
        Token{ .kind = .Identifier, .value = "y" },
        Token{ .kind = .DotComma },
        Token{ .kind = .FunReturn },
        Token{ .kind = .Identifier, .value = "result" },
        Token{ .kind = .DotComma },
        Token{ .kind = .RightBrace },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

// los reales
test "Test for code snippet 1" {
    const input =
        \\fn foo(x: i32) {
        \\    println!("{}", x + 1);
        \\}
        \\
        \\fn main() {
        \\    let x = 20;
        \\    println!("{}", x);
        \\    foo(x);
        \\    println!("{}", x);
        \\}
    ;

    const expected_tokens = [_]Token{
        Token{ .kind = .FN },
        Token{ .kind = .Identifier, .value = "foo" },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .RightParen },
        Token{ .kind = .LeftBrace },

        Token{ .kind = .Identifier, .value = "println" },
        Token{ .kind = .Exclamation },
        Token{ .kind = .LeftParen },
        Token{ .kind = .String, .value = "{}" },
        Token{ .kind = .Comma },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .Plus },
        Token{ .kind = .Integer, .value = "1" },
        Token{ .kind = .RightParen },
        Token{ .kind = .DotComma },

        Token{ .kind = .RightBrace },

        Token{ .kind = .FN },
        Token{ .kind = .Identifier, .value = "main" },
        Token{ .kind = .LeftParen },
        Token{ .kind = .RightParen },
        Token{ .kind = .LeftBrace },

        Token{ .kind = .Let },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .Assign },
        Token{ .kind = .Integer, .value = "20" },
        Token{ .kind = .DotComma },

        Token{ .kind = .Identifier, .value = "println" },
        Token{ .kind = .Exclamation },
        Token{ .kind = .LeftParen },
        Token{ .kind = .String, .value = "{}" },
        Token{ .kind = .Comma },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .RightParen },
        Token{ .kind = .DotComma },

        Token{ .kind = .Identifier, .value = "foo" },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .RightParen },
        Token{ .kind = .DotComma },

        Token{ .kind = .Identifier, .value = "println" },
        Token{ .kind = .Exclamation },
        Token{ .kind = .LeftParen },
        Token{ .kind = .String, .value = "{}" },
        Token{ .kind = .Comma },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .RightParen },
        Token{ .kind = .DotComma },

        Token{ .kind = .RightBrace },

        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "Test for code snippet 2" {
    const input =
        \\static mut A: i32 = 0;
        \\static mut B: i32 = 0;
        \\
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
        Token{ .kind = .Static },
        Token{ .kind = .Mut },
        Token{ .kind = .Identifier, .value = "A" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .Assign },
        Token{ .kind = .Integer, .value = "0" },
        Token{ .kind = .DotComma },

        Token{ .kind = .Static },
        Token{ .kind = .Mut },
        Token{ .kind = .Identifier, .value = "B" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .Assign },
        Token{ .kind = .Integer, .value = "0" },
        Token{ .kind = .DotComma },

        Token{ .kind = .FN },
        Token{ .kind = .Identifier, .value = "main" },
        Token{ .kind = .LeftParen },
        Token{ .kind = .RightParen },
        Token{ .kind = .LeftBrace },

        Token{ .kind = .Let },
        Token{ .kind = .Identifier, .value = "y" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .DotComma },

        Token{ .kind = .Unsafe },
        Token{ .kind = .LeftBrace },

        Token{ .kind = .Identifier, .value = "A" },
        Token{ .kind = .Assign },
        Token{ .kind = .Integer, .value = "3" },
        Token{ .kind = .DotComma },

        Token{ .kind = .Identifier, .value = "B" },
        Token{ .kind = .Assign },
        Token{ .kind = .Integer, .value = "4" },
        Token{ .kind = .DotComma },

        Token{ .kind = .Identifier, .value = "y" },
        Token{ .kind = .Assign },
        Token{ .kind = .Identifier, .value = "A" },
        Token{ .kind = .Plus },
        Token{ .kind = .Identifier, .value = "B" },
        Token{ .kind = .DotComma },

        Token{ .kind = .RightBrace },

        Token{ .kind = .Identifier, .value = "println" },
        Token{ .kind = .Exclamation },
        Token{ .kind = .LeftParen },
        Token{ .kind = .String, .value = "{}" },
        Token{ .kind = .Comma },
        Token{ .kind = .Identifier, .value = "y" },
        Token{ .kind = .RightParen },
        Token{ .kind = .DotComma },

        Token{ .kind = .RightBrace },

        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "Test for code snippet 3" {
    const input =
        \\static mut A: i32 = 0;
        \\static mut B: i32 = 0;
        \\
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
        \\
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
        Token{ .kind = .Static },
        Token{ .kind = .Mut },
        Token{ .kind = .Identifier, .value = "A" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .Assign },
        Token{ .kind = .Integer, .value = "0" },
        Token{ .kind = .DotComma },

        Token{ .kind = .Static },
        Token{ .kind = .Mut },
        Token{ .kind = .Identifier, .value = "B" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .Assign },
        Token{ .kind = .Integer, .value = "0" },
        Token{ .kind = .DotComma },

        // Function declaration
        Token{ .kind = .FN },
        Token{ .kind = .Identifier, .value = "suma" },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Mut },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .RightParen },
        Token{ .kind = .Arrow },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .LeftBrace },

        // Function body
        Token{ .kind = .Let },
        Token{ .kind = .Mut },
        Token{ .kind = .Identifier, .value = "accum" },
        Token{ .kind = .Assign },
        Token{ .kind = .Integer, .value = "0" },
        Token{ .kind = .DotComma },

        Token{ .kind = .Identifier, .value = "println" },
        Token{ .kind = .Exclamation },
        Token{ .kind = .LeftParen },
        Token{ .kind = .String, .value = "{}" },
        Token{ .kind = .Comma },
        Token{ .kind = .Integer, .value = "15" },
        Token{ .kind = .RightParen },
        Token{ .kind = .DotComma },

        // While loop
        Token{ .kind = .While },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .GreaterThan },
        Token{ .kind = .Integer, .value = "0" },
        Token{ .kind = .LeftBrace },

        // Loop body
        Token{ .kind = .Identifier, .value = "accum" },
        Token{ .kind = .Assign },
        Token{ .kind = .Identifier, .value = "accum" },
        Token{ .kind = .Plus },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .DotComma },

        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .Assign },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .Minus },
        Token{ .kind = .Integer, .value = "1" },
        Token{ .kind = .DotComma },

        Token{ .kind = .RightBrace },

        Token{ .kind = .Identifier, .value = "println" },
        Token{ .kind = .Exclamation },
        Token{ .kind = .LeftParen },
        Token{ .kind = .String, .value = "{}" },
        Token{ .kind = .Comma },
        Token{ .kind = .Integer, .value = "16" },
        Token{ .kind = .RightParen },
        Token{ .kind = .DotComma },

        // Return value
        Token{ .kind = .Identifier, .value = "accum" },
        Token{ .kind = .RightBrace },

        // Main function
        Token{ .kind = .FN },
        Token{ .kind = .Identifier, .value = "main" },
        Token{ .kind = .LeftParen },
        Token{ .kind = .RightParen },
        Token{ .kind = .LeftBrace },

        Token{ .kind = .Let },
        Token{ .kind = .Identifier, .value = "y" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .DotComma },

        Token{ .kind = .Unsafe },
        Token{ .kind = .LeftBrace },

        Token{ .kind = .Identifier, .value = "A" },
        Token{ .kind = .Assign },
        Token{ .kind = .Integer, .value = "10" },
        Token{ .kind = .DotComma },

        Token{ .kind = .Identifier, .value = "B" },
        Token{ .kind = .Assign },
        Token{ .kind = .Integer, .value = "11" },
        Token{ .kind = .DotComma },

        Token{ .kind = .RightBrace },

        Token{ .kind = .Identifier, .value = "println" },
        Token{ .kind = .Exclamation },
        Token{ .kind = .LeftParen },
        Token{ .kind = .String, .value = "{}" },
        Token{ .kind = .Comma },
        Token{ .kind = .Integer, .value = "14" },
        Token{ .kind = .RightParen },
        Token{ .kind = .DotComma },

        Token{ .kind = .Identifier, .value = "y" },
        Token{ .kind = .Assign },
        Token{ .kind = .Identifier, .value = "suma" },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Integer, .value = "4" },
        Token{ .kind = .RightParen },
        Token{ .kind = .DotComma },

        Token{ .kind = .Identifier, .value = "println" },
        Token{ .kind = .Exclamation },
        Token{ .kind = .LeftParen },
        Token{ .kind = .String, .value = "{}" },
        Token{ .kind = .Comma },
        Token{ .kind = .Identifier, .value = "y" },
        Token{ .kind = .RightParen },
        Token{ .kind = .DotComma },

        Token{ .kind = .RightBrace },

        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "Test for code snippet 4" {
    const input =
        \\static mut A: i32 = 0;
        \\static mut B: i32 = 0;
        \\
        \\fn sumarec(x: i32) -> i32 {
        \\    println!("{}", x);
        \\    if x < 1 {
        \\        return 0;
        \\    }
        \\    return x + sumarec(x - 1);
        \\}
        \\
        \\fn main() {
        \\    let y = sumarec(4);
        \\    println!("{}", y);
        \\}
    ;

    const expected_tokens = [_]Token{
        // Global variables
        Token{ .kind = .Static },
        Token{ .kind = .Mut },
        Token{ .kind = .Identifier, .value = "A" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .Assign },
        Token{ .kind = .Integer, .value = "0" },
        Token{ .kind = .DotComma },

        Token{ .kind = .Static },
        Token{ .kind = .Mut },
        Token{ .kind = .Identifier, .value = "B" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .Assign },
        Token{ .kind = .Integer, .value = "0" },
        Token{ .kind = .DotComma },

        // Function declaration
        Token{ .kind = .FN },
        Token{ .kind = .Identifier, .value = "sumarec" },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .DoubleColon },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .RightParen },
        Token{ .kind = .Arrow },
        Token{ .kind = .Type, .value = "i32" },
        Token{ .kind = .LeftBrace },

        // Function body
        Token{ .kind = .Identifier, .value = "println" },
        Token{ .kind = .Exclamation },
        Token{ .kind = .LeftParen },
        Token{ .kind = .String, .value = "{}" },
        Token{ .kind = .Comma },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .RightParen },
        Token{ .kind = .DotComma },

        Token{ .kind = .If },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .LessThan },
        Token{ .kind = .Integer, .value = "1" },
        Token{ .kind = .LeftBrace },

        Token{ .kind = .FunReturn },
        Token{ .kind = .Integer, .value = "0" },
        Token{ .kind = .DotComma },

        Token{ .kind = .RightBrace },

        Token{ .kind = .FunReturn },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .Plus },
        Token{ .kind = .Identifier, .value = "sumarec" },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Identifier, .value = "x" },
        Token{ .kind = .Minus },
        Token{ .kind = .Integer, .value = "1" },
        Token{ .kind = .RightParen },
        Token{ .kind = .DotComma },

        Token{ .kind = .RightBrace },

        // Main function
        Token{ .kind = .FN },
        Token{ .kind = .Identifier, .value = "main" },
        Token{ .kind = .LeftParen },
        Token{ .kind = .RightParen },
        Token{ .kind = .LeftBrace },

        Token{ .kind = .Let },
        Token{ .kind = .Identifier, .value = "y" },
        Token{ .kind = .Assign },
        Token{ .kind = .Identifier, .value = "sumarec" },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Integer, .value = "4" },
        Token{ .kind = .RightParen },
        Token{ .kind = .DotComma },

        Token{ .kind = .Identifier, .value = "println" },
        Token{ .kind = .Exclamation },
        Token{ .kind = .LeftParen },
        Token{ .kind = .String, .value = "{}" },
        Token{ .kind = .Comma },
        Token{ .kind = .Identifier, .value = "y" },
        Token{ .kind = .RightParen },
        Token{ .kind = .DotComma },

        Token{ .kind = .RightBrace },

        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

fn test_lexer(input: []const u8, expected_tokens: []const Token) !void {
    var lexer = Lexer{
        .input = input,
    };
    var index: usize = 0;

    while (true) {
        const token = lexer.next_token();

        try std.testing.expectEqual(expected_tokens[index].kind, token.kind);
        try std.testing.expect(std.mem.eql(u8, expected_tokens[index].value, token.value));
        if (token.kind == .EndOfFile) break;
        index += 1;
    }
}
