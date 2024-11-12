const std = @import("std");

pub const log_level: std.log.Level = .info;
/// Represents the different types of tokens that can be identified in a rust program
pub const TokenType = enum {
    /// Represents an integer number.
    Integer,

    /// Represents a floating-point number.
    Float,

    /// Represents a boolean.
    Boolean,

    /// Represents a string.
    String,
    Char,

    // functions
    /// 'fn'
    FN,
    /// 'return'
    FunReturn,
    /// '->' used to declare a function return type.
    Arrow,

    Loop,
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
    /// '=' used to assign a lexeme to a variable.
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

    LogicalOr,
LogicalAnd ,

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
    NotEqual,

    /// Represents the end of the input.
    EndOfFile,
    /// Represents an invalid or unrecognized token.
    Invalid,

    // keywords

    /// Represents a variable type
    Type,

    /// ID
    Identifier,

    /// Comments (they are ignored during lexing)
    // SingleCommentSlash,
    // MultiCommentStart,
    // MultiCommentEnd,
    DocString,

    // Pending implementation
    Struct,
};

/// A token with its type and optional lexeme. Used by the parser to interpret the expression.
pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8 = "",

    fn print_lexeme(self: Token, writer: anytype, name: []const u8) !void {
        try writer.print("{s}: {s}\n", .{ name, self.lexeme });
    }

    /// Formats the token for display.
    pub fn format(self: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.token_type) {
            // Numeric tokens
            TokenType.Integer => try print_lexeme(self, writer, "Integer"),

            TokenType.Float => try print_lexeme(self, writer, "Float"),
            // Literal tokens
            TokenType.Boolean => try print_lexeme(self, writer, "Boolean"),

            TokenType.String => try print_lexeme(self, writer, "String"),
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

            // TODO: print the correct lexeme ({})
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
            TokenType.NotEqual => try writer.print("!=", .{}),
            // Identifier and type tokens
            TokenType.Identifier => try print_lexeme(self, writer, "ID"),

            TokenType.Type => try print_lexeme(self, writer, "Type"),
            // End of file and invalid tokens
            TokenType.EndOfFile => try writer.print("EOF", .{}),
            TokenType.Invalid => try writer.print("Invalid token", .{}),
            else => try print_lexeme(self, writer, "Unknown"),
        }
    }
};

/// The `Lexer` structure processes a mathematical expression into a series of tokens.
pub const Lexer = struct {
    input: []const u8,
    current: usize = 0,

    /// Retrieves a list of tokens from the input.
    ///
    /// # Returns
    /// A list of `Token` representing the parts of the expression.
    pub fn tokenize(self: *Lexer) ![]Token {
        var tokens = std.ArrayList(Token).init(std.heap.page_allocator);
        std.debug.print("Tokens: \n", .{});
        while (true) {
            const token = self.next_token();
            if (token.token_type == .EndOfFile) {
                break;
            }
            std.debug.print("{}\n", .{ token});
            try tokens.append(token);
        }
        std.debug.print("\n\n", .{});
        return try tokens.toOwnedSlice();
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
            return Token{ .token_type = .EndOfFile };
        }

        const c = self.input[self.current];
        self.current += 1;

        switch (c) {
            // match single characters
            '{' => return Token{ .token_type = .LeftBrace },
            '}' => return Token{ .token_type = .RightBrace },
            ':' => return Token{ .token_type = .DoubleColon },
            ',' => return Token{ .token_type = .Comma },
            ';' => return Token{ .token_type = .DotComma },
            '(' => return Token{ .token_type = .LeftParen },
            ')' => return Token{ .token_type = .RightParen },

            '+' => return Token{ .token_type = .Plus },
            '^' => return Token{ .token_type = .Power },
            '%' => return Token{ .token_type = .Modulo },
            '&' => return Token{ .token_type = .Ampersand },
            '!' => return Token{ .token_type = .Exclamation },
            '"' => return parse_string(self),
            '\'' => return parse_char(self),
            '*' => return Token{ .token_type = .Multiply }, // overlap with */, but is handled when parsing /*
            // '/' => return Token{ .token_type = .Divide }, // overlap with /*, //, ///
            // '=' => return Token{ .token_type = .Assign }, // overlap with ==
            // '-' => return Token{ .token_type = .Minus }, // overlap with ->
            // '!' => return Token{ .token_type = .Factorial }, // overlap with !=
            // '<' => return Token{ .token_type = .LessThan }, // overlap with <=
            // '>' => return Token{ .token_type = .GreaterThan }, // overlap with >=
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

                return Token{ .token_type = .Invalid };
            },
        }
    }

    fn parse_keyword(self: *Lexer) Token {
        // read word with whitespace

        const start = self.current;

        while (self.current < self.input.len and !is_whitespace(self.input[self.current]) and is_alpha(self.input[self.current])) {
            self.current += 1;
        }

        const word = self.input[start..self.current];

        if (std.mem.eql(u8, word, "fn")) {
            return Token{ .token_type = .FN };
        }
        if (std.mem.eql(u8, word, "return")) {
            return Token{ .token_type = .FunReturn };
        }
        if (std.mem.eql(u8, word, "let")) {
            return Token{ .token_type = .Let };
        }
        if (std.mem.eql(u8, word, "mut")) {
            return Token{ .token_type = .Mut };
        }
        if (std.mem.eql(u8, word, "static")) {
            return Token{ .token_type = .Static };
        }

        if (std.mem.eql(u8, word, "unsafe")) {
            return Token{ .token_type = .Unsafe };
        }

        if (std.mem.eql(u8, word, "while")) {
            return Token{ .token_type = .While };
        }
        if (std.mem.eql(u8, word, "for")) {
            return Token{ .token_type = .For };
        }
        if (std.mem.eql(u8, word, "in")) {
            return Token{ .token_type = .In };
        }
        if (std.mem.eql(u8, word, "if")) {
            return Token{ .token_type = .If };
        }
        if (std.mem.eql(u8, word, "else")) {
            return Token{ .token_type = .Else };
        }

        // types (i32, i64, f32, f64, bool, str)
        if (std.mem.eql(u8, word, "i32")) {
            return Token{ .token_type = .Type, .lexeme = word };
        }
        if (std.mem.eql(u8, word, "f32")) {
            return Token{ .token_type = .Type, .lexeme = word };
        }
        if (std.mem.eql(u8, word, "i64")) {
            return Token{ .token_type = .Type, .lexeme = word };
        }
        if (std.mem.eql(u8, word, "f64")) {
            return Token{ .token_type = .Type, .lexeme = word };
        }
        if (std.mem.eql(u8, word, "bool")) {
            return Token{ .token_type = .Type, .lexeme = word };
        }
        if (std.mem.eql(u8, word, "str")) {
            return Token{ .token_type = .Type, .lexeme = word };
        }

        // boolean lexemes
        if (std.mem.eql(u8, word, "true")) {
            return Token{ .token_type = .Boolean, .lexeme = word };
        }
        if (std.mem.eql(u8, word, "false")) {
            return Token{ .token_type = .Boolean, .lexeme = word };
        }

        return Token{ .token_type = .Identifier, .lexeme = word };
    }

    /// parses a possible 1-3 char symbol
    fn parse_small_char(self: *Lexer, prev: u8) Token {
        // test for any of:
        // =, ==, -, ->, !, !=, <, <=, >, >=, /, /*, //, ///

        // exceeded input
        if (self.current >= self.input.len) {
            return Token{ .token_type = .EndOfFile };
        }

        const next = self.input[self.current];

        if (prev == '=') {
            if (next == '=') {
                self.current += 1;
                return Token{ .token_type = .Equal };
            }
            return Token{ .token_type = .Assign };
        }
        if (prev == '-') {
            if (next == '>') {
                self.current += 1;
                return Token{ .token_type = .Arrow };
            }
            return Token{ .token_type = .Minus };
        }
        if (prev == '!') {
            if (next == '=') {
                self.current += 1;
                return Token{ .token_type = .NotEqual };
            }
            return Token{ .token_type = .Factorial };
        }
        if (prev == '<') {
            if (next == '=') {
                self.current += 1;
                return Token{ .token_type = .LessThanOrEqual };
            }
            return Token{ .token_type = .LessThan };
        }
        if (prev == '>') {
            if (next == '=') {
                self.current += 1;
                return Token{ .token_type = .GreaterThanOrEqual };
            }
            return Token{ .token_type = .GreaterThan };
        }
        //comments

        if (prev == '/') {
            if (next == '/') {
                self.current += 1;
                if (self.current >= self.input.len) {
                    return Token{ .token_type = .EndOfFile };
                } else if (self.input[self.current] == '/') {
                    self.current += 1;

                    // skip whitespace
                    while (self.current < self.input.len and is_whitespace(self.input[self.current])) {
                        self.current += 1;
                    }

                    const doc_string_start: usize = self.current;
                    while (self.current < self.input.len and self.input[self.current] != '\n') {
                        self.current += 1;
                    }

                    return Token{ .token_type = .DocString, .lexeme = self.input[doc_string_start..self.current] };
                }

                // single line comment
                while (self.current < self.input.len and self.input[self.current] != '\n') {
                    self.current += 1;
                }
                return self.next_token();
            }
            if (next == '*') {
                // multi line comment
                while (self.current < self.input.len) {
                    if (self.input[self.current] == '*' and self.input[self.current + 1] == '/') {
                        self.current += 2;
                        break;
                    }
                    self.current += 1;
                }
                return self.next_token();
            }
            return Token{ .token_type = .Divide };
        }

        return Token{ .token_type = .Invalid };
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
            return Token{ .token_type = .Integer, .lexeme = int_value };
        }

        if (!is_dot(self.input[self.current])) {
            return Token{ .token_type = .Integer, .lexeme = int_value };
        }
        // dot found
        self.current += 1;
        lookup_integer(self);

        const float_value = self.input[start..self.current];
        return Token{ .token_type = .Float, .lexeme = float_value };
    }

    fn parse_string(self: *Lexer) Token {
        const start = self.current;
        while (self.current < self.input.len and self.input[self.current] != '"') {
            self.current += 1;
        }
        if (self.current >= self.input.len) {
            return Token{ .token_type = .Invalid };
        }
        self.current += 1;

        const string_value = self.input[start .. self.current - 1];
        return Token{ .token_type = .String, .lexeme = string_value };
    }
    fn parse_char(self: *Lexer) Token {

        const char_slice = self.input[self.current..self.current + 1];
        self.current += 1;

        if (self.current >= self.input.len) {
            return Token{ .token_type = .Invalid };
        }

        if (self.input[self.current] != '\'') {
            return Token{ .token_type = .Invalid };
        }
        self.current += 1;

        return Token{ .token_type = .Char, .lexeme = char_slice };

    }
};

/// checks if a char leads to a 2-3 char symbol
/// # Parameters
/// - `c`: The character to check.
///
/// # Returns
/// `true` if the character could be a small char, `false` otherwise.
fn is_small_char_begin(c: u8) bool {
    return c == '<' or c == '>' or c == '=' or c == '!' or c == '-' or c == '/' or c == '*';
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
        Token{ .token_type = .FunReturn },
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
        Token{ .token_type = .FunReturn },
        Token{ .token_type = .Identifier, .lexeme = "result" },
        Token{ .token_type = .DotComma },
        Token{ .token_type = .RightBrace },
        Token{ .token_type = .EndOfFile },
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

        Token{ .token_type = .FunReturn },
        Token{ .token_type = .Integer, .lexeme = "0" },
        Token{ .token_type = .DotComma },

        Token{ .token_type = .RightBrace },

        Token{ .token_type = .FunReturn },
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
    var lexer = Lexer{
        .input = input,
    };
    var index: usize = 0;

    while (true) {
        const token = lexer.next_token();

        try std.testing.expectEqual(expected_tokens[index].token_type, token.token_type);
        try std.testing.expect(std.mem.eql(u8, expected_tokens[index].lexeme, token.lexeme));
        if (token.token_type == .EndOfFile) break;
        index += 1;
    }
}
