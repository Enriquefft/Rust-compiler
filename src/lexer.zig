const std = @import("std");

/// Represents the different types of tokens that can be identified in a mathematical expression.
const TokenType = enum {
    /// Represents a numeric value.
    Number,
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
    /// Represents the '(' character.
    LeftParen,
    /// Represents the ')' character.
    RightParen,
    /// Represents the end of the input.
    EndOfFile,
    /// Represents an invalid or unrecognized token.
    Invalid,

    // keywords

    /// variable declaration
    ID,

    /// print function
    PRINT,

    /// Assignment operator
    ASSIGN,

    /// dot-comma (;)
    DC,
};

/// A token with its type and optional value. Used by the parser to interpret the expression.
pub const Token = struct {
    kind: TokenType,
    value: ?f64 = null, // only used for numbers

    /// Formats the token for display.
    pub fn format(self: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.kind) {
            TokenType.Number => {
                if (self.value) |val| {
                    try writer.print("{any}", .{val});
                } else {
                    try writer.print("Invalid number", .{});
                }
            },
            TokenType.Plus => try writer.print("+", .{}),
            TokenType.Minus => try writer.print("-", .{}),
            TokenType.Multiply => try writer.print("*", .{}),
            TokenType.Divide => try writer.print("/", .{}),
            TokenType.Power => try writer.print("^", .{}),
            TokenType.Modulo => try writer.print("%", .{}),
            TokenType.Factorial => try writer.print("!", .{}),
            TokenType.LeftParen => try writer.print("(", .{}),
            TokenType.RightParen => try writer.print(")", .{}),
            TokenType.EndOfFile => try writer.print("EOF", .{}),
            TokenType.Invalid => try writer.print("Invalid token", .{}),
        }
    }
};

/// The `Lexer` structure processes a mathematical expression into a series of tokens.
pub const Lexer = struct {
    input: []const u8,
    current: usize = 0,

    fn parse_keyword(_: *Lexer) Token {
        // read word will whitespace

        // var palabra
        //
        // while (!whitespace){
        //     current += 1;
        //     palabra += self.input[self.current];
        // }
        //
        // if (palabra == print)
        //
        // if (palabra == assign)
        //
        return Token{ .kind = .ID };
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

        // Match characters to their corresponding tokens
        switch (c) {
            '+' => return Token{ .kind = .Plus },
            '-' => return Token{ .kind = .Minus },
            '*' => return Token{ .kind = .Multiply },
            '/' => return Token{ .kind = .Divide },
            '^' => return Token{ .kind = .Power },
            '%' => return Token{ .kind = .Modulo },
            '!' => return Token{ .kind = .Factorial },
            '(' => return Token{ .kind = .LeftParen },
            ')' => return Token{ .kind = .RightParen },
            ';' => return Token{ .kind = .DC },
            else => {
                if (is_digit(c)) {
                    var number: f64 = 0;

                    number = @floatFromInt(c - '0');

                    // Process the non-decimal part of the number
                    while (self.current < self.input.len and is_digit(self.input[self.current])) {
                        number = number * 10 + @as(f64, @floatFromInt(self.input[self.current] - '0'));

                        self.current += 1;
                    }

                    // reached end of input or not a decimal number
                    if ((self.current >= self.input.len) or !is_dot(self.input[self.current])) {
                        return Token{ .kind = .Number, .value = number };
                    }

                    // Process the decimal part of the number
                    self.current += 1;
                    var decimal: f64 = 0.1;
                    while (self.current < self.input.len and is_digit(self.input[self.current])) {
                        number += @as(f64, @floatFromInt(self.input[self.current] - '0')) * decimal;
                        decimal /= 10;
                        self.current += 1;
                    }
                    return Token{ .kind = .Number, .value = number };
                } else {
                    // ID, PRINT or ASSIGN
                    if (is_letter(c)) {
                        return parse_keyword(self);
                    }

                    return Token{ .kind = .Invalid };
                }
            },
        }
    }
};

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

test "lexer tokenizes simple number" {
    const input = "42";
    const expected_tokens = [_]Token{
        Token{ .kind = .Number, .value = 42 },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "lexer tokenizes simple addition" {
    const input = "1 + 2";
    const expected_tokens = [_]Token{
        Token{ .kind = .Number, .value = 1 },
        Token{ .kind = .Plus },
        Token{ .kind = .Number, .value = 2 },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "lexer tokenizes simple arithmetic expression" {
    const input = "1 + 2";
    const expected_tokens = [_]Token{
        Token{ .kind = .Number, .value = 1 },
        Token{ .kind = .Plus },
        Token{ .kind = .Number, .value = 2 },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "lexer tokenizes simple mod/exp expression" {
    const input = "2 ^ 3 % 4";
    const expected_tokens = [_]Token{
        Token{ .kind = .Number, .value = 2 },
        Token{ .kind = .Power },
        Token{ .kind = .Number, .value = 3 },
        Token{ .kind = .Modulo },
        Token{ .kind = .Number, .value = 4 },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "lexer tokenizes complex mixed expression" {
    const input = "5! + 3 * 2 - 8 / 4";
    const expected_tokens = [_]Token{
        Token{ .kind = .Number, .value = 5 },
        Token{ .kind = .Factorial },
        Token{ .kind = .Plus },
        Token{ .kind = .Number, .value = 3 },
        Token{ .kind = .Multiply },
        Token{ .kind = .Number, .value = 2 },
        Token{ .kind = .Minus },
        Token{ .kind = .Number, .value = 8 },
        Token{ .kind = .Divide },
        Token{ .kind = .Number, .value = 4 },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}
test "lexer tokenizes complex nested expression" {
    const input = "(2 + 3) * ((7 - 2) ^ 2) / 5";
    const expected_tokens = [_]Token{
        Token{ .kind = .LeftParen },
        Token{ .kind = .Number, .value = 2 },
        Token{ .kind = .Plus },
        Token{ .kind = .Number, .value = 3 },
        Token{ .kind = .RightParen },
        Token{ .kind = .Multiply },
        Token{ .kind = .LeftParen },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Number, .value = 7 },
        Token{ .kind = .Minus },
        Token{ .kind = .Number, .value = 2 },
        Token{ .kind = .RightParen },
        Token{ .kind = .Power },
        Token{ .kind = .Number, .value = 2 },
        Token{ .kind = .RightParen },
        Token{ .kind = .Divide },
        Token{ .kind = .Number, .value = 5 },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}
test "lexer tokenizes nested big numbers with factorial" {
    const input = "((1000000 + 0.0001) * 3!) / 2";
    const expected_tokens = [_]Token{
        Token{ .kind = .LeftParen },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Number, .value = 1000000 },
        Token{ .kind = .Plus },
        Token{ .kind = .Number, .value = 0.0001 },
        Token{ .kind = .RightParen },
        Token{ .kind = .Multiply },
        Token{ .kind = .Number, .value = 3 },
        Token{ .kind = .Factorial },
        Token{ .kind = .RightParen },
        Token{ .kind = .Divide },
        Token{ .kind = .Number, .value = 2 },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}
test "lexer tokenizes complex nested expression 2" {
    const input = "((5! + 2^3) * (3 - 0.001))";
    const expected_tokens = [_]Token{
        Token{ .kind = .LeftParen },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Number, .value = 5 },
        Token{ .kind = .Factorial },
        Token{ .kind = .Plus },
        Token{ .kind = .Number, .value = 2 },
        Token{ .kind = .Power },
        Token{ .kind = .Number, .value = 3 },
        Token{ .kind = .RightParen },
        Token{ .kind = .Multiply },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Number, .value = 3 },
        Token{ .kind = .Minus },
        Token{ .kind = .Number, .value = 0.001 },
        Token{ .kind = .RightParen },
        Token{ .kind = .RightParen },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}
test "lexer tokenizes complex nested expression 3" {
    const input = "((10!) % (20! / 15)) * 1000000";
    const expected_tokens = [_]Token{
        Token{ .kind = .LeftParen },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Number, .value = 10 },
        Token{ .kind = .Factorial },
        Token{ .kind = .RightParen },
        Token{ .kind = .Modulo },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Number, .value = 20 },
        Token{ .kind = .Factorial },
        Token{ .kind = .Divide },
        Token{ .kind = .Number, .value = 15 },
        Token{ .kind = .RightParen },
        Token{ .kind = .RightParen },
        Token{ .kind = .Multiply },
        Token{ .kind = .Number, .value = 1000000 },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

test "lexer tokenizes complex nested expression with big numbers" {
    const input = "((((1 + 2) * 3!) - 100000) / (0.000001 + 5))";
    const expected_tokens = [_]Token{
        Token{ .kind = .LeftParen },
        Token{ .kind = .LeftParen },
        Token{ .kind = .LeftParen },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Number, .value = 1 },
        Token{ .kind = .Plus },
        Token{ .kind = .Number, .value = 2 },
        Token{ .kind = .RightParen },
        Token{ .kind = .Multiply },
        Token{ .kind = .Number, .value = 3 },
        Token{ .kind = .Factorial },
        Token{ .kind = .RightParen },
        Token{ .kind = .Minus },
        Token{ .kind = .Number, .value = 100000 },
        Token{ .kind = .RightParen },
        Token{ .kind = .Divide },
        Token{ .kind = .LeftParen },
        Token{ .kind = .Number, .value = 0.000001 },
        Token{ .kind = .Plus },
        Token{ .kind = .Number, .value = 5 },
        Token{ .kind = .RightParen },
        Token{ .kind = .RightParen },
        Token{ .kind = .EndOfFile },
    };

    try test_lexer(input, &expected_tokens);
}

const tolerance = std.math.floatEps(f64);
fn test_lexer(input: []const u8, expected_tokens: []const Token) !void {
    var lexer = Lexer{
        .input = input,
    };
    var index: usize = 0;
    while (true) {
        const token = lexer.next_token();
        try std.testing.expectEqual(expected_tokens[index].kind, token.kind);
        if (token.kind == .Number) {
            const expected_value = expected_tokens[index].value.?;
            const actual_value = token.value.?;
            try std.testing.expect(std.math.approxEqAbs(f64, expected_value, actual_value, tolerance));
        }
        if (token.kind == .EndOfFile) break;
        index += 1;
    }
}
