//! Parser for basic arithmetic expressions.
//!
//! basic grammar:
//! expression   : term ((+ | -) term)*
//! term         : factor ((* | /) factor)*
//! factor       : NUMBER | '(' expression ')'
//! NUMBER       : [0-9]+
//!
//! complete grammar:
//! expression   : term (('+' | '-') term)*
//! term         : factor (('*' | '/' | '%') factor)*
//! factor       : power ('!'?)
//! power        : primary ('^' power)?
//! primary      : NUMBER | '(' expression ')'
//! NUMBER       : [0-9]+

const std = @import("std");
const TokenType = @import("lexer.zig").TokenType;
const Token = @import("lexer.zig").Token;
const Lexer = @import("lexer.zig").Lexer;
const Factorial = @import("factorial.zig");

const ParsingErrors = error{} || Factorial.FactorialErrors;

pub fn parse(input: []const u8) ParsingErrors!f64 {
    var lexer = Lexer{ .input = input };
    var parser = Parser{ .lexer = &lexer, .current_token = lexer.next_token() };
    return try parser.parse_expression();
}

/// The `Parser` structure processes tokens into an evaluated result.
///
/// The parser uses a recursive descent method to handle the grammar of basic
/// arithmetic expressions.
const Parser = struct {
    lexer: *Lexer,
    current_token: Token,

    /// Advances to the next token in the input stream.
    fn advance(self: *Parser) void {
        self.current_token = self.lexer.next_token();
    }

    /// Parses and evaluates an expression, which may consist of one or more terms
    /// combined by `+` or `-` operators.
    ///
    /// # Returns
    /// The result of the evaluated expression.
    fn parse_expression(self: *Parser) !f64 {
        var result = try self.parse_term();

        // Handle addition and subtraction operators
        while (self.current_token.kind == .Plus or self.current_token.kind == .Minus) {
            const token = self.current_token;
            self.advance();

            if (token.kind == .Plus) {
                result += try self.parse_term();
            } else if (token.kind == .Minus) {
                result -= try self.parse_term();
            }
        }

        return result;
    }

    /// Parses and evaluates a term, which may consist of one or more factors
    /// combined by `*`, '%' or `/` operators.
    ///
    /// # Returns
    /// The result of the evaluated term.
    fn parse_term(self: *Parser) ParsingErrors!f64 {
        var result = try self.parse_factor();

        // Handle multiplication and division operators
        while (self.current_token.kind == .Multiply or self.current_token.kind == .Divide or self.current_token.kind == .Modulo) {
            const token = self.current_token;
            self.advance();

            if (token.kind == .Multiply) {
                result *= try self.parse_factor();
            } else if (token.kind == .Divide) {
                result /= try self.parse_factor();
            } else if (token.kind == .Modulo) {
                // result = try std.math.rem(f64, result, self.parse_factor());
                result = @rem(result, try self.parse_factor());
            }
        }

        return result;
    }

    /// Parses and evaluates a factor, which is the most basic unit of an expression after applying power and factorial,
    /// such as a number or a parenthesized sub-expression and optionally, a power.
    ///
    /// # Returns
    /// The result of the evaluated factor.
    fn parse_factor(self: *Parser) ParsingErrors!f64 {
        var result = try self.parse_power();

        if (self.current_token.kind == .Factorial) {
            self.advance();
            result = @as(f64, @floatFromInt(try Factorial.factorial(i64, @as(i8, @intFromFloat(result)))));
        }
        return result;
    }

    /// Parses and evaluates a power, which is a factor raised to another factor.
    ///
    /// # Returns
    /// The result of the evaluated power.
    fn parse_power(self: *Parser) ParsingErrors!f64 {
        var result = try self.parse_primary();
        if (self.current_token.kind == .Power) {
            self.advance();
            result = std.math.pow(f64, result, try self.parse_power());
        }
        return result;
    }

    // primary
    /// Parses and evaluates a primary, which is the simples form of a factor, a number or a parenthesis expression.
    ///
    /// # Returns
    /// The result of the evaluated primary.
    fn parse_primary(self: *Parser) ParsingErrors!f64 {
        const token = self.current_token;
        self.advance();
        switch (token.kind) {
            .Number => return token.value.?,
            .Minus => {
                return -try self.parse_primary();
            },
            .LeftParen => {
                const result = try self.parse_expression();
                if (self.current_token.kind != .RightParen) {
                    std.debug.print("Expected ')'", .{});
                }
                self.advance();
                return result;
            },
            else => {
                std.debug.print("Unexpected token: {s}\n", .{token});
                return 0.0;
            },
        }
    }
};
