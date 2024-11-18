const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
const serialize = @import("serialize.zig").serialize_program;

/// Comprehensive error set for the Parser
pub const ParseError = error{
    UnexpectedToken,
    ExpectedIdentifier,
    ExpectedType,
    UnterminatedBlock,
    UnexpectedEndOfFile,
    InvalidExpression,
    InvalidStatement,
    ExpectedFunction,
    ExpectedParameterList,
    ExpectedBlock,
    ExpectedRightParen,
    ExpectedLeftBrace,
    ExpectedRightBrace,
    ExpectedComma,
    ExpectedArrow,
    ExpectedLiteral,
    ExpectedExpression,
    NotImplemented,
};

/// Internal error for the parser
const TestParseError = ParseError || error{ TestUnexpectedResult, TestExpectedEqual };

/// Represents the abstract syntax tree of a Rust program
pub const Program = struct {
    items: []const Item,
};

/// Item represents any top-level item in Rust (functions, structs, enums, etc.), each with an optional docstring.
pub const Item = struct {
    docstring: ?[]const u8,
    content: ItemContent,
};

pub const ItemContent = union(enum) {
    Function: Function,
};

pub const Function = struct {
    is_unsafe: bool,
    fname: []const u8,
    generics: ?Generics,
    parameters: []const Parameter,
    return_type: ?TypeAnnotation,
    body: Block,
};

pub const Parameter = struct {
    name: []const u8,
    type_annotation: TypeAnnotation,
};

/// Generics represents generic parameters in functions, structs, enums, etc.
pub const Generics = struct {
    params: []GenericParam,
};

/// GenericParam represents a single generic parameter with optional bounds.
pub const GenericParam = struct {
    name: []const u8,
    bounds: []const u8,
};

/// TypeAnnotation represents type information, including references and mutability.
pub const TypeAnnotation = struct {
    is_reference: bool,
    is_mutable: bool,
    base_type: *const BaseType,
};

pub const BaseType = union(enum) {
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    Bool,
    Char,
    Str,
    Tuple: TupleType,
    Array: ArrayType,
    Function: FunctionType,
    Generic: GenericType,
    Path: []const u8, // Represents paths like std::vec::Vec & custom types
};

pub const TupleType = struct {
    types: []const TypeAnnotation,
};

pub const ArrayType = struct {
    element_type: *const TypeAnnotation,
    size: u64, // Assuming size is known at compile time
};

pub const FunctionType = struct {
    parameters: []const TypeAnnotation,
    return_type: ?*const TypeAnnotation,
};

pub const GenericType = struct {
    name: []const u8,
    type_params: []TypeAnnotation,
};

/// Block represents a block of statements with an optional trailing expression.
pub const Block = struct {
    statements: []const Statement,
    expression: ?*const Expression,
};

/// Statement represents a single statement within a block.
pub const Statement = union(enum) { LetStatement: LetStatement, Expression: *const Expression, Item: *const Item };

/// LetStatement represents a variable declaration.
pub const LetStatement = struct {
    is_mutable: bool,
    pattern: Pattern,
    type_annotation: ?TypeAnnotation,
    value: ?*const Expression,
};

/// Pattern represents different kinds of patterns in let statements.
pub const Pattern = union(enum) {
    Identifier: []const u8,
    Wildcard,
    Tuple: TuplePattern,
    Struct: StructPattern,
};

pub const TuplePattern = struct {
    patterns: []Pattern,
};

pub const StructPattern = struct {
    name: []const u8,
    fields: []StructPatternField,
};

pub const StructPatternField = struct {
    name: []const u8,
    pattern: Pattern,
};

/// Expression represents all possible expressions in the language.
pub const Expression = union(enum) {
    Assignment: *const AssignmentExpression,
    Conditional: *const ConditionalExpression,
    Loop: *const LoopExpression,
    FunctionCall: FunctionCall,
    BinaryOperation: BinaryOperation,
    UnaryOperation: UnaryOperation,
    Literal: Literal,
    Identifier: []const u8,
};

pub const AssignmentExpression = struct {
    identifier: []const u8,
    value: *const Expression,
};

pub const ConditionalExpression = struct {
    condition: *const Expression,
    then_branch: Block,
    else_branch: ?Block,
};

pub const Literal = union(enum) {
    Integer: i64,
    Float: f64,
    Boolean: bool,
    String: []const u8,
    Char: u8,
};

pub const BinaryOperation = struct {
    left: *const Expression,
    operator: BinaryOperator,
    right: *const Expression,
};

pub const UnaryOperation = struct {
    operator: UnaryOperator,
    operand: *const Expression,
};

pub const FunctionCall = struct {
    function_name: []const u8,
    arguments: []const *const Expression,
};

pub const BinaryOperator = enum {
    LogicalOr,
    LogicalAnd,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Add,
    Subtract,
    Multiply,
    Divide,
};

pub const UnaryOperator = enum {
    Negate,
    LogicalNot, // (!)
};

pub const LoopExpression = union(enum) {
    Infinite: Loop,
    While: WhileLoop,
    For: ForLoop,
};

/// Loop represents an infinite loop (`loop { ... }`).
pub const Loop = struct {
    body: Block,
};

pub const WhileLoop = struct {
    condition: *const Expression,
    body: Block,
};

pub const ForLoop = struct {
    iterator: Pattern,
    iterable: *const Expression,
    body: Block,
};

/// Parser structure that maintains parsing state and methods.
pub const Parser = struct {
    tokens: []const Token,
    current: usize, // Index of the current token
    allocator: std.mem.Allocator,

    /// Initializes a new Parser.
    pub fn init(tokens: []const Token, allocator: std.mem.Allocator) Parser {
        return Parser{
            .tokens = tokens,
            .current = 0,
            .allocator = allocator,
        };
    }

    /// Peeks at the current token without advancing.
    fn peek(self: *Parser) ?Token {
        if (self.current < self.tokens.len) {
            return self.tokens[self.current];
        } else {
            return null;
        }
    }

    /// Advances to the next token and returns the current token.
    fn advance(self: *Parser) ?Token {
        if (self.current < self.tokens.len) {
            const token = self.tokens[self.current];
            self.current += 1;
            return token;
        } else {
            return null;
        }
    }

    /// Checks if the current token matches any in `token_types` and consumes it.
    fn matchTokens(self: *Parser, token_types: []const TokenType) bool {
        if (self.peek()) |token| {
            for (token_types) |token_type| {
                if (token.token_type == token_type) {
                    _ = self.advance();
                    return true;
                }
            }
        }
        return false;
    }

    // Checks if the current token matches the given `token_type` and consumes it.
    fn matchToken(self: *Parser, token_type: TokenType) bool {
        if (self.peek()) |token| {
            if (token.token_type == token_type) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    /// Expects the current token to match `token_type` and consumes it.
    /// Returns an error if it does not match.
    fn expectToken(self: *Parser, token_type: TokenType) !Token {
        const current_token = self.peek() orelse return ParseError.UnexpectedEndOfFile;
        if (current_token.token_type != token_type) {
            return ParseError.UnexpectedToken;
        }
        return self.advance() orelse ParseError.UnexpectedEndOfFile;
    }

    fn consumeToken(self: *Parser, token_type: TokenType) !void {
        const current_token = self.peek() orelse return ParseError.UnexpectedEndOfFile;
        if (current_token.token_type != token_type) {
            return ParseError.UnexpectedToken;
        }
        _ = self.advance() orelse return ParseError.UnexpectedEndOfFile;
    }

    /// Parses the entire program and returns the AST.
    pub fn parseProgram(self: *Parser) !Program {
        var items = std.ArrayList(Item).init(self.allocator);
        defer items.deinit();

        while (self.peek()) |token| {
            if (token.token_type == TokenType.EndOfFile) {
                break;
            }

            const item = try self.parseItem();
            try items.append(item);
        }

        return Program{
            .items = try items.toOwnedSlice(),
        };
    }

    /// Parses a top-level item (e.g., function).
    fn parseItem(self: *Parser) !Item {
        var docstring: ?[]const u8 = null;

        if (self.matchTokens(&[_]TokenType{TokenType.DocString})) {
            docstring = self.advance().?.lexeme;
        }

        const current_token = self.peek() orelse return ParseError.UnexpectedEndOfFile;

        if (current_token.token_type == TokenType.FN or current_token.token_type == TokenType.Unsafe) {
            return Item{
                .docstring = docstring,
                .content = .{ .Function = try self.parseFunction() },
            };
        } else {
            return ParseError.UnexpectedToken;
        }
    }

    /// Parses a function definition.
    fn parseFunction(self: *Parser) !Function {
        var is_unsafe = false;
        if (self.matchTokens(&[_]TokenType{TokenType.Unsafe})) {
            is_unsafe = true;
        }
        _ = try self.expectToken(TokenType.FN);

        const function_name = (try self.expectToken(TokenType.Identifier)).lexeme;

        _ = try self.expectToken(TokenType.LeftParen);
        const parameters = try self.parseParameterList();
        _ = try self.expectToken(TokenType.RightParen);

        var return_type: ?TypeAnnotation = null;
        if (self.matchTokens(&[_]TokenType{TokenType.Arrow})) {
            return_type = try self.parseTypeAnnotation();
        }

        const body = try self.parseBlock();

        return Function{
            .is_unsafe = is_unsafe,
            .fname = function_name,
            .generics = null,
            .parameters = parameters,
            .return_type = return_type,
            .body = body,
        };
    }

    /// Parses a block of code enclosed in braces.
    fn parseBlock(self: *Parser) !Block {
        _ = try self.expectToken(TokenType.LeftBrace);

        var statements = std.ArrayList(Statement).init(self.allocator);

        var block_expression: ?*const Expression = null;

        while (self.peek()) |token| {
            if (token.token_type == TokenType.RightBrace) {
                break;
            }

            if (self.parseExpression()) |exp| {
                block_expression = exp;
                break;
            } else |_| {
                const statement = try self.parseStatement();
                try statements.append(statement);
            }
        }

        _ = try self.expectToken(TokenType.RightBrace);

        return Block{
            .statements = try statements.toOwnedSlice(),
            .expression = block_expression,
        };
    }

    /// Determines if a token type can start an expression.
    fn isExpressionStart(token_type: TokenType) bool {
        return switch (token_type) {
            TokenType.Identifier, TokenType.Integer, TokenType.Float, TokenType.Boolean, TokenType.String, TokenType.Char, TokenType.LeftParen, TokenType.Minus, TokenType.Exclamation => true,
            else => false,
        };
    }

    /// Parses a single statement.
    fn parseStatement(self: *Parser) !Statement {
        const current_token = self.peek() orelse return ParseError.UnexpectedEndOfFile;

        if (current_token.token_type == TokenType.Let) {
            return Statement{ .LetStatement = try self.parseLetStatement() };
        } else {
            const expr = try self.parseExpression();
            try self.consumeToken(TokenType.DotComma);
            return Statement{ .Expression = expr };
        }
    }

    /// Parses a let statement.
    fn parseLetStatement(self: *Parser) !LetStatement {
        _ = try self.expectToken(TokenType.Let);
        const is_mutable = self.matchTokens(&[_]TokenType{TokenType.Mut});
        const pattern = (try self.expectToken(TokenType.Identifier)).lexeme;

        var type_annotation: ?TypeAnnotation = null;
        if (self.matchTokens(&[_]TokenType{TokenType.DoubleColon})) {
            type_annotation = try self.parseTypeAnnotation();
        }

        var value: ?*const Expression = null;
        if (self.matchTokens(&[_]TokenType{TokenType.Assign})) {
            value = try self.parseExpression();
        }

        return LetStatement{
            .is_mutable = is_mutable,
            .pattern = Pattern{ .Identifier = pattern },
            .type_annotation = type_annotation,
            .value = value,
        };
    }

    /// Parses a list of parameters within function definitions.
    fn parseParameterList(self: *Parser) ![]Parameter {
        var parameters = std.ArrayList(Parameter).init(self.allocator);
        defer parameters.deinit();

        if (self.peek()) |token| {
            if (token.token_type == TokenType.RightParen) {
                // Empty parameter list
                return parameters.toOwnedSlice();
            }
        }

        while (true) {
            const param = try self.parseParameter();
            try parameters.append(param);

            if (!self.matchTokens(&[_]TokenType{TokenType.Comma})) {
                break;
            }
        }

        return parameters.toOwnedSlice();
    }

    /// Parses a single parameter.
    fn parseParameter(self: *Parser) !Parameter {
        const name = (try self.expectToken(TokenType.Identifier)).lexeme;
        _ = try self.expectToken(TokenType.DoubleColon);
        const type_annotation = try self.parseTypeAnnotation();

        return Parameter{
            .name = name,
            .type_annotation = type_annotation,
        };
    }

    /// Parses a type annotation, handling references and mutability.
    fn parseTypeAnnotation(self: *Parser) !TypeAnnotation {
        var is_reference = false;
        var is_mutable = false;

        if (self.matchTokens(&[_]TokenType{TokenType.Ampersand})) {
            is_reference = true;
            if (self.matchTokens(&[_]TokenType{TokenType.Mut})) {
                is_mutable = true;
            }
        }

        const base_type = try self.parseBaseType();
        return TypeAnnotation{
            .is_reference = is_reference,
            .is_mutable = is_mutable,
            .base_type = base_type,
        };
    }

    /// Parses the base type in a type annotation.
    fn parseBaseType(self: *Parser) !*const BaseType {
        const token = self.peek() orelse return ParseError.UnexpectedEndOfFile;
        const base_type_ptr = try self.allocator.create(BaseType);

        switch (token.token_type) {
            TokenType.Type => {
                base_type_ptr.* = try getBaseType(token.lexeme);
                _ = self.advance();
            },
            TokenType.Identifier => {
                base_type_ptr.* = BaseType{ .Path = token.lexeme };
                _ = self.advance();
            },
            else => return ParseError.ExpectedType,
        }

        return base_type_ptr;
    }

    /// Maps a lexeme to a BaseType variant.
    fn getBaseType(lexeme: []const u8) !BaseType {
        if (std.mem.eql(u8, lexeme, "i32")) {
            return BaseType.I32;
        } else if (std.mem.eql(u8, lexeme, "i64")) {
            return BaseType.I64;
        } else if (std.mem.eql(u8, lexeme, "f32")) {
            return BaseType.F32;
        } else if (std.mem.eql(u8, lexeme, "f64")) {
            return BaseType.F64;
        } else if (std.mem.eql(u8, lexeme, "bool")) {
            return BaseType.Bool;
        } else if (std.mem.eql(u8, lexeme, "str")) {
            return BaseType.Str;
        }
        // TODO: Add more base types
        else {
            return BaseType{ .Path = lexeme };
        }
    }

    /// Parses an expression based on operator precedence.
    fn parseExpression(self: *Parser) !*const Expression {
        return try self.parseExpressionHelper(Precedence.Lowest);
    }

    /// Parses expressions with operator precedence.
    fn parseExpressionHelper(self: *Parser, min_prec: Precedence) anyerror!*const Expression {
        const token_type = self.peek().?.token_type;

        switch (token_type) {
            TokenType.If => {
                const expr_ptr = try self.allocator.create(Expression);
                expr_ptr.* = Expression{ .Conditional = try self.parseConditionalExpression() };
                return try self.parseBinaryOp(expr_ptr, min_prec);
            },
            TokenType.Loop, TokenType.While, TokenType.For => {
                const expr_ptr = try self.allocator.create(Expression);
                expr_ptr.* = Expression{ .Loop = try self.parseLoopExpression() };

                return try self.parseBinaryOp(expr_ptr, min_prec);
            },
            TokenType.Identifier, TokenType.Integer, TokenType.Float, TokenType.Boolean, TokenType.String, TokenType.Char, TokenType.LeftParen, TokenType.Minus, TokenType.Exclamation => {
                const expr_ptr = try self.parseUnaryOrPrimary();

                return try self.parseBinaryOp(expr_ptr, min_prec);
            },
            else => return ParseError.UnexpectedToken,
        }
    }

    /// Parses a unary operation or a primary expression.
    fn parseUnaryOrPrimary(self: *Parser) !*const Expression {
        const token = self.peek().?;
        return switch (token.token_type) {
            TokenType.Minus, TokenType.Exclamation => try self.parseUnaryOp(),
            else => try self.parsePrimary(),
        };
    }

    /// Parses any kind of loop expression (infinite, while, or for).
    fn parseLoopExpression(self: *Parser) !*const LoopExpression {
        const token = self.advance() orelse return ParseError.UnexpectedEndOfFile;

        const loop_expr_ptr = try self.allocator.create(LoopExpression);

        loop_expr_ptr.* = switch (token.token_type) {
            .Loop => LoopExpression{ .Infinite = Loop{ .body = try self.parseBlock() } },
            .While => LoopExpression{ .While = WhileLoop{
                .condition = try self.parseExpression(),
                .body = try self.parseBlock(),
            } },
            .For => LoopExpression{ .For = ForLoop{
                .iterator = try self.parsePattern(),
                .iterable = try self.parseExpression(),
                .body = try self.parseBlock(),
            } },
            else => return ParseError.UnexpectedToken,
        };
        return loop_expr_ptr;
    }

    fn parsePattern(_: *Parser) !Pattern {
        return ParseError.NotImplemented;
    }

    /// Parses a primary expression (e.g., literals, identifiers, parentheses).
    fn parsePrimary(self: *Parser) !*const Expression {
        const token = self.advance().?;

        if (token.token_type == TokenType.LeftParen) {
            const expr = try self.parseExpressionHelper(Precedence.Lowest);
            _ = try self.expectToken(TokenType.RightParen);
            return expr;
        }

        const expr_ptr = try self.allocator.create(Expression);

        expr_ptr.* = switch (token.token_type) {
            TokenType.Identifier => Expression{ .Identifier = token.lexeme },
            TokenType.Integer => Expression{ .Literal = Literal{ .Integer = try std.fmt.parseInt(i64, token.lexeme, 10) } },
            TokenType.Float => Expression{ .Literal = Literal{ .Float = try std.fmt.parseFloat(f64, token.lexeme) } },
            TokenType.Boolean => Expression{
                .Literal = Literal{ .Boolean = std.mem.eql(u8, token.lexeme, "true") },
            },
            TokenType.String => Expression{ .Literal = Literal{ .String = token.lexeme } },
            TokenType.Char => Expression{ .Literal = Literal{ .Char = token.lexeme[0] } },
            else => return error.UnexpectedToken,
        };

        return expr_ptr;
    }

    /// Parses a unary operation.
    fn parseUnaryOp(self: *Parser) !*const Expression {
        const token = self.advance() orelse return ParseError.UnexpectedEndOfFile;

        const operator = switch (token.token_type) {
            TokenType.Minus => UnaryOperator.Negate,
            TokenType.Exclamation => UnaryOperator.LogicalNot,
            else => return ParseError.UnexpectedToken,
        };

        const operand = try self.parseExpression();
        const unary_expr = try self.allocator.create(Expression);

        unary_expr.* = Expression{
            .UnaryOperation = UnaryOperation{
                .operator = operator,
                .operand = operand,
            },
        };

        return unary_expr;
    }

    /// Parses binary operations based on precedence.
    fn parseBinaryOp(self: *Parser, left: *const Expression, min_prec: Precedence) !*const Expression {
        var current_left = left;

        while (true) {
            const lookahead = self.peek() orelse return ParseError.UnexpectedEndOfFile;

            const prec = getPrecedence(lookahead.token_type);

            if (@intFromEnum(prec) < @intFromEnum(min_prec)) {
                break;
            }

            const operator_token = self.advance() orelse return ParseError.UnexpectedEndOfFile;
            const operator = switch (operator_token.token_type) {
                TokenType.LogicalOr => BinaryOperator.LogicalOr,
                TokenType.LogicalAnd => BinaryOperator.LogicalAnd,
                TokenType.Equal => BinaryOperator.Equal,
                TokenType.NotEqual => BinaryOperator.NotEqual,
                TokenType.LessThan => BinaryOperator.LessThan,
                TokenType.GreaterThan => BinaryOperator.GreaterThan,
                TokenType.LessThanOrEqual => BinaryOperator.LessThanOrEqual,
                TokenType.GreaterThanOrEqual => BinaryOperator.GreaterThanOrEqual,
                TokenType.Plus => BinaryOperator.Add,
                TokenType.Minus => BinaryOperator.Subtract,
                TokenType.Multiply => BinaryOperator.Multiply,
                TokenType.Divide => BinaryOperator.Divide,
                else => return ParseError.UnexpectedToken,
            };

            const next_min_prec: u4 = switch (operator) {
                BinaryOperator.LogicalOr, BinaryOperator.LogicalAnd, BinaryOperator.Equal, BinaryOperator.NotEqual, BinaryOperator.LessThan, BinaryOperator.GreaterThan, BinaryOperator.LessThanOrEqual, BinaryOperator.GreaterThanOrEqual => @intFromEnum(prec) + 1,
                BinaryOperator.Add, BinaryOperator.Subtract => @intFromEnum(prec) + 1,
                BinaryOperator.Multiply, BinaryOperator.Divide => @intFromEnum(prec) + 1,
            };

            const right_expr = try self.parseExpressionHelper(@enumFromInt(next_min_prec));

            const bin_op_exp = try self.allocator.create(Expression);
            bin_op_exp.* = Expression{ .BinaryOperation = BinaryOperation{
                .left = current_left,
                .operator = operator,
                .right = right_expr,
            } };

            current_left = bin_op_exp;
        }

        return current_left;
    }

    /// Defines operator precedence levels.
    const Precedence = enum(u4) {
        NotFound = 0,
        Lowest = 1,
        LogicalOr = 2, // ||
        LogicalAnd = 3, // &&
        Equality = 4, // ==, !=
        Comparison = 5, // <, >, <=, >=
        Term = 6, // +, -
        Factor = 7, // *, /
        Unary = 8, // !, -
        Call = 9, // function calls
        Primary = 10,
    };

    /// Determines the precedence of a given token type.
    fn getPrecedence(token_type: TokenType) Precedence {
        return switch (token_type) {
            TokenType.LogicalOr => Precedence.LogicalOr,
            TokenType.LogicalAnd => Precedence.LogicalAnd,
            TokenType.Equal, TokenType.NotEqual => Precedence.Equality,
            TokenType.LessThan, TokenType.GreaterThan, TokenType.LessThanOrEqual, TokenType.GreaterThanOrEqual => Precedence.Comparison,
            TokenType.Plus, TokenType.Minus => Precedence.Term,
            TokenType.Multiply, TokenType.Divide => Precedence.Factor,
            TokenType.LeftParen => Precedence.Call,
            else => @enumFromInt(0),
        };
    }

    /// Parses a conditional (if-else) expression.
    fn parseConditionalExpression(self: *Parser) !*const ConditionalExpression {
        _ = try self.expectToken(TokenType.If);
        const condition = try self.parseExpression();
        const then_branch = try self.parseBlock();
        var else_branch: ?Block = null;
        if (self.matchTokens(&[_]TokenType{TokenType.Else})) {
            else_branch = try self.parseBlock();
        }
        const cond_expr = try self.allocator.create(ConditionalExpression);
        cond_expr.* = ConditionalExpression{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
        };
        return cond_expr;
    }
};

/// Parses the tokens into an abstract syntax tree (AST) program.
pub fn parse(tokens: []Token, allocator: std.mem.Allocator) !Program {
    var parser = Parser.init(tokens, allocator);
    return parser.parseProgram();
}

// ===================== TESTS =====================

const testing = std.testing;

test "Parse a simple safe function" {
    const input =
        \\fn add(a: i32, b: i32) -> i32 {
        \\    a + b
        \\}
    ;

    const expected_ast = Program{ .items = &[_]Item{
        Item{
            .docstring = null,
            .content = ItemContent{
                .Function = Function{
                    .is_unsafe = false,
                    .fname = "add",
                    .generics = null,
                    .parameters = &[_]Parameter{
                        Parameter{
                            .name = "a",
                            .type_annotation = TypeAnnotation{
                                .is_reference = false,
                                .is_mutable = false,
                                .base_type = &.I32,
                            },
                        },
                        Parameter{
                            .name = "b",
                            .type_annotation = TypeAnnotation{
                                .is_reference = false,
                                .is_mutable = false,
                                .base_type = &.I32,
                            },
                        },
                    },
                    .return_type = TypeAnnotation{
                        .is_reference = false,
                        .is_mutable = false,
                        .base_type = &.I32,
                    },
                    .body = Block{
                        .statements = &[_]Statement{},
                        .expression = &Expression{
                            .BinaryOperation = BinaryOperation{
                                .left = &Expression{ .Identifier = "a" },
                                .operator = .Add,
                                .right = &Expression{ .Identifier = "b" },
                            },
                        },
                    },
                },
            },
        },
    } };

    try test_parser(input, expected_ast);
}

/// Helper function to run parser tests using Zig's testing framework.
fn test_parser(input: []const u8, expected_ast: Program) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init(input, allocator);
    const tokens = try lexer.tokenize();

    const actual_ast = try parse(tokens, allocator);

    try std.testing.expectEqual(actual_ast.items.len, expected_ast.items.len);

    for (actual_ast.items, expected_ast.items) |actual_item, expected_item| {
        try compareItem(actual_item, expected_item);
    }

    // Redundancy tests
    var actual_buffer = std.ArrayList(u8).init(allocator);
    defer actual_buffer.deinit();
    const actual_writer = actual_buffer.writer();

    var expected_buffer = std.ArrayList(u8).init(allocator);
    defer expected_buffer.deinit();
    const expected_writer = expected_buffer.writer();

    try serialize(actual_writer, actual_ast);
    try serialize(expected_writer, expected_ast);

    const actual_json = actual_buffer.items;
    const expected_json = expected_buffer.items;

    try std.testing.expectEqualSlices(u8, actual_json, expected_json);
}

/// Compares two `Item` instances for equality.
fn compareItem(actual: Item, expected: Item) TestParseError!void {
    // if any docstring is present, they must be equal
    if (actual.docstring != null or expected.docstring != null) {
        try std.testing.expect(std.mem.eql(u8, actual.docstring.?, expected.docstring.?));
    }

    // content must be the same
    // Dont compare Enums directly since it recursively compares everything, including pointers
    try std.testing.expectEqual(@tagName(actual.content), @tagName(expected.content));

    // Compare content
    switch (actual.content) {
        .Function => {
            try compareFunction(actual.content.Function, expected.content.Function);
        },
    }
}

/// Compares two `Function` instances for equality.
fn compareFunction(actual: Function, expected: Function) !void {
    try std.testing.expectEqual(actual.is_unsafe, expected.is_unsafe);
    try std.testing.expect(std.mem.eql(u8, actual.fname, expected.fname));
    // TODO: compare generics
    try compareParameters(actual.parameters, expected.parameters);
    try compareTypeAnnotation(actual.return_type, expected.return_type);
    try compareBlock(actual.body, expected.body);
}

/// Compares two lists of parameters for equality.
fn compareParameters(actual_params: []const Parameter, expected_params: []const Parameter) !void {
    try std.testing.expectEqual(actual_params.len, expected_params.len);
    for (actual_params, expected_params) |actual, expected| {
        try std.testing.expect(std.mem.eql(u8, actual.name, expected.name));
        try compareTypeAnnotation(actual.type_annotation, expected.type_annotation);
    }
}

// is_reference: bool,
// is_mutable: bool,
// base_type: *const BaseType,
/// Compares two `TypeAnnotation` instances for equality.
fn compareTypeAnnotation(actual: ?TypeAnnotation, expected: ?TypeAnnotation) !void {

    // if both are null, they are equal
    if (actual == null and expected == null) {
        return;
    }

    try std.testing.expectEqual(actual.?.is_reference, expected.?.is_reference);
    try std.testing.expectEqual(actual.?.is_mutable, expected.?.is_mutable);
    try compareBaseType(actual.?.base_type.*, expected.?.base_type.*);
}

/// Compares two `BaseType` instances for equality.
fn compareBaseType(actual: BaseType, expected: BaseType) !void {
    try std.testing.expectEqual(actual, expected);
}

/// Compares two `Block` instances for equality.
fn compareBlock(actual_block: Block, expected_block: Block) !void {
    try std.testing.expectEqual(actual_block.statements.len, expected_block.statements.len);

    for (actual_block.statements, expected_block.statements) |actual, expected| {
        try compareStatement(actual, expected);
    }

    try compareExpression(actual_block.expression.?, actual_block.expression.?);
}

/// Compares two `Statement` instances for equality.
fn compareStatement(actual: Statement, expected: Statement) !void {
    try std.testing.expectEqual(actual, expected);

    switch (actual) {
        .LetStatement => {
            try std.testing.expectEqual(actual.LetStatement.is_mutable, expected.LetStatement.is_mutable);
            try comparePattern(actual.LetStatement.pattern, expected.LetStatement.pattern);
            try compareTypeAnnotation(actual.LetStatement.type_annotation, expected.LetStatement.type_annotation);
            try compareExpression(actual.LetStatement.value.?, expected.LetStatement.value.?);
        },
        .Expression => {
            try compareExpression(actual.Expression, expected.Expression);
        },
        .Item => {
            try compareItem(actual.Item.*, expected.Item.*);
        },
    }
}

/// Compares two `Pattern` instances for equality.
fn comparePattern(actual: Pattern, expected: Pattern) !void {
    try std.testing.expectEqual(actual, expected);

    switch (actual) {
        .Identifier => try std.testing.expect(std.mem.eql(u8, actual.Identifier, expected.Identifier)),
        .Wildcard => return ParseError.NotImplemented,
        .Tuple => return ParseError.NotImplemented,
        .Struct => return ParseError.NotImplemented,
    }
}

/// Compares two `Expression` instances for equality.
fn compareExpression(actual: *const Expression, expected: *const Expression) TestParseError!void {
    try std.testing.expectEqual(actual, expected);

    switch (actual.*) {
        .Identifier => try compareIdentifier(actual.Identifier, expected.Identifier),
        .Literal => try compareLiteral(actual.Literal, expected.Literal),

        .BinaryOperation => try compareBinaryOperation(actual.BinaryOperation, expected.BinaryOperation),
        .UnaryOperation => try compareUnaryOperation(actual.UnaryOperation, expected.UnaryOperation),
        .FunctionCall => try compareFunctionCall(actual.FunctionCall, expected.FunctionCall),
        else => return ParseError.InvalidExpression,
    }
}

/// Compare 2 identifiers
fn compareIdentifier(actual: []const u8, expected: []const u8) !void {
    try std.testing.expect(std.mem.eql(u8, actual, expected));
}

/// Compare 2 literals
fn compareLiteral(actual: Literal, expected: Literal) !void {

    // same enum variant
    try std.testing.expectEqual(actual, expected);

    switch (actual) {
        .Integer => try std.testing.expectEqual(actual.Integer, expected.Integer),
        .Float => try std.testing.expectEqual(actual.Float, expected.Float),
        .Boolean => try std.testing.expectEqual(actual.Boolean, expected.Boolean),
        .String => try std.testing.expect(std.mem.eql(u8, actual.String, expected.String)),
        .Char => try std.testing.expectEqual(actual.Char, expected.Char),
    }
}

/// Compare 2 binary operations
fn compareBinaryOperation(actual: BinaryOperation, expected: BinaryOperation) !void {
    try std.testing.expectEqual(actual.operator, expected.operator);
    try compareExpression(actual.left, expected.left);
    try compareExpression(actual.right, expected.right);
}

/// Compare 2 unary operations
fn compareUnaryOperation(actual: UnaryOperation, expected: UnaryOperation) !void {
    try std.testing.expectEqual(actual.operator, expected.operator);
    try compareExpression(actual.operand, expected.operand);
}

/// Compare 2 function calls
fn compareFunctionCall(actual: FunctionCall, expected: FunctionCall) !void {
    try std.testing.expect(std.mem.eql(u8, actual.function_name, expected.function_name));
    try std.testing.expectEqual(actual.arguments.len, expected.arguments.len);
    for (actual.arguments, expected.arguments) |actual_arg, expected_arg| {
        try compareExpression(actual_arg, expected_arg);
    }
}
