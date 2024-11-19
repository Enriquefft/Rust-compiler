const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const compareToken = @import("lexer.zig").compareToken;
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
} || std.mem.Allocator.Error ||
    std.fmt.ParseIntError || std.fmt.ParseFloatError;

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
pub const Statement = union(enum) { LetStatement: LetStatement, Expression: *const Expression, Item: *const Item, MacroInvocationSemi: MacroInvocationSemi };

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
    Range: RangeExpression,
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

pub const RangeExpression = struct {
    start: *const Expression,
    end: *const Expression,
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

pub const MacroInvocationSemi = struct {
    macro_invocation: MacroInvocation,
};

pub const MacroInvocation = struct {
    simple_path: SimplePath,
    delim: DelimTokenTree,
};

pub const DelimTokenTree = union(enum) {
    Parentheses: []const TokenTree,
    Brackets: []const TokenTree,
    Braces: []const TokenTree,
};

pub const TokenTree = union(enum) {
    Token: Token,
    DelimTokenTree: *const DelimTokenTree,
};

pub const SimplePath = struct {
    is_absolute: bool, // starts with ::
    path_segments: []const []const u8,
};

/// Parser structure that maintains parsing state and methods.
pub const Parser = struct {
    tokens: []const Token,
    current: usize, // Index of the current token
    allocator: std.mem.Allocator,

    freeze_stack: std.ArrayList(usize),

    /// Initializes a new Parser.
    pub fn init(tokens: []const Token, allocator: std.mem.Allocator) Parser {

        // populate the base type pointers

        return Parser{
            .tokens = tokens,
            .current = 0,
            .allocator = allocator,
            .freeze_stack = std.ArrayList(usize).init(allocator),
        };
    }

    /// Stores the current state of the parser.
    fn freeze(self: *Parser) !void {
        try self.freeze_stack.append(self.current);
    }

    /// Removes the last saved state
    fn pop(self: *Parser) !void {
        _ = self.freeze_stack.pop();
    }

    /// Restores the last state of the parser.
    fn restore(self: *Parser) !void {
        const last = self.freeze_stack.pop();
        self.current = last;
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

    /// plurale
    fn expectTokens(self: *Parser, token_types: []const TokenType) !Token {
        const current_token = self.peek() orelse return ParseError.UnexpectedEndOfFile;

        var is_valid = false;

        for (token_types) |valid_token_type| {
            if (valid_token_type == current_token.token_type) {
                is_valid = true;
            }
        }

        if (!is_valid) {
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
    fn parseItem(self: *Parser) ParseError!Item {
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
    fn parseFunction(self: *Parser) ParseError!Function {
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
    fn parseBlock(self: *Parser) ParseError!Block {
        try self.consumeToken(TokenType.LeftBrace);

        var statements = std.ArrayList(Statement).init(self.allocator);
        var block_expression: ?*const Expression = null;

        while (self.peek().?.token_type != TokenType.RightBrace) {
            // Check for empty statement (just a semicolon)
            if (self.peek().?.token_type == TokenType.DotComma) {
                try self.consumeToken(TokenType.DotComma);
                continue;
            }

            // Attempt to parse a statement
            try self.freeze();
            if (self.parseStatement()) |stm| {
                try self.pop();
                try statements.append(stm);
                continue;
            } else |_| {
                try self.restore();
            }

            // If not a statement, attempt to parse the final expression
            if (block_expression == null) {
                block_expression = try self.parseExpression();
                // After parsing the expression, expect a RightBrace or semicolon
                if (self.peek().?.token_type == TokenType.DotComma) {
                    try self.consumeToken(TokenType.DotComma);

                    // If there's a semicolon after the expression, it's treated as a statement
                    try statements.append(Statement{ .Expression = block_expression.? });

                    block_expression = null;
                }
                break;
            } else {
                // Multiple expressions without semicolons are not allowed
                return ParseError.InvalidExpression;
            }
        }

        try self.consumeToken(TokenType.RightBrace);

        return Block{
            .statements = try statements.toOwnedSlice(),
            .expression = block_expression,
        };
    }

    /// Parses a single statement.
    fn parseStatement(self: *Parser) !Statement {
        const current_token = self.peek() orelse return ParseError.UnexpectedEndOfFile;

        if (current_token.token_type == TokenType.Let) {
            return Statement{ .LetStatement = try self.parseLetStatement() };
        }

        // try to parse macro_invocation

        try self.freeze();
        if (self.parseMacroInvocationSemi()) |macro_invocation| {
            try self.pop();

            return Statement{ .MacroInvocationSemi = macro_invocation };
        } else |_| {
            try self.restore();
        }

        // try to parse expression

        try self.freeze();
        if (self.parseExpression()) |expr| {
            try self.pop();
            try self.consumeToken(TokenType.DotComma);
            return Statement{ .Expression = expr };
        } else |_| {
            try self.restore();
        }

        // try to parse item

        const item_ptr = try self.allocator.create(Item);
        item_ptr.* = try self.parseItem();
        return Statement{ .Item = item_ptr };
    }

    /// Parses a macro invocation that ends with a semicolon. (statement)
    fn parseMacroInvocationSemi(self: *Parser) !MacroInvocationSemi {
        const macro_invocation = try self.parseMacroInvocation();

        // if (macro_invocation.delim.Brackets or macro_invocation.delim.Parentheses ){
        //     try self.consumeToken(TokenType.DotComma);
        // }

        switch (macro_invocation.delim) {
            .Brackets, .Parentheses => try self.consumeToken(TokenType.DotComma),
            else => {},
        }

        return MacroInvocationSemi{
            .macro_invocation = macro_invocation,
        };
    }

    /// Parses a macro invocation
    fn parseMacroInvocation(self: *Parser) !MacroInvocation {
        const simple_path = try self.parseSimplePath();

        try self.consumeToken(TokenType.Exclamation);

        const delim_token_tree = try self.parseDelimTokenTree();

        return MacroInvocation{
            .simple_path = simple_path,
            .delim = delim_token_tree,
        };
    }

    /// Parses a delimited token tree
    fn parseDelimTokenTree(self: *Parser) ParseError!DelimTokenTree {
        const delim = try self.expectTokens(&[_]TokenType{ TokenType.LeftParen, TokenType.LeftBrace, TokenType.LeftBracket });

        var token_trees = std.ArrayList(TokenTree).init(self.allocator);

        while (true) {
            const next_tree = try self.parseTokenTree();

            if (next_tree == null) {
                break;
            } else {
                try token_trees.append(next_tree.?);
            }
        }

        switch (delim.token_type) {
            .LeftParen => {
                try self.consumeToken(TokenType.RightParen);
                return DelimTokenTree{ .Parentheses = try token_trees.toOwnedSlice() };
            },
            .LeftBrace => {
                try self.consumeToken(TokenType.RightBrace);
                return DelimTokenTree{ .Braces = try token_trees.toOwnedSlice() };
            },
            .LeftBracket => {
                try self.consumeToken(TokenType.RightBracket);
                return DelimTokenTree{ .Brackets = try token_trees.toOwnedSlice() };
            },
            else => return ParseError.UnexpectedToken,
        }
    }

    /// Parses a token tree
    fn parseTokenTree(self: *Parser) !?TokenTree {
        if (self.matchTokens(&[_]TokenType{ TokenType.LeftParen, TokenType.LeftBrace, TokenType.LeftBracket })) {
            const delim_token_tree_ptr = try self.allocator.create(DelimTokenTree);
            delim_token_tree_ptr.* = try self.parseDelimTokenTree();

            return TokenTree{ .DelimTokenTree = delim_token_tree_ptr };
        }

        if (self.matchTokens(&[_]TokenType{ TokenType.RightParen, TokenType.RightBrace, TokenType.RightBracket })) {
            self.current -= 1;
            return null;
        }

        return TokenTree{ .Token = self.advance().? };
    }

    fn parseSimplePath(self: *Parser) !SimplePath {
        var is_absolute = false;
        if (self.matchToken(TokenType.PathSep)) {
            is_absolute = true;
        }

        var segments = std.ArrayList([]const u8).init(self.allocator);

        const first_segment = try self.parsePathSegment();
        try segments.append(first_segment);

        while (self.matchToken(TokenType.PathSep)) {
            const segment = try self.parsePathSegment();
            try segments.append(segment);
        }

        return SimplePath{
            .is_absolute = is_absolute,
            .path_segments = try segments.toOwnedSlice(),
        };
    }

    // Parses a path segment, used in simple & expression paths.
    fn parsePathSegment(self: *Parser) ![]const u8 {
        const current_token = try self.expectTokens(&[_]TokenType{ TokenType.Identifier, TokenType.Super, TokenType.Self, TokenType.Crate, TokenType.Dollar });

        return switch (current_token.token_type) {
            TokenType.Identifier => current_token.lexeme,
            TokenType.Super => "super",
            TokenType.Self => "self",
            TokenType.Crate => "crate",
            TokenType.Dollar => {
                try self.consumeToken(TokenType.Crate);
                return "$crate";
            },
            else => return ParseError.UnexpectedToken,
        };
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

        try self.consumeToken(TokenType.DotComma);

        return LetStatement{
            .is_mutable = is_mutable,
            .pattern = Pattern{ .Identifier = pattern },
            .type_annotation = type_annotation,
            .value = value,
        };
    }

    /// Parses a list of parameters within function definitions.
    fn parseParameterList(self: *Parser) ParseError![]Parameter {
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
    fn parseExpression(self: *Parser) ParseError!*const Expression {
        return try self.parseExpressionHelper(Precedence.Lowest);
    }

    /// Parses expressions with operator precedence.
    fn parseExpressionHelper(self: *Parser, min_prec: Precedence) ParseError!*const Expression {
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

                // Assignment expression
                if (self.matchToken(TokenType.Assign)) {
                    const assignment_expr = try self.allocator.create(Expression);
                    const assignment_ptr = try self.allocator.create(AssignmentExpression);

                    assignment_ptr.* = AssignmentExpression{
                        .identifier = expr_ptr.Identifier,
                        .value = try self.parseExpression(),
                    };

                    assignment_expr.* = Expression{ .Assignment = assignment_ptr
                    };
                    return assignment_expr;
                }

                return try self.parseBinaryOp(expr_ptr, min_prec);
            },
            else => return ParseError.UnexpectedToken,
        }
    }

    /// Parses a unary operation or a primary expression.
    fn parseUnaryOrPrimary(self: *Parser) ParseError!*const Expression {
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
    fn parsePrimary(self: *Parser) ParseError!*const Expression {
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
pub fn parse(tokens: []const Token, allocator: std.mem.Allocator) !Program {
    var parser = Parser.init(tokens, allocator);
    return parser.parseProgram();
}

// ===================== TESTS =====================

const testing = std.testing;

test "Parse a simple safe function declaration" {
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

test "Parse the main function with mutable variables and println statements" {
    const input =
        \\fn main() {
        \\    let mut x: i32;
        \\    let mut y: i32;
        \\    let mut z: i64;
        \\
        \\    x = 1;
        \\    y = 10;
        \\    z = 1000000;
        \\    x = 20;
        \\
        \\    println!("{}", x);
        \\    println!("{}", y);
        \\    println!("{}", z);
        \\}
    ;

    const expected_ast = Program{
        .items = &[_]Item{
            Item{
                .docstring = null,
                .content = ItemContent{
                    .Function = Function{
                        .is_unsafe = false,
                        .fname = "main",
                        .generics = null,
                        .parameters = &[_]Parameter{},
                        .return_type = null,
                        .body = Block{
                            .statements = &[_]Statement{
                                // let mut x: i32;
                                Statement{
                                    .LetStatement = LetStatement{
                                        .is_mutable = true,
                                        .pattern = Pattern{
                                            .Identifier = "x",
                                        },
                                        .type_annotation = TypeAnnotation{
                                            .is_reference = false,
                                            .is_mutable = false,
                                            .base_type = &.I32,
                                        },
                                        .value = null,
                                    },
                                },
                                // let mut y: i32;
                                Statement{
                                    .LetStatement = LetStatement{
                                        .is_mutable = true,
                                        .pattern = Pattern{
                                            .Identifier = "y",
                                        },
                                        .type_annotation = TypeAnnotation{
                                            .is_reference = false,
                                            .is_mutable = false,
                                            .base_type = &.I32,
                                        },
                                        .value = null,
                                    },
                                },
                                // let mut z: i64;
                                Statement{
                                    .LetStatement = LetStatement{
                                        .is_mutable = true,
                                        .pattern = Pattern{
                                            .Identifier = "z",
                                        },
                                        .type_annotation = TypeAnnotation{
                                            .is_reference = false,
                                            .is_mutable = false,
                                            .base_type = &.I64,
                                        },
                                        .value = null,
                                    },
                                },
                                // x = 1;
                                Statement{
                                    .Expression = &Expression{
                                        .Assignment = &AssignmentExpression{
                                            .identifier = "x",
                                            .value = &Expression{
                                                .Literal = Literal{
                                                    .Integer = 1,
                                                },
                                            },
                                        },
                                    },
                                },
                                // y = 10;
                                Statement{
                                    .Expression = &Expression{
                                        .Assignment = &AssignmentExpression{
                                            .identifier = "y",
                                            .value = &Expression{
                                                .Literal = Literal{
                                                    .Integer = 10,
                                                },
                                            },
                                        },
                                    },
                                },
                                // z = 1000000;
                                Statement{
                                    .Expression = &Expression{
                                        .Assignment = &AssignmentExpression{
                                            .identifier = "z",
                                            .value = &Expression{
                                                .Literal = Literal{
                                                    .Integer = 1000000,
                                                },
                                            },
                                        },
                                    },
                                },
                                // x = 20;
                                Statement{
                                    .Expression = &Expression{
                                        .Assignment = &AssignmentExpression{
                                            .identifier = "x",
                                            .value = &Expression{
                                                .Literal = Literal{
                                                    .Integer = 20,
                                                },
                                            },
                                        },
                                    },
                                },
                                // println!("{}", x);
                                Statement{

                                    .MacroInvocationSemi = MacroInvocationSemi{
                                        .macro_invocation = MacroInvocation{
                                            .simple_path = SimplePath{
                                                .is_absolute = false,
                                                .path_segments = &[_][]const u8{
                                                    "println",
                                                },
                                            },
                                            .delim = DelimTokenTree{
                                                .Parentheses = &[_]TokenTree{
                                                    TokenTree{
                                                    .Token = Token{
                                                        .token_type = TokenType.String,
                                                        .lexeme = "{}",
                                                    },

                                                    },
                                                    TokenTree{
                                                    .Token = Token{
                                                        .token_type = TokenType.Comma,
                                                    },

                                                    },
                                                    TokenTree{
                                                    .Token = Token{
                                                        .token_type = TokenType.Identifier,
                                                        .lexeme = "x",
                                                    },
                                                    }
                                                },
                                            },
                                        },
                                    },

                                },
                                // println!("{}", y);
                                Statement{
                                    .MacroInvocationSemi = MacroInvocationSemi{
                                        .macro_invocation = MacroInvocation{
                                            .simple_path = SimplePath{
                                                .is_absolute = false,
                                                .path_segments = &[_][] const u8{
                                                    "println",
                                                },
                                            },
                                            .delim = DelimTokenTree{
                                                .Parentheses = &[_]TokenTree{
                                                    TokenTree{
                                                    .Token = Token{
                                                        .token_type = TokenType.String,
                                                        .lexeme = "{}",
                                                    },

                                                    },
                                                    TokenTree{
                                                    .Token = Token{
                                                        .token_type = TokenType.Comma,
                                                    },

                                                    },
                                                    TokenTree{
                                                    .Token = Token{
                                                        .token_type = TokenType.Identifier,
                                                        .lexeme = "y",
                                                    },
                                                    }
                                                },
                                            },
                                        },
                                    },
                                },
                                // println!("{}", z);
                                Statement{
                                    .MacroInvocationSemi = MacroInvocationSemi{
                                        .macro_invocation = MacroInvocation{
                                            .simple_path = SimplePath{
                                                .is_absolute = false,
                                                .path_segments = &[_][] const u8{
                                                    "println",
                                                },
                                            },
                                            .delim = DelimTokenTree{
                                                .Parentheses = &[_]TokenTree{
                                                    TokenTree{
                                                    .Token = Token{
                                                        .token_type = TokenType.String,
                                                        .lexeme = "{}",
                                                    },

                                                    },
                                                    TokenTree{
                                                    .Token = Token{
                                                        .token_type = TokenType.Comma,
                                                    },

                                                    },
                                                    TokenTree{
                                                    .Token = Token{
                                                        .token_type = TokenType.Identifier,
                                                        .lexeme = "z",
                                                    },
                                                    }
                                                },
                                            },
                                        },
                                    },
                                },
                            },
                            .expression = null,
                        },
                    },
                },
            },
        },
    };

    try test_parser(input, expected_ast);
} //
// test "Parse the main function with mutable variables and if-else statements" {
//     const input =
//         \\fn main() {
//         \\    let mut x: i32;
//         \\    let mut y: i32;
//         \\
//         \\    x = 5;
//         \\    y = 10;
//         \\
//         \\    if x > y {
//         \\        println!("{}", x);
//         \\    } else {
//         \\        println!("{}", y);
//         \\    }
//         \\}
//     ;
//
//     const expected_ast = Program{
//         .items = &[_]Item{
//             Item{
//                 .docstring = null,
//                 .content = ItemContent{
//                     .Function = Function{
//                         .is_unsafe = false,
//                         .fname = "main",
//                         .generics = null,
//                         .parameters = &[_]Parameter{},
//                         .return_type = null,
//                         .body = Block{
//                             .statements = &[_]Statement{
//                                 // let mut x: i32;
//                                 Statement{
//                                     .LetStatement = LetStatement{
//                                         .is_mutable = true,
//                                         .pattern = Pattern{
//                                             .Identifier = "x",
//                                         },
//                                         .type_annotation = TypeAnnotation{
//                                             .is_reference = false,
//                                             .is_mutable = false,
//                                             .base_type = &.I32,
//                                         },
//                                         .value = null,
//                                     },
//                                 },
//                                 // let mut y: i32;
//                                 Statement{
//                                     .LetStatement = LetStatement{
//                                         .is_mutable = true,
//                                         .pattern = Pattern{
//                                             .Identifier = "y",
//                                         },
//                                         .type_annotation = TypeAnnotation{
//                                             .is_reference = false,
//                                             .is_mutable = false,
//                                             .base_type = &.I32,
//                                         },
//                                         .value = null,
//                                     },
//                                 },
//                                 // x = 5;
//                                 Statement{
//                                     .Expression = &Expression{
//                                         .Assignment = &AssignmentExpression{
//                                             .identifier = "x",
//                                             .value = &Expression{
//                                                 .Literal = Literal{
//                                                     .Integer = 5,
//                                                 },
//                                             },
//                                         },
//                                     },
//                                 },
//                                 // y = 10;
//                                 Statement{
//                                     .Expression = &Expression{
//                                         .Assignment = &AssignmentExpression{
//                                             .identifier = "y",
//                                             .value = &Expression{
//                                                 .Literal = Literal{
//                                                     .Integer = 10,
//                                                 },
//                                             },
//                                         },
//                                     },
//                                 },
//                                 // if x > y { println!("{}", x); } else { println!("{}", y); }
//                                 Statement{
//                                     .Expression = &Expression{
//                                         .Conditional = &ConditionalExpression{
//                                             .condition = &Expression{
//                                                 .BinaryOperation = BinaryOperation{
//                                                     .left = &Expression{ .Identifier = "x" },
//                                                     .operator = .GreaterThan,
//                                                     .right = &Expression{ .Identifier = "y" },
//                                                 },
//                                             },
//                                             .then_branch = Block{
//                                                 .statements = &[_]Statement{
//                                                     // println!("{}", x);
//                                                     Statement{
//                                                         .Expression = &Expression{
//                                                             .FunctionCall = FunctionCall{
//                                                                 .function_name = "println",
//                                                                 .arguments = &[_]*const Expression{
//                                                                     &Expression{
//                                                                         .Literal = Literal{
//                                                                             .String = "{}",
//                                                                         },
//                                                                     },
//                                                                     &Expression{
//                                                                         .Identifier = "x",
//                                                                     },
//                                                                 },
//                                                             },
//                                                         },
//                                                     },
//                                                 },
//                                                 .expression = null,
//                                             },
//                                             .else_branch = Block{
//                                                 .statements = &[_]Statement{
//                                                     // println!("{}", y);
//                                                     Statement{
//                                                         .Expression = &Expression{
//                                                             .FunctionCall = FunctionCall{
//                                                                 .function_name = "println",
//                                                                 .arguments = &[_]*const Expression{
//                                                                     &Expression{
//                                                                         .Literal = Literal{
//                                                                             .String = "{}",
//                                                                         },
//                                                                     },
//                                                                     &Expression{
//                                                                         .Identifier = "y",
//                                                                     },
//                                                                 },
//                                                             },
//                                                         },
//                                                     },
//                                                 },
//                                                 .expression = null,
//                                             },
//                                         },
//                                     },
//                                 },
//                             },
//                             .expression = null,
//                         },
//                     },
//                 },
//             },
//         },
//     };
//
//     try test_parser(input, expected_ast);
// }
//
//
// test "Parse the main function with a for loop and addition" {
//     const input =
//         \\fn main() {
//         \\    let mut x: i32;
//         \\
//         \\    x = 1;
//         \\
//         \\    for i in 0..10 { // i is a i32
//         \\        x += i;
//         \\    }
//         \\
//         \\    println!("{}", x);
//         \\}
//     ;
//
//     const expected_ast = Program{
//         .items = &[_]Item{
//             Item{
//                 .docstring = null,
//                 .content = ItemContent{
//                     .Function = Function{
//                         .is_unsafe = false,
//                         .fname = "main",
//                         .generics = null,
//                         .parameters = &[_]Parameter{},
//                         .return_type = null,
//                         .body = Block{
//                             .statements = &[_]Statement{
//                                 // let mut x: i32;
//                                 Statement{
//                                     .LetStatement = LetStatement{
//                                         .is_mutable = true,
//                                         .pattern = Pattern{
//                                             .Identifier = "x",
//                                         },
//                                         .type_annotation = TypeAnnotation{
//                                             .is_reference = false,
//                                             .is_mutable = false,
//                                             .base_type = &.I32,
//                                         },
//                                         .value = null,
//                                     },
//                                 },
//                                 // x = 1;
//                                 Statement{
//                                     .Expression = &Expression{
//                                         .Assignment = &AssignmentExpression{
//                                             .identifier = "x",
//                                             .value = &Expression{
//                                                 .Literal = Literal{
//                                                     .Integer = 1,
//                                                 },
//                                             },
//                                         },
//                                     },
//                                 },
//                                 // for i in 0..10 { x += i; }
//                                 Statement{
//                                     .Expression = &Expression{
//                                         .Loop = &LoopExpression{
//                                             .For = ForLoop{
//                                                 .iterator = Pattern{
//                                                     .Identifier = "i",
//                                                 },
//                                                 .iterable = &Expression{
//                                                     .Range = RangeExpression{
//                                                         .start = &Expression{
//                                                             .Literal = Literal{
//                                                                 .Integer = 0,
//                                                             },
//                                                         },
//                                                         .end = &Expression{
//                                                             .Literal = Literal{
//                                                                 .Integer = 10,
//                                                             },
//                                                         },
//                                                     },
//                                                 },
//                                                 .body = Block{
//                                                     .statements = &[_]Statement{
//                                                         // x += i;
//                                                         Statement{
//                                                             .Expression = &Expression{
//                                                                 .Assignment = &AssignmentExpression{
//                                                                     .identifier = "x",
//                                                                     .value = &Expression{
//                                                                         .BinaryOperation = BinaryOperation{
//                                                                             .left = &Expression{
//                                                                                 .Identifier = "x",
//                                                                             },
//                                                                             .operator = .Add,
//                                                                             .right = &Expression{
//                                                                                 .Identifier = "i",
//                                                                             },
//                                                                         },
//                                                                     },
//                                                                 },
//                                                             },
//                                                         },
//                                                     },
//                                                     .expression = null,
//                                                 },
//                                             },
//                                         },
//                                     },
//                                 },
//                                 // println!("{}", x);
//                                 Statement{
//                                     .Expression = &Expression{
//                                         .FunctionCall = FunctionCall{
//                                             .function_name = "println",
//                                             .arguments = &[_]*const Expression{
//                                                 &Expression{
//                                                     .Literal = Literal{
//                                                         .String = "{}",
//                                                     },
//                                                 },
//                                                 &Expression{
//                                                     .Identifier = "x",
//                                                 },
//                                             },
//                                         },
//                                     },
//                                 },
//                             },
//                             .expression = null,
//                         },
//                     },
//                 },
//             },
//         },
//     };
//
//     try test_parser(input, expected_ast);
// }
//
//
// test "Parse multiple functions with function calls" {
//     const input =
//         \\fn suma(a: i32, b: i32) -> i32 {
//         \\    a + b
//         \\}
//         \\
//         \\fn main() {
//         \\    let mut x: i32;
//         \\    let mut y: i32;
//         \\    x = 1;
//         \\    y = 20;
//         \\    println!("{}", suma(x, y));
//         \\}
//     ;
//
//     const expected_ast = Program{
//         .items = &[_]Item{
//             // Item 1: fn suma(a: i32, b: i32) -> i32 { a + b }
//             Item{
//                 .docstring = null,
//                 .content = ItemContent{
//                     .Function = Function{
//                         .is_unsafe = false,
//                         .fname = "suma",
//                         .generics = null,
//                         .parameters = &[_]Parameter{
//                             // Parameter 1: a: i32
//                             Parameter{
//                                 .name = "a",
//                                 .type_annotation = TypeAnnotation{
//                                     .is_reference = false,
//                                     .is_mutable = false,
//                                     .base_type = &.I32,
//                                 },
//                             },
//                             // Parameter 2: b: i32
//                             Parameter{
//                                 .name = "b",
//                                 .type_annotation = TypeAnnotation{
//                                     .is_reference = false,
//                                     .is_mutable = false,
//                                     .base_type = &.I32,
//                                 },
//                             },
//                         },
//                         .return_type = TypeAnnotation{
//                             .is_reference = false,
//                             .is_mutable = false,
//                             .base_type = &.I32,
//                         },
//                         .body = Block{
//                             .statements = &[_]Statement{},
//                             .expression = &Expression{
//                                 .BinaryOperation = BinaryOperation{
//                                     .left = &Expression{ .Identifier = "a" },
//                                     .operator = .Add,
//                                     .right = &Expression{ .Identifier = "b" },
//                                 },
//                             },
//                         },
//                     },
//                 },
//             },
//             // Item 2: fn main() { ... }
//             Item{
//                 .docstring = null,
//                 .content = ItemContent{
//                     .Function = Function{
//                         .is_unsafe = false,
//                         .fname = "main",
//                         .generics = null,
//                         .parameters = &[_]Parameter{},
//                         .return_type = null,
//                         .body = Block{
//                             .statements = &[_]Statement{
//                                 // let mut x: i32;
//                                 Statement{
//                                     .LetStatement = LetStatement{
//                                         .is_mutable = true,
//                                         .pattern = Pattern{
//                                             .Identifier = "x",
//                                         },
//                                         .type_annotation = TypeAnnotation{
//                                             .is_reference = false,
//                                             .is_mutable = false,
//                                             .base_type = &.I32,
//                                         },
//                                         .value = null,
//                                     },
//                                 },
//                                 // let mut y: i32;
//                                 Statement{
//                                     .LetStatement = LetStatement{
//                                         .is_mutable = true,
//                                         .pattern = Pattern{
//                                             .Identifier = "y",
//                                         },
//                                         .type_annotation = TypeAnnotation{
//                                             .is_reference = false,
//                                             .is_mutable = false,
//                                             .base_type = &.I32,
//                                         },
//                                         .value = null,
//                                     },
//                                 },
//                                 // x = 1;
//                                 Statement{
//                                     .Expression = &Expression{
//                                         .Assignment = &AssignmentExpression{
//                                             .identifier = "x",
//                                             .value = &Expression{
//                                                 .Literal = Literal{
//                                                     .Integer = 1,
//                                                 },
//                                             },
//                                         },
//                                     },
//                                 },
//                                 // y = 20;
//                                 Statement{
//                                     .Expression = &Expression{
//                                         .Assignment = &AssignmentExpression{
//                                             .identifier = "y",
//                                             .value = &Expression{
//                                                 .Literal = Literal{
//                                                     .Integer = 20,
//                                                 },
//                                             },
//                                         },
//                                     },
//                                 },
//                                 // println!("{}", suma(x, y));
//                                 Statement{
//                                     .Expression = &Expression{
//                                         .FunctionCall = FunctionCall{
//                                             .function_name = "println",
//                                             .arguments = &[_]*const Expression{
//                                                 &Expression{
//                                                     .Literal = Literal{
//                                                         .String = "{}",
//                                                     },
//                                                 },
//                                                 &Expression{
//                                                     .FunctionCall = FunctionCall{
//                                                         .function_name = "suma",
//                                                         .arguments = &[_]*const Expression{
//                                                             &Expression{
//                                                                 .Identifier = "x",
//                                                             },
//                                                             &Expression{
//                                                                 .Identifier = "y",
//                                                             },
//                                                         },
//                                                     },
//                                                 },
//                                             },
//                                         },
//                                     },
//                                 },
//                             },
//                             .expression = null,
//                         },
//                     },
//                 },
//             },
//         },
//     };
//
//     try test_parser(input, expected_ast);
// }

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
    // var actual_buffer = std.ArrayList(u8).init(allocator);
    // defer actual_buffer.deinit();
    // const actual_writer = actual_buffer.writer();
    //
    // var expected_buffer = std.ArrayList(u8).init(allocator);
    // defer expected_buffer.deinit();
    // const expected_writer = expected_buffer.writer();

    // try serialize(actual_writer, actual_ast);
    // try serialize(expected_writer, expected_ast);
    //
    // const actual_json = actual_buffer.items;
    // const expected_json = expected_buffer.items;

    // try std.testing.expectEqualSlices(u8, actual_json, expected_json);
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

    // if any has expression, compare them
    if (actual_block.expression != null or expected_block.expression != null) {


    try compareExpression(actual_block.expression.?, actual_block.expression.?);
    }
}

/// Compares two `Statement` instances for equality.
fn compareStatement(actual: Statement, expected: Statement) !void {

    try std.testing.expect(std.mem.eql(u8, @tagName(actual), @tagName(expected)));

    switch (actual) {
        .LetStatement => {
            try std.testing.expectEqual(actual.LetStatement.is_mutable, expected.LetStatement.is_mutable);
            try comparePattern(actual.LetStatement.pattern, expected.LetStatement.pattern);
            try compareTypeAnnotation(actual.LetStatement.type_annotation, expected.LetStatement.type_annotation);

            // if any has value
            if (actual.LetStatement.value != null or expected.LetStatement.value != null) {
                try compareExpression(actual.LetStatement.value.?, expected.LetStatement.value.?);
            }
        },
        .Expression => {
            try compareExpression(actual.Expression, expected.Expression);
        },
        .Item => {
            try compareItem(actual.Item.*, expected.Item.*);
        },
        .MacroInvocationSemi => try compareMacroInvocation(
        actual.MacroInvocationSemi.macro_invocation, expected.MacroInvocationSemi.macro_invocation

    )
    }
}

/// Compare 2 macro invocations
fn compareMacroInvocation(actual: MacroInvocation, expected: MacroInvocation) !void {
    try compareSimplePath(actual.simple_path, expected.simple_path);
    try compareDelimTokenTree(actual.delim, expected.delim);
}

/// Compare 2 simple paths
fn compareSimplePath(actual: SimplePath, expected: SimplePath) !void {
    try std.testing.expectEqual(actual.is_absolute, expected.is_absolute);
    try std.testing.expectEqual(actual.path_segments.len, expected.path_segments.len);
    for (actual.path_segments, expected.path_segments) |actual_segment, expected_segment| {
        try std.testing.expect(std.mem.eql(u8, actual_segment, expected_segment));
    }
}

/// Compare 2 delim token trees
fn compareDelimTokenTree(actual: DelimTokenTree, expected: DelimTokenTree) !void {

    try std.testing.expect(std.mem.eql(u8, @tagName(actual), @tagName(expected)));

    switch (actual) {
        .Parentheses, .Braces, .Brackets => |actual_tokens| {

            switch (expected) {
                .Parentheses, .Braces, .Brackets => |expected_tokens| {
                    try std.testing.expectEqual(actual_tokens.len, expected_tokens.len);
                    for (actual_tokens, expected_tokens) |actual_token_tree, expected_token_tree| {
                        try compareTokenTree(actual_token_tree, expected_token_tree);
                    }
                },
            }
        },
    }
}


/// Compare 2 token trees
fn compareTokenTree(actual: TokenTree, expected: TokenTree) !void {
    try std.testing.expect(std.mem.eql(u8, @tagName(actual), @tagName(expected)));
    switch (actual) {
        .Token => try compareToken(actual.Token, expected.Token),
        .DelimTokenTree => return ParseError.NotImplemented,
    }
}

/// Compares two `Pattern` instances for equality.
fn comparePattern(actual: Pattern, expected: Pattern) !void {
    try std.testing.expect(std.mem.eql(u8, @tagName(actual), @tagName(expected)));

    switch (actual) {
        .Identifier => try std.testing.expect(std.mem.eql(u8, actual.Identifier, expected.Identifier)),
        .Wildcard => return ParseError.NotImplemented,
        .Tuple => return ParseError.NotImplemented,
        .Struct => return ParseError.NotImplemented,
    }
}

/// Compares two `Expression` instances for equality.
fn compareExpression(actual: *const Expression, expected: *const Expression) TestParseError!void {
    try std.testing.expect(std.mem.eql(u8, @tagName(actual.*), @tagName(expected.*)));

    switch (actual.*) {
        .Identifier => try compareIdentifier(actual.Identifier, expected.Identifier),
        .Literal => try compareLiteral(actual.Literal, expected.Literal),

        .BinaryOperation => try compareBinaryOperation(actual.BinaryOperation, expected.BinaryOperation),
        .UnaryOperation => try compareUnaryOperation(actual.UnaryOperation, expected.UnaryOperation),
        .FunctionCall => try compareFunctionCall(actual.FunctionCall, expected.FunctionCall),
        .Assignment => try compareAssignmentExpression(actual.Assignment, expected.Assignment),
        else => return ParseError.NotImplemented,
    }
}

/// Compare 2 assignment expressions
fn compareAssignmentExpression(actual: *const AssignmentExpression, expected: *const AssignmentExpression) !void {

    try std.testing.expect(std.mem.eql(u8, actual.*.identifier, expected.*.identifier));
    try compareExpression(actual.*.value, expected.*.value);
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
