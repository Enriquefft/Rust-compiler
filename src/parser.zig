const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
const serialize = @import("serialize.zig").serialize_program;

pub const ParseError = error{
    UnexpectedToken,
    ExpectedIdentifier,
    ExpectedType,
};

pub fn parse(tokens: []Token, allocator: *const std.mem.Allocator) !Program {
    var parser = Parser.init(tokens, allocator);
    return parser.parseProgram();
}

pub const Parser = struct {
    tokens: []const Token,
    current: usize, // Index of the current token
    allocator: *const std.mem.Allocator,

    pub fn init(tokens: []Token, allocator: *const std.mem.Allocator) Parser {
        return Parser{
            .tokens = tokens,
            .current = 0,
            .allocator = allocator,
        };
    }

    /// Attempts to peek at the current token without advancing the parser.
    /// Returns
    /// null if there are no more tokens.
    /// The current token otherwise.
    fn peek(self: *Parser) ?Token {
        if (self.current < self.tokens.len) {
            return self.tokens[self.current];
        } else {
            return null;
        }
    }

    /// Advances the parser to the next token and returns the current token.
    /// Returns
    /// null if there are no more tokens.
    /// The current token otherwise.
    fn advance(self: *Parser) ?Token {
        if (self.current < self.tokens.len) {
            const token = self.tokens[self.current];
            self.current += 1;
            return token;
        } else {
            return null;
        }
    }

    /// Consumes the current token if it matches any of the provided token types.
    /// Returns
    /// true if the current token matches any of the provided
    /// false otherwise
    fn match(self: *Parser, token_types: []const TokenType) bool {
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

    /// Consumes the current token if it matches the provided token type.
    /// If the current token does not match the provided token type, returns an error.
    /// Returns
    /// the matched token if it matches the provided token type
    fn expect(self: *Parser, token_type: TokenType) !Token {
        const current_token = self.peek().?;
        if (current_token.token_type != token_type) {
            return error.UnexpectedToken;
        }
        return self.advance() orelse error.UnexpectedToken;
    }

    pub fn parseProgram(self: *Parser) !Program {
        var items = std.ArrayList(Item).init(self.allocator.*);

        while (self.peek()) |token| {
            if (token.token_type == TokenType.EndOfFile) {
                break;
            }

            const item = try self.parseItem();
            items.append(item) catch |err| {
                return err;
            };
        }

        return Program{
            .items = try items.toOwnedSlice(),
        };
    }

    fn parseItem(self: *Parser) !Item {
        var docstring: ?[]const u8 = null;

        if (self.match(&[_]TokenType{TokenType.DocString})) {
            docstring = self.advance().?.lexeme;
        }

        const current_token = self.peek().?;

        if (current_token.token_type == TokenType.FN or current_token.token_type == TokenType.Unsafe) {
            return Item{
                .docstring = docstring,
                .content = .{ .Function = try self.parseFunction() },
            };
        } else {
            // error
            return error.UnexpectedToken;
        }

        // } else if (current_token.token_type == TokenType.Struct) {
        //     return Item{
        //     .docstring = docstring,
        //     .content = .{.StructDefinition = try self.parseStructDefinition() },
        //     };
        // } else if (current_token.token_type == TokenType.Static) {
        //     return Item{
        //     .docstring = docstring,
        //     .content = .{.StaticVariableDeclaration = try self.parseStaticVariableDeclaration()},
        //     };
        // } else if (current_token.token_type == TokenType.Static) {
        //     return Item{
        //     .docstring = docstring,
        //     .content = .{.StaticVariableDeclaration = try self.parseStaticVariableDeclaration()},
        // };
        // } else if (current_token.token_type == TokenType.Enum) {
        //     return Enum{
        //     .docstring = docstring,
        //     .content = .StaticVariableDeclaration(try self.parseStaticVariableDeclaration()),
        //     };
        // }

    }

    fn parseFunction(self: *Parser) !Function {
        var is_unsafe = false;
        if (self.match(&[_]TokenType{TokenType.Unsafe})) {
            is_unsafe = true;
        }
        _ = try self.expect(TokenType.FN);

        const function_name = (try self.expect(TokenType.Identifier)).lexeme;

        _ = try self.expect(TokenType.LeftParen);
        const parameters = try self.parseParameterList();
        _ = try self.expect(TokenType.RightParen);

        var return_type: ?TypeAnnotation = null;
        if (self.match(&[_]TokenType{TokenType.Arrow})) {
            return_type = try self.parseTypeAnnotation();
        }

        const body = try self.parseBlock();

        return Function{
            .is_unsafe = is_unsafe,
            .fname = function_name,
            .parameters = parameters,
            .generics = null,
            .return_type = return_type,
            .body = body,
        };
    }

    fn parseBlock(self: *Parser) !Block {
        _ = try self.expect(TokenType.LeftBrace);

        // this line
        var statements = std.ArrayList(Statement).init(self.allocator.*);
        var block_value: ?*const Expression = null;

        while (self.peek()) |token| {
            if (token.token_type == TokenType.RightBrace) {
                break;
            }

            if (self.parseExpression()) |exp| {
                block_value = exp;
                break;
            } else |_| {
                const statement = try self.parseStatement();
                try statements.append(statement);
            }
        }
        _ = try self.expect(TokenType.RightBrace);

        return Block{
            .statements = try statements.toOwnedSlice(),
            .expression = block_value,
        };
    }

    fn parseStatement(self: *Parser) !Statement {
        const current_token = self.peek().?;

        if (current_token.token_type == TokenType.Let) {
            return Statement{ .LetStatement = try self.parseLetStatement() };
        } else {
            return Statement{ .Expression = try self.parseExpression() };
        }
    }

    fn parseLetStatement(self: *Parser) !LetStatement {
        _ = try self.expect(TokenType.Let);
        const is_mutable = self.match(&[_]TokenType{TokenType.Mut});
        const pattern = (try self.expect(TokenType.Identifier)).lexeme;
        var type_annotation: ?TypeAnnotation = null;
        if (self.match(&[_]TokenType{TokenType.DoubleColon})) {
            type_annotation = try self.parseTypeAnnotation();
        }
        var value: ?*const Expression = null;
        if (self.match(&[_]TokenType{TokenType.Assign})) {
            value = try self.parseExpression();
        }
        return LetStatement{
            .is_mutable = is_mutable,
            .pattern = Pattern{ .Identifier = pattern },
            .type_annotation = type_annotation,
            .value = value,
        };
    }

    fn parseParameterList(self: *Parser) ![]Parameter {
        var parameters = std.ArrayList(Parameter).init(self.allocator.*);

        if (self.peek()) |token| {
            if (token.token_type == TokenType.RightParen) {
                // Empty parameter list
                return parameters.toOwnedSlice();
            }
        }

        while (true) {
            const param = try self.parseParameter();
            parameters.append(param) catch |err| {
                return err;
            };

            if (!self.match(&[_]TokenType{TokenType.Comma})) {
                break;
            }
        }

        return parameters.toOwnedSlice();
    }

    fn parseParameter(self: *Parser) !Parameter {
        const name = (try self.expect(TokenType.Identifier)).lexeme;

        _ = try self.expect(TokenType.DoubleColon);

        return Parameter{
            .name = name,
            .type_annotation = try parseTypeAnnotation(self),
        };
    }

    fn parseTypeAnnotation(self: *Parser) !TypeAnnotation {
        var is_reference = false;
        var is_mutable = false;

        if (self.match(&[_]TokenType{TokenType.Ampersand})) {
            is_reference = true;
            if (self.match(&[_]TokenType{TokenType.Mut})) {
                is_mutable = true;
            }
        }

        const base_type = try parseBaseType(self);
        // const base_type_token = self.advance() orelse return error.ExpectedType;
        //
        // const base_type = switch (base_type_token.token_type) {
        //     TokenType.Type => try getBaseType(base_type_token.lexeme),
        //     TokenType.Identifier => BaseType{ .Path = base_type_token.lexeme },
        //     else => return error.ExpectedType,
        // };

        return TypeAnnotation{
            .is_reference = is_reference,
            .is_mutable = is_mutable,
            .base_type = base_type,
        };
    }

    fn parseBaseType(self: *Parser) !*const BaseType {
        const token = self.peek().?;
        const base_type = try self.allocator.create(BaseType);
        switch (token.token_type) {
            TokenType.Type => {
                base_type.* = try getBaseType(token.lexeme);
                _ = self.advance().?; // Consume the type token
            },
            TokenType.Identifier => {
                const path = token.lexeme;
                _ = self.advance().?; // Consume the identifier
                // Allocate a BaseType with Path variant
                base_type.* = BaseType{ .Path = path };
                return base_type;
            },
            else => return error.ExpectedType,
        }
        return base_type;
    }

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
        } else {
            std.debug.print("Parsed type-path: {s}\n", .{lexeme});
            return BaseType{ .Path = lexeme };
        }
    }

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

    fn parsePrimary(self: *Parser) !*const Expression {
        const token = self.advance().?;

        if (token.token_type == TokenType.LeftParen) {
            const expr = try self.parseExpressionHelper(Precedence.Lowest);
            _ = try self.expect(TokenType.RightParen);
            return expr;
        }

        const expr_ptr = try self.allocator.create(Expression);

        const expr: Expression = switch (token.token_type) {
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
        expr_ptr.* = expr;
        return expr_ptr;
    }

    fn parseUnaryOp(self: *Parser) !*const Expression {
        const token = self.advance().?;
        const operator = switch (token.token_type) {
            TokenType.Minus => UnaryOperator.Negate,
            TokenType.Exclamation => UnaryOperator.LogicalNot,
            else => return error.UnexpectedToken,
        };

        const expr_ptr = try self.allocator.create(Expression);
        const operand = try self.parseExpressionHelper(Precedence.Unary);

        expr_ptr.* = Expression{
            .UnaryOperation = UnaryOperation{
                .operator = operator,
                .operand = operand,
            },
        };
        return expr_ptr;
    }
    fn parseBinaryOp(self: *Parser, left: *const Expression, min_prec: Precedence) !*const Expression {
        var current_left = left;
        while (true) {
            const lookahead = self.peek();
            if (lookahead == null) break;
            const prec = getPrecedence(lookahead.?.token_type);
            if (@intFromEnum(prec) < @intFromEnum(min_prec)) {
                break;
            }

            const operator_token = self.advance().?;
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
                else => return error.UnexpectedToken,
            };

            // Determine the precedence for the next expression
            const next_min_prec: u4 = switch (operator) {
                BinaryOperator.LogicalOr, BinaryOperator.LogicalAnd, BinaryOperator.Equal, BinaryOperator.NotEqual, BinaryOperator.LessThan, BinaryOperator.GreaterThan, BinaryOperator.LessThanOrEqual, BinaryOperator.GreaterThanOrEqual => @intFromEnum(prec) + 1,
                BinaryOperator.Add, BinaryOperator.Subtract => @intFromEnum(prec) + 1,
                BinaryOperator.Multiply, BinaryOperator.Divide => @intFromEnum(prec) + 1,
                else => @intFromEnum(Precedence.Lowest),
            };

            // Parse the right-hand side expression
            const right_expr = try self.parseExpressionHelper(@enumFromInt(next_min_prec));

            // Allocate new BinaryOperation on the heap
            const bin_op = try self.allocator.create(BinaryOperation);
            bin_op.* = BinaryOperation{
                .left = current_left,
                .operator = operator,
                .right = right_expr,
            };

            // Allocate new Expression on the heap
            const new_expr = try self.allocator.create(Expression);
            new_expr.* = Expression{ .BinaryOperation = bin_op.* };

            // Update current_left for the next iteration
            current_left = new_expr;
        }
        return current_left;
    }

    fn parseExpression(self: *Parser) !*const Expression {
        return try self.parseExpressionHelper(Precedence.Lowest);
    }

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
            else => return error.UnexpectedToken,
        }
    }

    fn parseUnaryOrPrimary(self: *Parser) !*const Expression {
        const token = self.peek().?;
        return switch (token.token_type) {
            TokenType.Minus, TokenType.Exclamation => try self.parseUnaryOp(),
            else => try self.parsePrimary(),
        };
    }

    fn parseConditionalExpression(self: *Parser) anyerror!*const ConditionalExpression {
        _ = try self.expect(TokenType.If);
        const condition = try self.parseExpression();
        const then_branch = try self.parseBlock();
        var else_branch: ?Block = null;
        if (self.match(&[_]TokenType{TokenType.Else})) {
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

    fn parseLoopExpression(self: *Parser) anyerror!*const LoopExpression {
        const loop_expr = try self.allocator.create(LoopExpression);
        loop_expr.* = LoopExpression{
            .Loop = Loop{ .body = try self.parseBlock() },
        };
        return loop_expr;
    }

    fn parseAssignmentExpression(self: *Parser) anyerror!AssignmentExpression {
        const identifier = (try self.expect(TokenType.Identifier)).lexeme;
        _ = try self.expect(TokenType.Assign);

        const assignment_value = try self.parseExpression();

        return AssignmentExpression{
            .identifier = identifier,
            .value = assignment_value,
        };
    }
};

// Continue with parseLogicalOrExpression, parseLogicalAndExpression, etc.

// Implement parseFunction, parseStructDefinition, parseStaticVariableDeclaration, parseStatement, etc.

pub const Program = struct {
    items: []const Item,
};

// Item represents any top-level item in Rust (functions, structs, enums, etc.), each with an optional docstring.
pub const Item = struct {
    docstring: ?[]const u8,
    content: ItemContent,
};

pub const ItemContent = union(enum) {
    Function: Function,
    StructDefinition: StructDefinition,
    EnumDefinition: EnumDefinition,
    TraitDefinition: TraitDefinition,
    ImplDefinition: ImplDefinition,
    UseDeclaration: UseDeclaration,
    ModuleDeclaration: ModuleDeclaration,
    StaticVariable: StaticVariable,
    ConstantVariable: ConstantVariable,
    TypeAlias: TypeAlias,
    MacroDefinition: MacroDefinition,
    ForeignMod: ForeignMod,
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

// Generics represents generic parameters in functions, structs, enums, etc.
pub const Generics = struct {
    params: []GenericParam,
};

// GenericParam represents a single generic parameter with optional bounds.
pub const GenericParam = struct {
    name: []const u8,
    bounds: []const u8,
};

// TypeAnnotation represents type information, including references and mutability.
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
    size: u64, // Assuming size is known at compile time, idk
};

pub const FunctionType = struct {
    parameters: []const TypeAnnotation,
    return_type: ?*const TypeAnnotation,
};

pub const GenericType = struct {
    name: []const u8,
    type_params: []TypeAnnotation,
};

pub const StructDefinition = struct {
    name: []const u8,
    generics: ?Generics,
    fields: []StructField,
};

pub const StructField = struct {
    name: []const u8,
    type_annotation: TypeAnnotation,
};

pub const EnumDefinition = struct {
    name: []const u8,
    generics: ?Generics,
    variants: []EnumVariant,
};

pub const EnumVariant = struct {
    name: []const u8,
    fields: ?[]TypeAnnotation,
};

// todo:
pub const TraitDefinition = struct {
    name: []const u8,
    generics: ?Generics,
    items: []TraitItem,
};

pub const TraitItem = union(enum) {
    Function: FunctionSignature,
    TypeAlias: TypeAlias,
};

pub const FunctionSignature = struct {
    fname: []const u8,
    generics: ?Generics,
    parameters: []Parameter,
    return_type: ?TypeAnnotation,
};

pub const ImplDefinition = struct {
    generics: ?Generics,
    trait: ?[]const u8,
    for_type: TypeAnnotation,
    items: []ImplItem,
};

// ImplItem represents items within an impl block (functions, constants).
pub const ImplItem = union(enum) {
    Function: Function,
    Constant: ConstantVariable,
};

pub const UseDeclaration = struct {
    path: Path,
};

pub const ModuleDeclaration = struct {
    name: []const u8,
    items: ?[]Item,
};
/////////

pub const StaticVariable = struct {
    name: []const u8,
    type_annotation: TypeAnnotation,
    is_mutable: bool,
    value: *const Expression,
};

pub const ConstantVariable = struct {
    name: []const u8,
    is_mutable: bool,
    type_annotation: ?TypeAnnotation,
    value: *const Expression,
};

pub const TypeAlias = struct {
    name: []const u8,
    generics: ?Generics,
    aliased_type: TypeAnnotation,
};

pub const MacroDefinition = struct {
    name: []const u8,
    rules: []MacroRule,
};

pub const MacroRule = struct {
    pattern: []const u8,
    replacement: []const u8,
};

pub const ForeignMod = struct {
    abi: ?[]const u8, // e.g., "C"
    items: []ForeignItem,
};

// ForeignItem represents items within a foreign module (functions, statics).
pub const ForeignItem = union(enum) {
    Function: ForeignFunction,
    Static: ForeignStatic,
};

// ForeignFunction represents a foreign function declaration.
pub const ForeignFunction = struct {
    name: []const u8,
    parameters: []Parameter,
    return_type: ?TypeAnnotation,
};

// ForeignStatic represents a foreign static variable declaration.
pub const ForeignStatic = struct {
    name: []const u8,
    type_annotation: TypeAnnotation,
    is_mutable: bool,
};

// Path represents a module path (e.g., std::vec::Vec).
pub const Path = struct {
    segments: []const u8, // no per-path validation
};

pub const Statement = union(enum) {
    LetStatement: LetStatement,
    Expression: *const Expression,
    Item: Item,
};

pub const LetStatement = struct {
    is_mutable: bool,
    pattern: Pattern,
    type_annotation: ?TypeAnnotation,
    value: ?*const Expression,
};

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

pub const ReturnStatement = struct {
    value: ?Expression,
};

// Block represents a block of statements with an optional trailing expression.
pub const Block = struct {
    statements: []const Statement,
    expression: ?*const Expression,
};

pub const WhileStatement = struct {
    condition: Expression,
    body: Block,
};

pub const ForStatement = struct {
    iterator: Pattern,
    iterable: Expression,
    body: Block,
};

pub const IfExpression = struct {
    condition: Expression,
    then_branch: Block,
    else_branch: ?ElseBranch,
};

pub const ElseBranch = union(enum) {
    ElseIf: IfExpression,
    ElseBlock: Block,
};

pub const Expression = union(enum) {
    Assignment: *const AssignmentExpression,
    Conditional: *const ConditionalExpression,
    Loop: *const LoopExpression,
    FunctionCall: FunctionCall,
    BinaryOperation: BinaryOperation,
    UnaryOperation: UnaryOperation,
    Literal: Literal,
    Identifier: []const u8,
    Expression: *const Expression,
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
    arguments: []*const Expression,
};

pub const FieldAccess = struct {
    base: *Expression,
    field_name: []const u8,
};

pub const BinaryOperator = enum {
    Assign,
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
    Loop: Loop,
    While: WhileLoop,
    For: ForLoop,
};

// Loop represents an infinite loop (`loop { ... }`).
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

pub const IfLetExpression = struct {
    pattern: Pattern,
    expression: *const Expression,
    then_branch: Block,
    else_branch: ?ElseBranch,
};

pub const MatchExpression = struct {
    value: *const Expression,
    arms: []MatchArm,
};

pub const MatchArm = struct {
    pattern: Pattern,
    guard: ?*const Expression,
    result: *const Expression,
};

const testing = std.testing;

test "Parse a simple safe function" {
    const input =
        \\fn add(a: i32, b: i32) -> i32 {
        \\    a + b
        \\}
    ;

    const items: []const Item = &[_]Item{
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
    };

    const expected_ast = Program{ .items = items };

    try test_parser(input, expected_ast);
}

fn test_parser(input: []const u8, expected_ast: Program) !void {
    // Temporal fix
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);

    // this will free anything created from this arena
    defer arena.deinit();

    // create an std.mem.Allocator from the arena, this will be
    // the allocator we'll use internally
    const allocator = arena.allocator();

    // TODO: Implement deallocation at parse level as a member function
    // const allocator = &testing.allocator;

    var lexer = Lexer{ .input = input };
    const tokens = try lexer.tokenize();
    const ast = try parse(tokens, &allocator);

    var actual_buffer = std.ArrayList(u8).init(allocator);
    defer actual_buffer.deinit();
    const actual_writer = actual_buffer.writer();

    var expected_buffer = std.ArrayList(u8).init(allocator);
    defer expected_buffer.deinit();
    const expected_writer = expected_buffer.writer();

        try serialize(actual_writer, ast);
     try serialize(expected_writer, expected_ast);

    const actual_json = actual_buffer.items;
    const expected_json = expected_buffer.items;

    try std.testing.expectEqualSlices(u8, actual_json, expected_json);
}
