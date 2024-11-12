const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;

pub const ParseError = error{
    UnexpectedToken,
    ExpectedIdentifier,
    ExpectedType,
};

pub fn parse(tokens: []Token) !Program {
    var parser = Parser.init(tokens);
    return parser.parseProgram();
}

pub const Parser = struct {
    tokens: []const Token,
    current: usize, // Index of the current token

    pub fn init(tokens: []Token) Parser {
        return Parser{
            .tokens = tokens,
            .current = 0,
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
        var items = std.ArrayList(Item).init(std.heap.page_allocator);

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
    // item            = [ DocString ]
    //              | function
    //              | struct_def
    //              | enum_def
    //              | trait_def
    //              | impl_def
    //              | use_decl
    //              | mod_decl
    //              | constant
    //              | type_alias
    //              | macro_def
    //              | foreign_mod

    /// Checks if the item has a docstring and if its any of the following:
    /// - Function
    /// - Struct
    /// - Enum
    /// - Trait
    /// - Impl
    /// - Use
    /// - Mod
    /// - Const
    /// - Type
    /// - Macro
    /// - Foreign
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
        std.debug.print("parseFunction: {s}\n", .{function_name});

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

        std.debug.print("parseBlock start\n", .{});

        _ = try self.expect(TokenType.LeftBrace);

        // this line
        var statements = std.ArrayList(Statement).init(std.heap.page_allocator);
        var block_value: ?Expression = null;

        while (self.peek()) |token| {
            if (token.token_type == TokenType.RightBrace) {
                break;
            }

            if (self.parseExpression()) |exp| {
                std.debug.print("parseExpression succeeded\n", .{});
                block_value = exp;
                break;
            } else |_| {
                std.debug.print("parseExpression failed, trying parseStatement\n", .{});
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

        std.debug.print("parseStatement start\n", .{});

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
        var value: ?Expression = null;
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
        var parameters = std.ArrayList(Parameter).init(std.heap.page_allocator);

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

        const base_type_token = self.advance() orelse return error.ExpectedType;
        const base_type = switch (base_type_token.token_type) {
            TokenType.Type => try getBaseType(base_type_token.lexeme),
            TokenType.Identifier => BaseType{ .Path = base_type_token.lexeme },
            else => return error.ExpectedType,
        };

        return TypeAnnotation{
            .is_reference = is_reference,
            .is_mutable = is_mutable,
            .base_type = &base_type,
        };
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
            return BaseType{ .Path = lexeme };
        }
    }


const Precedence = enum(u4) {
        NotFound = 0,
    Lowest = 1,
    LogicalOr=2,       // ||
    LogicalAnd=3,      // &&
    Equality=4,        // ==, !=
    Comparison=5,      // <, >, <=, >=
    Term=6,            // +, -
    Factor=7,          // *, /
    Unary=8,           // !, -
    Call=9,            // function calls
    Primary=10,
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

fn parsePrimary(self: *Parser) !Expression {
    const token = self.advance().?;
    return switch (token.token_type) {
        TokenType.Identifier => Expression{ .Identifier = token.lexeme },
        TokenType.Integer => {
            const value = try std.fmt.parseInt(i64, token.lexeme, 10);
            return Expression{ .Literal = Literal{ .Integer = value } };
        },
        TokenType.Float => {
            const value = try std.fmt.parseFloat(f64, token.lexeme);
            return Expression{ .Literal = Literal{ .Float = value } };
        },
        TokenType.Boolean => {
            const value = std.mem.eql(u8, token.lexeme, "true");
            return Expression{ .Literal = Literal{ .Boolean = value } };
        },
        TokenType.String => Expression{ .Literal = Literal{ .String = token.lexeme } },
        TokenType.Char => Expression{ .Literal = Literal{ .Char = token.lexeme[0] } },
        TokenType.LeftParen => {
            const expr = try self.parseExpressionHelper(Precedence.Lowest);
            _ = try self.expect(TokenType.RightParen);
            return expr;
        },
        else => return error.UnexpectedToken,
    };
}

fn parseUnaryOp(self: *Parser) !Expression {
    const token = self.advance().?;
    const operator = switch (token.token_type) {
        TokenType.Minus => UnaryOperator.Negate,
        TokenType.Exclamation => UnaryOperator.LogicalNot,
        else => return error.UnexpectedToken,
    };

    const operand = try self.parseExpressionHelper(Precedence.Unary);
    return Expression{
        .UnaryOperation = UnaryOperation{
            .operator = operator,
            .operand = &operand,
        },
    };
}
fn parseBinaryOp(self: *Parser, left: Expression, min_prec: Precedence) !Expression {

        std.debug.print("parseBinaryOp start with min_prec: {}\n", .{min_prec});

        var new_left = left;
    while (true) {
        const lookahead = self.peek();
        if (lookahead == null) break;
        const prec = getPrecedence(lookahead.?.token_type);
            std.debug.print("parseBinaryOp, found lookahead: {}\n", .{lookahead.?});
            std.debug.print("parseBinaryOp, found prec: {}\n", .{prec});


            std.debug.print("prec value: {d}\n", .{@intFromEnum(prec)});
            std.debug.print("min_prec value: {d}\n", .{@intFromEnum(min_prec)});


        if (@intFromEnum(prec) < @intFromEnum(min_prec)) {
                std.debug.print("parseBinaryOp, prec < min_prec, breaking\n", .{});
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

            std.debug.print("parseBinaryOp, found operator: {s}\n", .{operator_token});

        // Determine the precedence for the next expression
        const next_min_prec:u4 = switch (operator) {
            BinaryOperator.LogicalOr, BinaryOperator.LogicalAnd, BinaryOperator.Equal, BinaryOperator.NotEqual,
            BinaryOperator.LessThan, BinaryOperator.GreaterThan, BinaryOperator.LessThanOrEqual, BinaryOperator.GreaterThanOrEqual => @intFromEnum(prec) + 1,
            BinaryOperator.Add, BinaryOperator.Subtract => @intFromEnum(prec) + 1,
            BinaryOperator.Multiply, BinaryOperator.Divide => @intFromEnum(prec) + 1,
            else => @intFromEnum(Precedence.Lowest),
        };

            std.debug.print("parseBinaryOp, parsing right side", .{});
        // Parse the right-hand side expression
        const right = try self.parseExpressionHelper(@enumFromInt(next_min_prec));
            std.debug.print("parseBinaryOp, parsed right side\n", .{});


        // Combine the left and right expressions with the operator
        new_left = Expression{
            .BinaryOperation = BinaryOperation{
                .left = &left,
                .operator = operator,
                .right = &right,
            },
        };
    }
        std.debug.print("parseBinaryOp finished , returning new_left\n", .{});

    return new_left;
}

    fn parseExpression(self: *Parser) !Expression {

        std.debug.print("parseExpression start\n", .{});

        return try self.parseExpressionHelper(Precedence.Lowest);
    }

fn parseExpressionHelper(self: *Parser, min_prec: Precedence ) anyerror!Expression {
    const expr = switch (self.peek().?.token_type) {
        TokenType.If => Expression{ .Conditional = &try self.parseConditionalExpression() },
        TokenType.Loop, TokenType.While, TokenType.For => Expression{ .Loop = &try self.parseLoopExpression() },
        TokenType.Identifier, TokenType.Integer, TokenType.Float, TokenType.Boolean, TokenType.String, TokenType.Char, TokenType.LeftParen, TokenType.Minus, TokenType.Exclamation => try self.parseUnaryOrPrimary(),
        else => return error.UnexpectedToken,
    };
    std.debug.print("parseExpressionHelper, found 1st expr\n", .{});

    return try self.parseBinaryOp(expr, min_prec);
}


fn parseUnaryOrPrimary(self: *Parser) !Expression {
    const token = self.peek().?;
    return switch (token.token_type) {
        TokenType.Minus, TokenType.Exclamation => try self.parseUnaryOp(),
        else => try self.parsePrimary(),
    };
}

    fn parseConditionalExpression(self: *Parser) anyerror!ConditionalExpression {
        _ = try self.expect(TokenType.If);
        const condition = try self.parseExpression();
        const then_branch = try self.parseBlock();
        var else_branch: ?Block = null;
        if (self.match(&[_]TokenType{TokenType.Else})) {
                else_branch = try self.parseBlock();
        }
        return ConditionalExpression{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
        };
    }

    fn parseLoopExpression(self: *Parser) anyerror!LoopExpression {
        return LoopExpression{
            .Loop = Loop{ .body = try self.parseBlock() },
        };
    }

    fn parseBinaryOperator(self: *Parser) anyerror!BinaryOperator {
        const current_token = self.peek().?;
        const operator = switch (current_token.token_type) {
            TokenType.Assign => BinaryOperator.Assign,
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
        std.debug.print("found BinaryOperator: {}\n", .{current_token});
        _ = self.advance();
        return operator;
    }

    fn parseAssignmentExpression(self: *Parser) anyerror!AssignmentExpression {
        const identifier = (try self.expect(TokenType.Identifier)).lexeme;
        _ = try self.expect(TokenType.Assign);
        std.debug.print("parseAssignmentExpression: {s}\n", .{identifier});

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
    types: []TypeAnnotation,
};

pub const ArrayType = struct {
    element_type: TypeAnnotation,
    size: u64, // Assuming size is known at compile time, idk
};

pub const FunctionType = struct {
    parameters: []TypeAnnotation,
    return_type: ?TypeAnnotation,
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
    value: Expression,
};

pub const ConstantVariable = struct {
    name: []const u8,
    is_mutable: bool,
    type_annotation: ?TypeAnnotation,
    value: Expression,
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
    Expression: Expression,
    Item: Item,
};

pub const LetStatement = struct {
    is_mutable: bool,
    pattern: Pattern,
    type_annotation: ?TypeAnnotation,
    value: ?Expression,
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
    expression: ?Expression,
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
    Expression: *Expression,
};

pub const AssignmentExpression = struct {
    identifier: []const u8,
    value: Expression,
};

pub const ConditionalExpression = struct {
    condition: Expression,
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
    arguments: []Expression,
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
    condition: Expression,
    body: Block,
};

pub const ForLoop = struct {
    iterator: Pattern,
    iterable: Expression,
    body: Block,
};

pub const IfLetExpression = struct {
    pattern: Pattern,
    expression: Expression,
    then_branch: Block,
    else_branch: ?ElseBranch,
};

pub const MatchExpression = struct {
    value: Expression,
    arms: []MatchArm,
};

pub const MatchArm = struct {
    pattern: Pattern,
    guard: ?Expression,
    result: Expression,
};


test "Parse a simple safe function" {
    const input =
        \\fn add(a: i32, b: i32) -> i32 {
        \\    a + b
        \\}
    ;

    const expected_ast = Program{
        .items = &[_]Item{
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
                            .expression = Expression{
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
        },
    };

    try test_parser(input, expected_ast);
}

fn test_parser(input: []const u8, expected_ast: Program) !void {
    var lexer = Lexer{ .input = input };
    const tokens = try lexer.tokenize();
    const ast = try parse(tokens);

    try std.testing.expectEqual(ast, expected_ast);
}
