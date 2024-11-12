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
            };} else {
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
        _ =try self.expect(TokenType.LeftBrace);

        // this line
        var statements = std.ArrayList(Statement).init(std.heap.page_allocator);

        while (self.peek()) |token| {
            if (token.token_type == TokenType.RightBrace) {
                break;
            }
            const statement = try self.parseStatement();
            statements.append(statement) catch |err| {
                return err;
            };
        }

        var block_value: ?Expression = null;

        if (!self.match(&[_]TokenType{TokenType.RightBrace})) {
            block_value = try self.parseExpression();
            _ = try self.expect(TokenType.RightBrace);
        }

        return Block{
            .statements = try statements.toOwnedSlice(),
            .expression = block_value,
        };

    }

    fn parseStatement(self: *Parser) !Statement {
        const current_token = self.peek().?;

        if (current_token.token_type == TokenType.Let) {
            return Statement{.LetStatement = try self.parseLetStatement()};
        } else {
            return Statement{.Expression = try self.parseExpression()};
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
            .pattern = Pattern{.Identifier = pattern},
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
            TokenType.Identifier => BaseType{.Path = base_type_token.lexeme},
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
            return BaseType{.Path = lexeme};
        }
    }
    fn parseExpression(self: *Parser) anyerror!Expression {
        return try self.parseAssignmentExpression();
    }

    fn parseAssignmentExpression(self: *Parser) anyerror!Expression {
        const identifier = (try self.expect(TokenType.Identifier)).lexeme;
        _ = try self.expect(TokenType.Assign);

//         ❯ zig test src/parser.zig
// src/parser.zig:345:58: error: unable to resolve inferred error set
//         const assignment_value = try self.parseExpression();
        const assignment_value = try self.parseExpression();

        return Expression{
            .Assignment = &AssignmentExpression{
                .identifier = identifier,
                .value = assignment_value,
            },
        };

    }
};

// Continue with parseLogicalOrExpression, parseLogicalAndExpression, etc.

// Implement parseFunction, parseStructDefinition, parseStaticVariableDeclaration, parseStatement, etc.

// Program represents the entire Rust program, consisting of multiple items.
pub const Program = struct {
    items: []const Item,
};

// Item represents any top-level item in Rust (functions, structs, enums, etc.).
pub const Item = struct {
    docstring: ?[]const u8,
    content: ItemContent,
};

// ItemContent enumerates all possible top-level items.
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

// Function represents a function definition.
pub const Function = struct {
    is_unsafe: bool,
    fname: []const u8,
    generics: ?Generics,
    parameters: []const Parameter,
    return_type: ?TypeAnnotation,
    body: Block,
};

// Parameter represents a function parameter.
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
    bounds: []const u8, // Simplified: can be extended to more complex bounds
};

// TypeAnnotation represents type information, including references and mutability.
pub const TypeAnnotation = struct {
    is_reference: bool,
    is_mutable: bool,
    base_type: *const BaseType,
};

// BaseType enumerates primitive types and identifiers.
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
    Path: []const u8, // Represents paths like std::vec::Vec
};

// TupleType represents a tuple type.
pub const TupleType = struct {
    types: []TypeAnnotation,
};

// ArrayType represents an array type with a specified size.
pub const ArrayType = struct {
    element_type: TypeAnnotation,
    size: u64, // Assuming size is known at compile time
};

// FunctionType represents a function type.
pub const FunctionType = struct {
    parameters: []TypeAnnotation,
    return_type: ?TypeAnnotation,
};

// GenericType represents a generic type with optional type parameters.
pub const GenericType = struct {
    name: []const u8,
    type_params: []TypeAnnotation,
};

// StructDefinition represents a struct.
pub const StructDefinition = struct {
    name: []const u8,
    generics: ?Generics,
    fields: []StructField,
};

// StructField represents a single field within a struct.
pub const StructField = struct {
    name: []const u8,
    type_annotation: TypeAnnotation,
};

// EnumDefinition represents an enum.
pub const EnumDefinition = struct {
    name: []const u8,
    generics: ?Generics,
    variants: []EnumVariant,
};

// EnumVariant represents a single variant within an enum.
pub const EnumVariant = struct {
    name: []const u8,
    fields: ?[]TypeAnnotation, // Tuple variants have types
    // For struct-like variants, extend this to include named fields
};

// TraitDefinition represents a trait.
pub const TraitDefinition = struct {
    name: []const u8,
    generics: ?Generics,
    items: []TraitItem,
};

// TraitItem represents items within a trait (functions, type aliases).
pub const TraitItem = union(enum) {
    Function: FunctionSignature,
    TypeAlias: TypeAlias,
};

// FunctionSignature represents a function signature within traits and impls.
pub const FunctionSignature = struct {
    fname: []const u8,
    generics: ?Generics,
    parameters: []Parameter,
    return_type: ?TypeAnnotation,
};

// ImplDefinition represents an implementation block.
pub const ImplDefinition = struct {
    generics: ?Generics,
    trait: ?[]const u8, // The trait being implemented, if any
    for_type: TypeAnnotation,
    items: []ImplItem,
};

// ImplItem represents items within an impl block (functions, constants).
pub const ImplItem = union(enum) {
    Function: Function,
    Constant: ConstantVariable,
};

// UseDeclaration represents a use statement.
pub const UseDeclaration = struct {
    path: Path,
};

// ModuleDeclaration represents a module.
pub const ModuleDeclaration = struct {
    name: []const u8,
    items: ?[]Item, // Inline module with items or external module
};

pub const StaticVariable = struct {
    name: []const u8,
    type_annotation: TypeAnnotation,
    is_mutable: bool,
    value: Expression,
};

// Constant represents a constant declaration.
pub const ConstantVariable = struct {
    name: []const u8,
    is_mutable: bool,
    type_annotation: ?TypeAnnotation,
    value: Expression,
};

// TypeAlias represents a type alias.
pub const TypeAlias = struct {
    name: []const u8,
    generics: ?Generics,
    aliased_type: TypeAnnotation,
};

// MacroDefinition represents a macro definition.
pub const MacroDefinition = struct {
    name: []const u8,
    rules: []MacroRule,
};

// MacroRule represents a single macro rule.
pub const MacroRule = struct {
    pattern: []const u8,      // Simplified: actual pattern structure can be more complex
    replacement: []const u8,  // Simplified: actual replacement structure can be more complex
};

// ForeignMod represents an external module.
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
    segments: []const u8, // Simplified: can be an array of identifiers
};

// Statement represents all possible statements within a block.
pub const Statement = union(enum) {
    LetStatement: LetStatement,
    Expression: Expression,
    Item: Item,
};

// LetStatement represents a let binding.
pub const LetStatement = struct {
    is_mutable: bool,
    pattern: Pattern,
    type_annotation: ?TypeAnnotation,
    value: ?Expression,
};

// Pattern represents patterns used in let bindings, match arms, etc.
pub const Pattern = union(enum) {
    Identifier: []const u8,
    Wildcard,
    Tuple: TuplePattern,
    Struct: StructPattern,
    // Add more pattern types as needed
};

// TuplePattern represents a tuple pattern.
pub const TuplePattern = struct {
    patterns: []Pattern,
};

// StructPattern represents a struct pattern with fields.
pub const StructPattern = struct {
    name: []const u8,
    fields: []StructPatternField,
};

// StructPatternField represents a single field in a struct pattern.
pub const StructPatternField = struct {
    name: []const u8,
    pattern: Pattern,
};

// ReturnStatement represents a return statement.
pub const ReturnStatement = struct {
    value: ?Expression,
};

// Block represents a block of statements with an optional trailing expression.
pub const Block = struct {
    statements: []const Statement,
    expression: ?Expression,
};

// WhileStatement represents a while loop.
pub const WhileStatement = struct {
    condition: Expression,
    body: Block,
};

// ForStatement represents a for loop.
pub const ForStatement = struct {
    iterator: Pattern,
    iterable: Expression,
    body: Block,
};

// IfExpression represents an if expression with optional else branch.
pub const IfExpression = struct {
    condition: Expression,
    then_branch: Block,
    else_branch: ?ElseBranch,
};

// ElseBranch represents either an else-if or an else block.
pub const ElseBranch = union(enum) {
    ElseIf: IfExpression,
    ElseBlock: Block,
};

// Expression represents all possible expressions.
pub const Expression = union(enum) {
    Assignment: *const AssignmentExpression,
    Conditional: *ConditionalExpression,
    Loop: *LoopExpression,
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

// Literal represents literal values.
pub const Literal = union(enum) {
    Integer: i64,
    Float: f64,
    Boolean: bool,
    String: []const u8,
    Char: u8,
    // Add more literals as needed (e.g., byte literals)
};

// BinaryOperation represents a binary operation.
pub const BinaryOperation = struct {
    left: *const Expression,
    operator: BinaryOperator,
    right: *const Expression,
};

// UnaryOperation represents a unary operation.
pub const UnaryOperation = struct {
    operator: UnaryOperator,
    operand: *Expression,
};

// FunctionCall represents a function call expression.
pub const FunctionCall = struct {
    function_name: []const u8,
    arguments: []Expression,
};

// FieldAccess represents accessing a field of a struct or enum.
pub const FieldAccess = struct {
    base: *Expression,
    field_name: []const u8,
};

// BinaryOperator enumerates all binary operators.
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
    // Add more operators as needed (e.g., bitwise operators)
};

// UnaryOperator enumerates all unary operators.
pub const UnaryOperator = enum {
    Negate,
    LogicalNot,
    // Add more unary operators as needed
};


// LoopExpression represents different types of loops.
pub const LoopExpression = union(enum) {
    Loop: Loop,
    While: WhileLoop,
    For: ForLoop,
    // Add more loop types if needed
};

// Loop represents an infinite loop (`loop { ... }`).
pub const Loop = struct {
    body: Block,
};

// WhileLoop represents a while loop.
pub const WhileLoop = struct {
    condition: Expression,
    body: Block,
};

// ForLoop represents a for loop.
pub const ForLoop = struct {
    iterator: Pattern,
    iterable: Expression,
    body: Block,
};

// IfLetExpression represents an if let expression.
pub const IfLetExpression = struct {
    pattern: Pattern,
    expression: Expression,
    then_branch: Block,
    else_branch: ?ElseBranch,
};

// MatchExpression represents a match expression.
pub const MatchExpression = struct {
    value: Expression,
    arms: []MatchArm,
};

// MatchArm represents a single arm within a match expression.
pub const MatchArm = struct {
    pattern: Pattern,
    guard: ?Expression,
    result: Expression,
};

// // Additional Structures for Patterns, etc. //

// (Optional) You can further expand patterns and other components as needed.

// Example usage:

// ```zig
// const program = Program{
//     .items = &[_]Item{
//         Item{
//             .docstring = null,
//             .content = .Function(Function{
//                 .is_unsafe = false,
//                 .fname = "main",
//                 .generics = null,
//                 .parameters = &[_]Parameter{},
//                 .return_type = null,
//                 .body = Block{
//                     .statements = &[_]Statement{},
//                     .expression = null,
//                 },
//             }),
//         },
//         // Add more items...
//     },
// };
// ```

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
                .content = ItemContent{.Function = Function{
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
