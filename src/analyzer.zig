const std = @import("std");
const Parser = @import("parser.zig");
const Program = Parser.Program;
const BaseType = Parser.BaseType;

pub const SemanticError = error{
    UndefinedVariable,
    TypeMismatch,
    DuplicateSymbol,
    InvalidBorrow,
    MissingReturnType,
    NotImplemented,
} || std.mem.Allocator.Error;

pub const Symbol = struct {
    name: []const u8,
    type: ?Parser.TypeAnnotation, // Track types for each symbol.
    is_mutable: bool, // Track mutability.
};
const TypePointers = struct {
    I32: *BaseType,
    I64: *BaseType,
    F32: *BaseType,
    F64: *BaseType,
    Bool: *BaseType,
    Str: *BaseType,
};

/// Symbol table with hierarchical scoping.
pub const SymbolTable = struct {
    parent: ?*const SymbolTable,
    symbols: std.StringHashMap(Symbol),

    pub fn init(allocator: std.mem.Allocator, parent: ?*const SymbolTable) !SymbolTable {
        const symbols = std.StringHashMap(Symbol).init(allocator);
        return SymbolTable{
            .parent = parent,
            .symbols = symbols,
        };
    }

    /// Add a symbol to the current scope.
    pub fn addSymbol(self: *SymbolTable, name: []const u8, symbol: Symbol) SemanticError!void {
        const inserted = try self.symbols.getOrPut(name);

        if (inserted.found_existing) {
            return SemanticError.DuplicateSymbol;
        } else {
            try self.symbols.put(name, symbol);
        }
    }

    /// Resolve a symbol, searching up the scope chain.
    pub fn resolve(self: *const SymbolTable, name: []const u8) ?Symbol {
        return self.symbols.get(name) orelse {
            if (self.parent) |parent_table| {
                return parent_table.resolve(name);
            }
            return null; // Symbol not found.
        };
    }
};

/// Main semantic analyzer struct.
pub const SemanticAnalyzer = struct {
    allocator: std.mem.Allocator,
    scope_stack: std.ArrayList(*SymbolTable), // Stack of scopes
    errors: std.ArrayList(SemanticError),
    type_pointers: TypePointers,

    /// Initialize a new semantic analyzer.
    pub fn init(allocator: std.mem.Allocator) !SemanticAnalyzer {
        const type_pointers = TypePointers{
            .I32 = try allocator.create(BaseType),
            .I64 = try allocator.create(BaseType),
            .F32 = try allocator.create(BaseType),
            .F64 = try allocator.create(BaseType),
            .Bool = try allocator.create(BaseType),
            .Str = try allocator.create(BaseType),
        };

        type_pointers.I32.* = BaseType.I32;
        type_pointers.I64.* = BaseType.I64;
        type_pointers.F32.* = BaseType.F32;
        type_pointers.F64.* = BaseType.F64;
        type_pointers.Bool.* = BaseType.Bool;
        type_pointers.Str.* = BaseType.Str;

        var analyzer = SemanticAnalyzer{
            .allocator = allocator,
            .scope_stack = std.ArrayList(*SymbolTable).init(allocator),
            .errors = std.ArrayList(SemanticError).init(allocator),
            .type_pointers = type_pointers,
        };
        // Push the global scope
        const global_scope = try allocator.create(SymbolTable);
        global_scope.* = SymbolTable.init(allocator, null) catch unreachable;
        try analyzer.scope_stack.append(global_scope);
        return analyzer;
    }

    /// Enter a new scope
    fn enterScope(self: *SemanticAnalyzer) !void {
        const current_scope = self.currentScope() orelse unreachable;
        const new_scope = try self.allocator.create(SymbolTable);
        new_scope.* = try SymbolTable.init(self.allocator, current_scope);
        try self.scope_stack.append(new_scope);
    }

    /// Exit the current scope
    fn exitScope(self: *SemanticAnalyzer) !void {
        _ = self.scope_stack.pop();
    }

    /// Get the current scope
    fn currentScope(self: *SemanticAnalyzer) ?*SymbolTable {
        return self.scope_stack.items[self.scope_stack.items.len - 1];
    }

    /// Collect and store errors
    fn reportError(self: *SemanticAnalyzer, err: SemanticError) !void {
        try self.errors.append(err);
    }

    /// Analyze the given program.
    pub fn analyze(self: *SemanticAnalyzer, program: Program) !void {
        for (program.items) |item| {
            try self.analyzeItem(item);
        }
    }

    /// Analyze a single top-level item (e.g., function).
    fn analyzeItem(self: *SemanticAnalyzer, item: Parser.Item) SemanticError!void {
        switch (item.content) {
            .Function => try self.analyzeFunction(item.content.Function),
            // Handle other item types (structs, enums, etc.)
            // else => self.reportError(SemanticError.NotImplemented) catch {},
        }
    }

    /// Analyze a function definition.
    fn analyzeFunction(self: *SemanticAnalyzer, func: Parser.Function) !void {
        const global_scope = self.currentScope() orelse return;
        const function_symbol = Symbol{
            .name = func.fname,
            .type = func.return_type, // Simplified; consider more detailed type info
            .is_mutable = false,
        };
        // Add function to global scope
        global_scope.addSymbol(func.fname, function_symbol) catch |err| {
            try self.reportError(err);
        };

        // Enter function scope
        self.enterScope() catch {};
        defer self.exitScope() catch {};

        const function_scope = self.currentScope() orelse return;

        // Add function parameters to the function scope
        for (func.parameters) |param| {
            const param_symbol = Symbol{
                .name = param.name,
                .type = param.type_annotation,
                .is_mutable = false,
            };
            function_scope.addSymbol(param.name, param_symbol) catch |err| {
                try self.reportError(err);
            };
        }

        // Analyze the function body
        try self.analyzeBlock(func.body);
    }

    /// Analyze a block of code.
    fn analyzeBlock(self: *SemanticAnalyzer, block: Parser.Block) !void {
        // Enter a new block scope
        self.enterScope() catch {};
        defer self.exitScope() catch {};

        // Analyze each statement in the block
        for (block.statements) |stmt| {
            try self.analyzeStatement(stmt);
        }

        // Analyze the block's trailing expression, if any
        if (block.expression) |expr| {
            _ = try self.analyzeExpression(expr);
        }
    }

    /// Analyze a single statement.
    fn analyzeStatement(self: *SemanticAnalyzer, stmt: Parser.Statement) !void {
        _ = switch (stmt) {
            .LetStatement => try self.analyzeLetStatement(stmt.LetStatement),
            .Expression => try self.analyzeExpression(stmt.Expression),
            .Item => try self.analyzeItem(stmt.Item.*),
            // else => self.reportError(SemanticError.NotImplemented) catch {},
        };
    }

    /// Analyze a let statement.
    fn analyzeLetStatement(self: *SemanticAnalyzer, let_stmt: Parser.LetStatement) !void {
        const current_scope = self.currentScope() orelse return;

        // Extract pattern information
        const pattern = let_stmt.pattern;
        var var_name: []const u8 = "";

        switch (pattern) {
            .Identifier => var_name = pattern.Identifier,
            // Handle other pattern types (Wildcard, Tuple, Struct)
            else => {
                self.reportError(SemanticError.NotImplemented) catch {};
                return;
            },
        }

        // Create the symbol
        const symbol = Symbol{
            .name = var_name,
            .type = let_stmt.type_annotation,
            .is_mutable = let_stmt.is_mutable,
        };

        // Add symbol to current scope
        current_scope.addSymbol(var_name, symbol) catch |err| {
            self.reportError(err) catch {};
        };

        // If there's an initializer, analyze the expression
        if (let_stmt.value) |value_expr| {
            const expr_type = try self.analyzeExpression(value_expr);
            // Type checking logic here
            if (let_stmt.type_annotation) |annot| {
                if (!isTypeCompatible(expr_type, annot)) {
                    self.reportError(SemanticError.TypeMismatch) catch {};
                }
            }
        }
    }

    /// Analyze an expression and optionally compare it to an expected type.
    fn analyzeExpression(self: *SemanticAnalyzer, expr: *const Parser.Expression) !?Parser.TypeAnnotation {
        switch (expr.*) {
            .Identifier => {
                const ident = expr.Identifier;
                // const symbol = self.currentScope() orelse return null;
                const resolved = self.currentScope().?.resolve(ident);
                if (resolved == null) {
                    self.reportError(SemanticError.UndefinedVariable) catch {};
                    return null;
                }
                return resolved.?.type;
            },
            .BinaryOperation => {
                const bin_op = expr.BinaryOperation;
                const left_type = try self.analyzeExpression(bin_op.left) orelse return null;
                const right_type = try self.analyzeExpression(bin_op.right) orelse return null;

                // Simple type compatibility check
                if (!isTypeCompatible(left_type, right_type)) {
                    self.reportError(SemanticError.TypeMismatch) catch {};
                    return null;
                }

                // Assume binary operation returns the left type for simplicity
                return left_type;
            },
            .Literal => {
                return switch (expr.Literal) {
                    .Integer => Parser.TypeAnnotation{
                        .base_type = self.type_pointers.I64,
                        .is_reference = false,
                        .is_mutable = false,
                    },
                    .Float => Parser.TypeAnnotation{
                        .base_type = self.type_pointers.F64,
                        .is_reference = false,
                        .is_mutable = false,
                    },
                    .Boolean => Parser.TypeAnnotation{
                        .base_type = self.type_pointers.Bool,
                        .is_reference = false,
                        .is_mutable = false,
                    },
                    .String => Parser.TypeAnnotation{
                        .base_type = self.type_pointers.Str,
                        .is_reference = false,
                        .is_mutable = false,
                    },
                    else => return SemanticError.NotImplemented,
                };
            },
            // Handle other expression types (UnaryOperation, FunctionCall, etc.)
            else => {
                self.reportError(SemanticError.NotImplemented) catch {};
                return null;
            },
        }
    }

    /// Check if two types are compatible.
    fn isTypeCompatible(left: ?Parser.TypeAnnotation, right: ?Parser.TypeAnnotation) bool {
        if (left == null or right == null) return false;
        return std.mem.eql(u8, @tagName(left.?.base_type.*), @tagName(right.?.base_type.*));
    }

    /// Retrieve all collected errors
    pub fn getErrors(self: *SemanticAnalyzer) []const SemanticError {
        return self.errors.toSlice();
    }
};
