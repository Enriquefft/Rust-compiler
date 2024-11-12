const std = @import("std");
const Parser = @import("parser.zig");
const parse = Parser.parse;
const Lexer = @import("lexer.zig").Lexer;

// Function to print indentation
fn indent(writer: anytype, level: usize) !void {
    for (0..level) |_| {
        try writer.print("    ", .{});
    }
}

// Function to escape strings for safe printing
fn escape_string(s: []const u8) []const u8 {
    // Simple escape implementation; can be extended
    return s;
}

// Function to pretty print the entire Program
pub fn pretty_print_program(writer: anytype, program: Parser.Program) !void {
    for (program.items) |item| {
        try pretty_print_item(writer, item, 0);
    }
}

// Function to pretty print an Item
fn pretty_print_item(writer: anytype, item: Parser.Item, level: usize) !void {
    if (item.docstring) |doc| {
        try indent(writer, level);
        try writer.print("// {s}\n", .{doc});
    }

    switch (item.content) {
        .Function => |func| {
            try pretty_print_function(writer, func, level);
        },
        .StructDefinition => |struct_def| {
            try pretty_print_struct(writer, struct_def, level);
        },
        .EnumDefinition => |enum_def| {
            try pretty_print_enum(writer, enum_def, level);
        },
        .TraitDefinition => |trait_def| {
            try pretty_print_trait(writer, trait_def, level);
        },
        .ImplDefinition => |impl_def| {
            try pretty_print_impl(writer, impl_def, level);
        },
        .UseDeclaration => |use_decl| {
            try indent(writer, level);
            try writer.print("use {s};\n", .{use_decl.path.segments});
        },
        .ModuleDeclaration => |mod_decl| {
            try pretty_print_module(writer, mod_decl, level);
        },
        .StaticVariable => |static_var| {
            try pretty_print_static_variable(writer, static_var, level);
        },
        .ConstantVariable => |const_var| {
            try pretty_print_constant_variable(writer, const_var, level);
        },
        .TypeAlias => |type_alias| {
            try pretty_print_type_alias(writer, type_alias, level);
        },
        .MacroDefinition => |macro_def| {
            try pretty_print_macro_definition(writer, macro_def, level);
        },
        .ForeignMod => |foreign_mod| {
            try pretty_print_foreign_mod(writer, foreign_mod, level);
        },
    }
}

// Function to pretty print a Function
fn pretty_print_function(writer: anytype, func: Parser.Function, level: usize) !void {
    try indent(writer, level);
    if (func.is_unsafe) {
        try writer.print("unsafe ", .{});
    }
    try writer.print("fn {s}(", .{func.fname});

    // Print parameters
    for (func.parameters, 0..) |param, i| {
        try writer.print("{s}: ", .{param.name});
        try pretty_print_type_annotation(writer, param.type_annotation);
        if (i < func.parameters.len - 1) {
            try writer.print(", ", .{});
        }
    }
    try writer.print(")", .{});

    // Print return type if exists
    if (func.return_type) |ret_type| {
        try writer.print(" -> ", .{});
        try pretty_print_type_annotation(writer, ret_type);
    }

    try writer.print(" LB\n", .{});
    // Print function body
    try pretty_print_block(writer, func.body, level + 1);
    try indent(writer, level);
    try writer.print("RB\n", .{});
}

// Function to pretty print a Struct
fn pretty_print_struct(writer: anytype, struct_def: Parser.StructDefinition, level: usize) !void {
    try indent(writer, level);
    try writer.print("struct {s}", .{struct_def.name});

    // Print generics if any
    if (struct_def.generics) |generics| {
        try writer.print("<", .{});
        for (generics.params, 0..) |param, i| {
            try writer.print("{s}", .{param.name});
            if (i < generics.params.len - 1) {
                try writer.print(", ", .{});
            }
        }
        try writer.print(">", .{});
    }

    try writer.print(" LB\n", .{});
    // Print struct fields
    for (struct_def.fields) |field| {
        try indent(writer, level + 1);
        try writer.print("{s}: ", .{field.name});
        try pretty_print_type_annotation(writer, field.type_annotation);
        try writer.print(",\n", .{});
    }
    try indent(writer, level);
    try writer.print("RB\n", .{});
}

// Function to pretty print an Enum
fn pretty_print_enum(writer: anytype, enum_def: Parser.EnumDefinition, level: usize) !void {
    try indent(writer, level);
    try writer.print("enum {s}", .{enum_def.name});

    // Print generics if any
    if (enum_def.generics) |generics| {
        try writer.print("<", .{});
        for (generics.params, 0..) |param, i| {
            try writer.print("{s}", .{param.name});
            if (i < generics.params.len - 1) {
                try writer.print(", ", .{});
            }
        }
        try writer.print(">", .{});
    }

    try writer.print(" LB\n", .{});
    // Print enum variants
    for (enum_def.variants) |variant| {
        try indent(writer, level + 1);
        try writer.print("{s}", .{variant.name});
        if (variant.fields) |fields| {
            try writer.print("(", .{});
            for (fields, 0..) |field, i| {
                try pretty_print_type_annotation(writer, field);
                if (i < fields.len - 1) {
                    try writer.print(", ", .{});
                }
            }
            try writer.print(")", .{});
        }
        try writer.print(",\n", .{});
    }
    try indent(writer, level);
    try writer.print("RB\n", .{});
}

// Function to pretty print a Trait
fn pretty_print_trait(writer: anytype, trait_def: Parser.TraitDefinition, level: usize) !void {
    try indent(writer, level);
    try writer.print("trait {s}", .{trait_def.name});

    // Print generics if any
    if (trait_def.generics) |generics| {
        try writer.print("<", .{});
        for (generics.params, 0..) |param, i| {
            try writer.print("{s}", .{param.name});
            if (i < generics.params.len - 1) {
                try writer.print(", ", .{});
            }
        }
        try writer.print(">", .{});
    }

    try writer.print(" LB\n", .{});
    // Print trait items
    for (trait_def.items) |item| {
        switch (item) {
            .Function => |func_sig| {
                try indent(writer, level + 1);
                try writer.print("fn {s}(", .{func_sig.fname});
                for (func_sig.parameters, 0..) |param, i| {
                    try writer.print("{s}: ", .{param.name});
                    try pretty_print_type_annotation(writer, param.type_annotation);
                    if (i < func_sig.parameters.len - 1) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print(")", .{});

                if (func_sig.return_type) |ret_type| {
                    try writer.print(" -> ", .{});
                    try pretty_print_type_annotation(writer, ret_type);
                }

                try writer.print(";\n", .{});
            },
            .TypeAlias => |type_alias| {
                try indent(writer, level + 1);
                try pretty_print_type_alias(writer, type_alias, level + 1);
            },
        }
    }
    try indent(writer, level);
    try writer.print("RB\n", .{});
}

// Function to pretty print an Impl
fn pretty_print_impl(writer: anytype, impl_def: Parser.ImplDefinition, level: usize) !void {
    try indent(writer, level);
    try writer.print("impl ", .{});

    // Print trait if any
    if (impl_def.trait) |trait_name| {
        try writer.print("Trait {s} for ", .{trait_name});
    } else {
        try writer.print("for ", .{});
    }

    // Print type being implemented
    try pretty_print_type_annotation(writer, impl_def.for_type);

    try writer.print(" LB\n", .{});
    // Print impl items
    for (impl_def.items) |item| {
        switch (item) {
            .Function => |func| {
                try pretty_print_function(writer, func, level + 1);
            },
            .Constant => |const_var| {
                try pretty_print_constant_variable(writer, const_var, level + 1);
            },
        }
    }
    try indent(writer, level);
    try writer.print("RB\n", .{});
}

// Function to pretty print a Module
fn pretty_print_module(writer: anytype, mod_decl: Parser.ModuleDeclaration, level: usize) anyerror!void {
    try indent(writer, level);
    try writer.print("mod {s} LB\n", .{mod_decl.name});
    if (mod_decl.items) |items| {
        for (items) |item| {
            try pretty_print_item(writer, item, level + 1);
        }
    }
    try indent(writer, level);
    try writer.print("RB\n", .{});
}

// Function to pretty print a Static Variable
fn pretty_print_static_variable(writer: anytype, static_var: Parser.StaticVariable, level: usize) !void {
    try indent(writer, level);
    try writer.print("static ", .{});
    if (static_var.is_mutable) {
        try writer.print("mut ", .{});
    }
    try writer.print("{s}: ", .{static_var.name});
    try pretty_print_type_annotation(writer, static_var.type_annotation);
    try writer.print(" = ", .{});
    try pretty_print_expression(writer, static_var.value, level);
    try writer.print(";\n", .{});
}

// Function to pretty print a Constant Variable
fn pretty_print_constant_variable(writer: anytype, const_var: Parser.ConstantVariable, level: usize) !void {
    try indent(writer, level);
    try writer.print("const {s}", .{const_var.name});
    if (const_var.type_annotation) |type_ann| {
        try writer.print(": ", .{});
        try pretty_print_type_annotation(writer, type_ann);
    }
    try writer.print(" = ", .{});
    try pretty_print_expression(writer, const_var.value, level);
    try writer.print(";\n", .{});
}

// Function to pretty print a Type Alias
fn pretty_print_type_alias(writer: anytype, type_alias: Parser.TypeAlias, level: usize) !void {
    try indent(writer, level);
    try writer.print("type {s}", .{type_alias.name});

    // Print generics if any
    if (type_alias.generics) |generics| {
        try writer.print("<", .{});
        for (generics.params, 0..) |param, i| {
            try writer.print("{s}", .{param.name});
            if (i < generics.params.len - 1) {
                try writer.print(", ", .{});
            }
        }
        try writer.print(">", .{});
    }

    try writer.print(" = ", .{});
    try pretty_print_type_annotation(writer, type_alias.aliased_type);
    try writer.print(";\n", .{});
}

// Function to pretty print a Macro Definition
fn pretty_print_macro_definition(writer: anytype, macro_def: Parser.MacroDefinition, level: usize) !void {
    try indent(writer, level);
    try writer.print("macro_rules! {s} LB\n", .{macro_def.name});
    for (macro_def.rules) |rule| {
        try indent(writer, level + 1);
        try writer.print("{s} => {s},\n", .{ rule.pattern, rule.replacement });
    }
    try indent(writer, level);
    try writer.print("RB\n", .{});
}

// Function to pretty print a Foreign Module
fn pretty_print_foreign_mod(writer: anytype, foreign_mod: Parser.ForeignMod, level: usize) !void {
    try indent(writer, level);
    try writer.print("extern ", .{});
    if (foreign_mod.abi) |abi| {
        try writer.print("{s} ", .{abi});
    }
    try writer.print("LB\n", .{});
    for (foreign_mod.items) |item| {
        switch (item) {
            .Function => |foreign_func| {
                try indent(writer, level + 1);
                try writer.print("fn {s}(", .{foreign_func.name});
                for (foreign_func.parameters, 0..) |param, i| {
                    try writer.print("{s}: ", .{param.name});
                    try pretty_print_type_annotation(writer, param.type_annotation);
                    if (i < foreign_func.parameters.len - 1) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print(")", .{});
                if (foreign_func.return_type) |ret_type| {
                    try writer.print(" -> ", .{});
                    try pretty_print_type_annotation(writer, ret_type);
                }
                try writer.print(";\n", .{});
            },
            .Static => |foreign_static| {
                try indent(writer, level + 1);
                try writer.print("static ", .{});
                if (foreign_static.is_mutable) {
                    try writer.print("mut ", .{});
                }
                try writer.print("{s}: ", .{foreign_static.name});
                try pretty_print_type_annotation(writer, foreign_static.type_annotation);
                try writer.print(";\n", .{});
            },
        }
    }
    try indent(writer, level);
    try writer.print("RB\n", .{});
}

// Function to pretty print a Block
fn pretty_print_block(writer: anytype, block: Parser.Block, level: usize) anyerror!void {
    for (block.statements) |stmt| {
        try pretty_print_statement(writer, stmt, level);
    }

    if (block.expression) |expr| {
        try indent(writer, level);
        try pretty_print_expression(writer, expr, level);
        try writer.print(";\n", .{});
    }
}

// Function to pretty print a Statement
fn pretty_print_statement(writer: anytype, stmt: Parser.Statement, level: usize) !void {
    switch (stmt) {
        .LetStatement => |let_stmt| {
            try indent(writer, level);
            try writer.print("let ", .{});
            if (let_stmt.is_mutable) {
                try writer.print("mut ", .{});
            }
            switch (let_stmt.pattern) {
                .Identifier => |id| {
                    try writer.print("{s}", .{id});
                },
                .Wildcard => |_| {
                    try writer.print("_", .{});
                },
                .Tuple => |tuple| {
                    try writer.print("(", .{});
                    for (tuple.patterns, 0..) |pat, i| {
                        try pretty_print_pattern(writer, pat, level);
                        if (i < tuple.patterns.len - 1) {
                            try writer.print(", ", .{});
                        }
                    }
                    try writer.print(")", .{});
                },
                .Struct => |struct_pat| {
                    try writer.print("{s} LB ", .{struct_pat.name});
                    for (struct_pat.fields, 0..) |field, i| {
                        try writer.print("{s}: ", .{field.name});
                        try pretty_print_pattern(writer, field.pattern, level);
                        if (i < struct_pat.fields.len - 1) {
                            try writer.print(", ", .{});
                        }
                    }
                    try writer.print(" RB", .{});
                },
            }
            if (let_stmt.type_annotation) |type_ann| {
                try writer.print(": ", .{});
                try pretty_print_type_annotation(writer, type_ann);
            }
            if (let_stmt.value) |val| {
                try writer.print(" = ", .{});
                try pretty_print_expression(writer, val, level);
            }
            try writer.print(";\n", .{});
        },
        .Expression => |expr| {
            try indent(writer, level);
            try pretty_print_expression(writer, expr, level);
            try writer.print(";\n", .{});
        },
        .Item => |item| {
            try pretty_print_item(writer, item, level);
        },
    }
}

// Function to pretty print a Pattern
fn pretty_print_pattern(writer: anytype, pattern: Parser.Pattern, level: usize) !void {
    switch (pattern) {
        .Identifier => |id| {
            try writer.print("{s}", .{id});
        },
        .Wildcard => |_| {
            try writer.print("_", .{});
        },
        .Tuple => |tuple_pat| {
            try writer.print("(", .{});
            for (tuple_pat.patterns, 0..) |pat, i| {
                try pretty_print_pattern(writer, pat, level);
                if (i < tuple_pat.patterns.len - 1) {
                    try writer.print(", ", .{});
                }
            }
            try writer.print(")", .{});
        },
        .Struct => |struct_pat| {
            try writer.print("{s} LB ", .{struct_pat.name});
            for (struct_pat.fields, 0..) |field, i| {
                try writer.print("{s}: ", .{field.name});
                try pretty_print_pattern(writer, field.pattern, level);
                if (i < struct_pat.fields.len - 1) {
                    try writer.print(", ", .{});
                }
            }
            try writer.print(" RB", .{});
        },
    }
}

// Function to pretty print a TypeAnnotation
fn pretty_print_type_annotation(writer: anytype, type_ann: Parser.TypeAnnotation) !void {
    if (type_ann.is_reference) {
        try writer.print("&", .{});
        if (type_ann.is_mutable) {
            try writer.print("mut ", .{});
        }
    }
    switch (type_ann.base_type.*) {
        .I8 => try writer.print("i8", .{}),
        .I16 => try writer.print("i16", .{}),
        .I32 => try writer.print("i32", .{}),
        .I64 => try writer.print("i64", .{}),
        .I128 => try writer.print("i128", .{}),
        .U8 => try writer.print("u8", .{}),
        .U16 => try writer.print("u16", .{}),
        .U32 => try writer.print("u32", .{}),
        .U64 => try writer.print("u64", .{}),
        .U128 => try writer.print("u128", .{}),
        .F32 => try writer.print("f32", .{}),
        .F64 => try writer.print("f64", .{}),
        .Bool => try writer.print("bool", .{}),
        .Char => try writer.print("char", .{}),
        .Str => try writer.print("&str", .{}),
        .Tuple => |tuple| {
            try writer.print("(", .{});
            for (tuple.types, 0..) |_type, i| {
                try pretty_print_type_annotation(writer, _type);
                if (i < tuple.types.len - 1) {
                    try writer.print(", ", .{});
                }
            }
            try writer.print(")", .{});
        },
        .Array => |array| {
            try writer.print("[", .{});
            try pretty_print_type_annotation(writer, array.element_type);
            try writer.print("; {d}]", .{array.size});
        },
        .Function => |func_type| {
            try writer.print("fn(", .{});
            for (func_type.parameters, 0..) |param, i| {
                try pretty_print_type_annotation(writer, param);
                if (i < func_type.parameters.len - 1) {
                    try writer.print(", ", .{});
                }
            }
            try writer.print(")", .{});
            if (func_type.return_type) |ret| {
                try writer.print(" -> ", .{});
                try pretty_print_type_annotation(writer, ret);
            }
        },
        .Generic => |generic_type| {
            try writer.print("{s} <", .{generic_type.name});
            for (generic_type.type_params, 0..) |param, i| {
                try pretty_print_type_annotation(writer, param);
                if (i < generic_type.type_params.len - 1) {
                    try writer.print(", ", .{});
                }
            }
            try writer.print(">", .{});
        },
        .Path => |path| {
            try writer.print("{s}", .{path});
        },
    }
}

// Function to pretty print an Expression
fn pretty_print_expression(writer: anytype, expr: Parser.Expression, level: usize) !void {
    switch (expr) {
        .Assignment => |assign_expr| {
            try writer.print("{s} = ", .{assign_expr.identifier});
            try pretty_print_expression(writer, assign_expr.value, level);
        },
        .Conditional => |cond_expr| {
            try writer.print("if (", .{});
            try pretty_print_expression(writer, cond_expr.condition, level);
            try writer.print(") ", .{});
            try pretty_print_block(writer, cond_expr.then_branch, level);
            if (cond_expr.else_branch) |else_br| {
                try writer.print(" else ", .{});
                try pretty_print_block(writer, else_br, level);
            }
        },
        .Loop => |loop_expr| {
            try writer.print("loop ", .{});

            switch (loop_expr.*) {
                .Loop => |infinite_expr| {
                    try writer.print("Loop", .{});
                    try writer.print("RB", .{});
                    try pretty_print_block(writer, infinite_expr.body, level);
                    try writer.print("RB", .{});
                },
                .While => |while_expr| {
                    try writer.print("while (", .{});
                    try pretty_print_expression(writer, while_expr.condition, level);
                    try writer.print(") ", .{});
                    try writer.print("LB", .{});
                    try pretty_print_block(writer, while_expr.body, level);
                    try writer.print("LB", .{});
                },
                .For => |for_expr| {
                    try writer.print("for (", .{});
                    try pretty_print_pattern(writer, for_expr.iterator, level);
                    try writer.print(" in ", .{});
                    try pretty_print_expression(writer, for_expr.iterable, level);
                    try writer.print(") ", .{});
                    try writer.print("LB", .{});
                    try pretty_print_block(writer, for_expr.body, level);
                    try writer.print("RB", .{});
                },
            }
        },
        .FunctionCall => |func_call| {
            try writer.print("{s}(", .{func_call.function_name});
            for (func_call.arguments, 0..) |arg, i| {
                try pretty_print_expression(writer, arg, level);
                if (i < func_call.arguments.len - 1) {
                    try writer.print(", ", .{});
                }
            }
            try writer.print(")", .{});
        },
        .BinaryOperation => |bin_op| {
            try pretty_print_expression(writer, bin_op.left.*, level);
            try writer.print(" {s} ", .{binary_operator_to_str(bin_op.operator)});
            try pretty_print_expression(writer, bin_op.right.*, level);
        },
        .UnaryOperation => |un_op| {
            try writer.print("{s} ", .{unary_operator_to_str(un_op.operator)});
            try pretty_print_expression(writer, un_op.operand.*, level);
        },
        .Literal => |lit| {
            switch (lit) {
                .Integer => |val| {
                    try writer.print("{d}", .{val});
                },
                .Float => |val| {
                    try writer.print("{d}", .{val});
                },
                .Boolean => |val| {
                    if (val) {
                        try writer.print("true", .{});
                    } else {
                        try writer.print("false", .{});
                    }
                },
                .String => |val| {
                    try writer.print("{s}", .{escape_string(val)});
                },
                .Char => |val| {
                    try writer.print("'{c}'", .{val});
                },
            }
        },
        .Identifier => |id| {
            try writer.print("{s}", .{id});
        },
        .Expression => |expr_ptr| {
            try pretty_print_expression(writer, expr_ptr.*, level);
        },
    }
}

// Helper function to convert BinaryOperator to string
fn binary_operator_to_str(op: Parser.BinaryOperator) []const u8 {
    return switch (op) {
        .Assign => "=",
        .LogicalOr => "||",
        .LogicalAnd => "&&",
        .Equal => "==",
        .NotEqual => "!=",
        .LessThan => "<",
        .GreaterThan => ">",
        .LessThanOrEqual => "<=",
        .GreaterThanOrEqual => ">=",
        .Add => "+",
        .Subtract => "-",
        .Multiply => "*",
        .Divide => "/",
    };
}

// Helper function to convert UnaryOperator to string
fn unary_operator_to_str(op: Parser.UnaryOperator) []const u8 {
    return switch (op) {
        .Negate => "-",
        .LogicalNot => "!",
    };
}

// // Function to pretty print a Trait
// fn pretty_print_trait(writer: anytype, trait_def: Parser.TraitDefinition, level: usize) !void {
//     // Implementation similar to Struct and Enum
// }
//
// // Function to pretty print an Impl
// fn pretty_print_impl(writer: anytype, impl_def: Parser.ImplDefinition, level: usize) !void {
//     // Implementation similar to Function and Struct
// }
//
// // Function to pretty print a Type Alias
// fn pretty_print_type_alias(writer: anytype, type_alias: Parser.TypeAlias, level: usize) !void {
//     // Similar to Function and Struct
// }
//
// // Function to pretty print a Macro Definition
// fn pretty_print_macro_definition(writer: anytype, macro_def: Parser.MacroDefinition, level: usize) !void {
//     // Similar to Function and Struct
// }
//
// // Function to pretty print a Foreign Module
// fn pretty_print_foreign_mod(writer: anytype, foreign_mod: Parser.ForeignMod, level: usize) !void {
//     // Similar to Function and Struct
// }
