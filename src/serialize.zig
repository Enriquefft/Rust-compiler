const std = @import("std");
const Parser = @import("parser.zig");
const parse = Parser.parse;
const Lexer = @import("lexer.zig").Lexer;

// Function to escape strings for JSON and write directly to the writer
fn escape_json_string(writer: anytype, s: []const u8) !void {
    // We'll write each character, escaping special JSON characters as needed
    for (s) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => {
                // Regular character, write as is
                try writer.writeByte(c);
            },
        }
    }
}

pub fn pretty_print_json(allocator: *const std.mem.Allocator, json: []const u8) ![]u8 {

    // Parse the JSON string
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator.*, json, .{});
    defer parsed.deinit();

    // Initialize a buffer to hold the formatted JSON
    var buffer = std.ArrayList(u8).init(allocator.*);
    defer buffer.deinit();

    var writer = buffer.writer();

    // Pretty-print the JSON into the buffer
    try printJsonPretty(&writer, parsed.value, 0);

    // Return the formatted JSON as a byte array
    return buffer.toOwnedSlice();
}

fn write_indent(writer: anytype, indent: usize) !void {
    const indentStr = "  "; // Two spaces per indentation level
    for (0..indent) |_| {
        try writer.print("{s}", .{indentStr});
    }
}

fn printJsonPretty(writer: anytype, value: std.json.Value, indent: usize) !void {
    const newIndent:usize = indent + 1;

    switch (value) {
        .null => try writer.print("null", .{}),
        .bool => try writer.print("{s}", .{if (value.bool == true) "true" else "false"}),
        .string => try writer.print("\"{s}\"", .{value.string}),
        .integer => try writer.print("{d}", .{value.integer}),
        .float => try writer.print("{d}", .{value.float}),
        .number_string => try writer.print("{s}", .{value.number_string}),
        .array => {
            const items = value.array.items; // Array of std.json.Value
            if (items.len == 0) {
                try writer.print("[]", .{});
                return;
            }
            try writer.print("[\n", .{});
            var first = true;
            for (items) |item| {
                if (!first) {
                    try writer.print(",\n", .{});
                } else {
                    first = false;
                }
                try write_indent(writer, newIndent);
                try printJsonPretty(writer, item, newIndent);
            }
            try writer.print("\n", .{});
            try write_indent(writer, indent);
            try writer.print("]", .{});

        },
        .object => {
            const obj = value.object; // std.StringHashMap(std.json.Value)
            if (obj.count() == 0) {
                try writer.print("{{}}", .{});
                return;
            }
            try writer.print("{{\n", .{});
            var it = obj.iterator();
            var first = true;
            while (it.next()) |entry| {
                if (!first) {
                    try writer.print(",\n", .{});
                } else {
                    first = false;
                }
                try write_indent(writer, newIndent);
                try writer.print("\"{s}\": ", .{entry.key_ptr.*});


                try printJsonPretty(writer, entry.value_ptr.*, newIndent);
            }
            try writer.print("\n", .{});
            try write_indent(writer, indent);
            try writer.print("}}", .{});

        },
    }
}

// Function to serialize the entire Program
pub fn serialize_program(writer: anytype, program: Parser.Program) !void {
    try writer.writeByte('{');
    try writer.writeAll("\"items\": [");
    for (program.items, 0..) |item, index| {
        try serialize_item(writer, item);
        if (index < program.items.len - 1) {
            try writer.writeByte(',');
        }
    }
    try writer.writeAll("]}");
}

// Function to serialize an Item
fn serialize_item(writer: anytype, item: Parser.Item) anyerror!void {
    try writer.writeByte('{');

    // Serialize docstring if present
    if (item.docstring) |doc| {
        try writer.writeAll("\"docstring\": ");
        try writer.writeByte('"');
        try escape_json_string(writer, doc);
        try writer.writeByte('"');
        try writer.writeByte(',');
    }

    try writer.writeAll("\"content\": {");

    // Determine the variant of ItemContent
    const content_variant = @tagName(item.content);

    try writer.writeAll("\"type\": \"");
    try writer.print("{s}", .{content_variant});
    try writer.writeAll("\",");

    switch (item.content) {
        .Function => |func| {
            try writer.writeAll("\"value\": ");
            try serialize_function(writer, func);
        },
        // Handle other variants similarly...
        else => {
            // Implement serialization for other ItemContent variants
            // For brevity, not all variants are implemented here
            try writer.writeAll("\"value\": null");
        },
    }

    try writer.writeByte('}'); // Close "content" object
    try writer.writeByte('}'); // Close Item object
}

// Function to serialize a Function
fn serialize_function(writer: anytype, func: Parser.Function) !void {
    try writer.writeByte('{');
    try writer.print("\"is_unsafe\": {},", .{func.is_unsafe});

    try writer.writeAll("\"fname\": \"");
    try escape_json_string(writer, func.fname);
    try writer.writeAll("\",");

    // Serialize generics if present
    if (func.generics) |generics| {
        try writer.writeAll("\"generics\": ");
        try serialize_generics(writer, generics);
        try writer.writeByte(',');
    } else {
        try writer.writeAll("\"generics\": null,");
    }

    // Serialize parameters
    try writer.writeAll("\"parameters\": [");
    for (func.parameters, 0..) |param, i| {
        try serialize_parameter(writer, param);
        if (i < func.parameters.len - 1) {
            try writer.writeByte(',');
        }
    }
    try writer.writeAll("],");

    // Serialize return_type if present
    if (func.return_type) |ret_type| {
        try writer.writeAll("\"return_type\": ");
        try serialize_type_annotation(writer, ret_type);
        try writer.writeByte(',');
    } else {
        try writer.writeAll("\"return_type\": null,");
    }

    // Serialize body
    try writer.writeAll("\"body\": ");
    try serialize_block(writer, func.body);

    try writer.writeByte('}'); // Close Function object
}

// Function to serialize a Parameter
fn serialize_parameter(writer: anytype, param: Parser.Parameter) !void {
    try writer.writeByte('{');
    try writer.writeAll("\"name\": \"");
    try escape_json_string(writer, param.name);
    try writer.writeAll("\",");
    try writer.writeAll("\"type_annotation\": ");
    try serialize_type_annotation(writer, param.type_annotation);
    try writer.writeByte('}');
}

// Function to serialize Generics
fn serialize_generics(writer: anytype, generics: Parser.Generics) !void {
    try writer.writeByte('{');
    try writer.writeAll("\"params\": [");
    for (generics.params, 0..) |param, i| {
        try serialize_generic_param(writer, param);
        if (i < generics.params.len - 1) {
            try writer.writeByte(',');
        }
    }
    try writer.writeAll("]}");
}

// Function to serialize a GenericParam
fn serialize_generic_param(writer: anytype, param: Parser.GenericParam) !void {
    try writer.writeByte('{');
    try writer.writeAll("\"name\": \"");
    try escape_json_string(writer, param.name);
    try writer.writeAll("\",");

    // Serialize bounds
    try writer.writeAll("\"bounds\": [");
    for (param.bounds, 0..) |bound, i| {
        try writer.writeByte('"');
        try writer.writeByte(bound);
        try writer.writeByte('"');
        if (i < param.bounds.len - 1) {
            try writer.writeByte(',');
        }
    }
    try writer.writeAll("]");
    try writer.writeByte('}');
}

// Function to serialize TypeAnnotation
fn serialize_type_annotation(writer: anytype, type_ann: Parser.TypeAnnotation) anyerror!void {
    try writer.writeByte('{');
    try writer.print("\"is_reference\": {},", .{type_ann.is_reference});
    try writer.print("\"is_mutable\": {},", .{type_ann.is_mutable});
    try writer.writeAll("\"base_type\": ");
    try serialize_base_type(writer, type_ann.base_type.*);
    try writer.writeByte('}');
}

// Function to serialize BaseType
fn serialize_base_type(writer: anytype, base_type: Parser.BaseType) !void {
    try writer.writeByte('{');
    const type_name = @tagName(base_type);

    try writer.writeAll("\"type\": \"");
    try writer.print("{s}", .{type_name});
    try writer.writeByte('"');

    switch (base_type) {
        .Tuple => |tuple_type| {
            try writer.writeByte(',');
            try writer.writeAll("\"types\": [");
            for (tuple_type.types, 0..) |type_ann, i| {
                try serialize_type_annotation(writer, type_ann);
                if (i < tuple_type.types.len - 1) {
                    try writer.writeByte(',');
                }
            }
            try writer.writeByte(']');
        },
        .Array => |array_type| {
            try writer.writeByte(',');
            try writer.writeAll("\"element_type\": ");
            try serialize_type_annotation(writer, array_type.element_type.*);
            try writer.writeByte(',');
            try writer.print("\"size\": {}", .{array_type.size});
        },
        .Function => |func_type| {
            try writer.writeByte(',');
            try writer.writeAll("\"parameters\": [");
            for (func_type.parameters, 0..) |param, i| {
                try serialize_type_annotation(writer, param);
                if (i < func_type.parameters.len - 1) {
                    try writer.writeByte(',');
                }
            }
            try writer.writeByte(']');
            if (func_type.return_type) |ret_type| {
                try writer.writeByte(',');
                try writer.writeAll("\"return_type\": ");
                try serialize_type_annotation(writer, ret_type.*);
            } else {
                try writer.writeByte(',');
                try writer.writeAll("\"return_type\": null");
            }
        },
        .Generic => |generic_type| {
            try writer.writeByte(',');
            try writer.writeAll("\"name\": \"");
            try escape_json_string(writer, generic_type.name);
            try writer.writeAll("\",");

            try writer.writeAll("\"type_params\": [");
            for (generic_type.type_params, 0..) |type_ann, i| {
                try serialize_type_annotation(writer, type_ann);
                if (i < generic_type.type_params.len - 1) {
                    try writer.writeByte(',');
                }
            }
            try writer.writeByte(']');
        },
        .Path => |path| {
            try writer.writeByte(',');
            try writer.writeAll("\"path\": \"");
            try escape_json_string(writer, path);
            try writer.writeByte('"');
        },
        else => {},
    }
    try writer.writeByte('}');
}

// Function to serialize a Block
fn serialize_block(writer: anytype, block: Parser.Block) !void {
    try writer.writeByte('{');
    try writer.writeAll("\"statements\": [");
    for (block.statements, 0..) |stmt, i| {
        try serialize_statement(writer, stmt);
        if (i < block.statements.len - 1) {
            try writer.writeByte(',');
        }
    }
    try writer.writeAll("],");
    if (block.expression) |expr| {
        try writer.writeAll("\"expression\": ");
        try serialize_expression(writer, expr);
    } else {
        try writer.writeAll("\"expression\": null");
    }
    try writer.writeByte('}');
}

// Function to serialize a Statement
fn serialize_statement(writer: anytype, stmt: Parser.Statement) !void {
    try writer.writeByte('{');
    const stmt_type = @tagName(stmt);

    // Convert comptime string to runtime slice

    try writer.writeAll("\"type\": \"");
    try writer.print("{s}", .{stmt_type});
    try writer.writeAll("\",");

    switch (stmt) {
        .LetStatement => |let_stmt| {
            try writer.writeAll("\"value\": ");
            try serialize_let_statement(writer, let_stmt);
        },
        .Expression => |expr| {
            try writer.writeAll("\"value\": ");
            try serialize_expression(writer, expr);
        },
        .Item => |item| {
            try writer.writeAll("\"value\": ");
            try serialize_item(writer, item);
        },
    }

    try writer.writeByte('}');
}

// Function to serialize a LetStatement
fn serialize_let_statement(writer: anytype, let_stmt: Parser.LetStatement) !void {
    try writer.writeByte('{');
    try writer.print("\"is_mutable\": {},", .{let_stmt.is_mutable});

    // Serialize pattern
    try writer.writeAll("\"pattern\": ");
    try serialize_pattern(writer, let_stmt.pattern);
    try writer.writeByte(',');

    // Serialize type_annotation if present
    if (let_stmt.type_annotation) |type_ann| {
        try writer.writeAll("\"type_annotation\": ");
        try serialize_type_annotation(writer, type_ann);
        try writer.writeByte(',');
    } else {
        try writer.writeAll("\"type_annotation\": null,");
    }

    // Serialize value if present
    if (let_stmt.value) |value| {
        try writer.writeAll("\"value\": ");
        try serialize_expression(writer, value);
    } else {
        try writer.writeAll("\"value\": null");
    }

    try writer.writeByte('}');
}

// Function to serialize a Pattern
fn serialize_pattern(writer: anytype, pattern: Parser.Pattern) anyerror!void {
    try writer.writeByte('{');
    const pattern_type = @tagName(pattern);

    // Convert comptime string to runtime slice

    try writer.writeAll("\"type\": \"");
    try writer.print("{s}", .{pattern_type});
    try writer.writeAll("\",");

    switch (pattern) {
        .Identifier => |id| {
            try writer.writeAll("\"value\": \"");
            try escape_json_string(writer, id);
            try writer.writeByte('"');
        },
        .Wildcard => {
            try writer.writeAll("\"value\": null");
        },
        .Tuple => |tuple_pat| {
            try writer.writeAll("\"patterns\": [");
            for (tuple_pat.patterns, 0..) |pat, i| {
                try serialize_pattern(writer, pat);
                if (i < tuple_pat.patterns.len - 1) {
                    try writer.writeByte(',');
                }
            }
            try writer.writeByte(']');
        },
        .Struct => |struct_pat| {
            try writer.writeAll("\"name\": \"");
            try escape_json_string(writer, struct_pat.name);
            try writer.writeAll("\",");

            try writer.writeAll("\"fields\": [");
            for (struct_pat.fields, 0..) |field, i| {
                try serialize_struct_pattern_field(writer, field);
                if (i < struct_pat.fields.len - 1) {
                    try writer.writeByte(',');
                }
            }
            try writer.writeByte(']');
        },
    }

    try writer.writeByte('}');
}

// Function to serialize a StructPatternField
fn serialize_struct_pattern_field(writer: anytype, field: Parser.StructPatternField) !void {
    try writer.writeByte('{');
    try writer.writeAll("\"name\": \"");
    try escape_json_string(writer, field.name);
    try writer.writeAll("\",");

    try writer.writeAll("\"pattern\": ");
    try serialize_pattern(writer, field.pattern);
    try writer.writeByte('}');
}

// Function to serialize an Expression
fn serialize_expression(writer: anytype, expr: *const Parser.Expression) !void {
    try writer.writeByte('{');
    const expr_type = @tagName(expr.*);

    // Convert comptime string to runtime slice

    try writer.writeAll("\"type\": \"");
    try writer.print("{s}", .{expr_type});
    try writer.writeAll("\",");

    switch (expr.*) {
        .Assignment => |assign_expr| {
            try writer.writeAll("\"identifier\": \"");
            try escape_json_string(writer, assign_expr.identifier);
            try writer.writeAll("\",");

            try writer.writeAll("\"value\": ");
            try serialize_expression(writer, assign_expr.value);
        },
        .FunctionCall => |func_call| {
            try writer.writeAll("\"function_name\": \"");
            try escape_json_string(writer, func_call.function_name);
            try writer.writeAll("\",");

            try writer.writeAll("\"arguments\": [");
            for (func_call.arguments, 0..) |arg, i| {
                try serialize_expression(writer, arg);
                if (i < func_call.arguments.len - 1) {
                    try writer.writeByte(',');
                }
            }
            try writer.writeByte(']');
        },
        .Literal => |lit| {
            switch (lit) {
                .Integer => |val| {
                    try writer.print("\"value\": {}", .{val});
                },
                .Float => |val| {
                    try writer.print("\"value\": {}", .{val});
                },
                .Boolean => |val| {
                    try writer.print("\"value\": {}", .{val});
                },
                .String => |val| {
                    try writer.writeAll("\"value\": \"");
                    try escape_json_string(writer, val);
                    try writer.writeByte('"');
                },
                .Char => |val| {
                    try writer.writeAll("\"value\": \"");
                    try writer.writeByte(val);
                    try writer.writeByte('"');
                },
            }
        },
        .Identifier => |id| {
            try writer.writeAll("\"value\": \"");
            try escape_json_string(writer, id);
            try writer.writeByte('"');
        },

        .BinaryOperation => |bin_op| {
            try writer.writeAll("\"operator\": \"");
            try escape_json_string(writer, @tagName(bin_op.operator));
            try writer.writeAll("\",");
            try writer.writeAll("\"left\": ");
            try serialize_expression(writer, bin_op.left);
            try writer.writeByte(',');
            try writer.writeAll("\"right\": ");
            try serialize_expression(writer, bin_op.right);
        },


        else => {
            // Implement serialization for other Expression variants
            // For brevity, not all variants are implemented here
            try writer.writeAll("\"value\": null");
        },
    }

    try writer.writeByte('}');
}

// ... Implement other serialization functions as needed ...
