const std = @import("std");
const tokens = @import("tokens.zig");
const ast = @import("ast.zig");
const diag = @import("../diag/diagnostics.zig");
const source_map = @import("../diag/source_map.zig");
const Token = tokens.Token;
const TokenKind = tokens.TokenKind;
const Span = source_map.Span;

pub fn parseCrate(arena: std.mem.Allocator, input: []const Token, diagnostics: *diag.Diagnostics) ast.Crate {
    var parser = Parser{
        .arena = arena,
        .diagnostics = diagnostics,
        .tokens = input,
        .pos = 0,
    };
    return parser.parseCrate();
}

const Parser = struct {
    arena: std.mem.Allocator,
    diagnostics: *diag.Diagnostics,
    tokens: []const Token,
    pos: usize,
    /// When true, struct literals (e.g., `Foo { x: 1 }`) are not parsed at the expression level.
    /// This is used in contexts where a block is required after an expression, such as
    /// `if cond { }`, `while cond { }`, and `for x in iter { }`.
    no_struct_literal: bool = false,

    fn parseCrate(self: *Parser) ast.Crate {
        var items = std.ArrayListUnmanaged(ast.Item){};

        while (!self.isAtEnd()) {
            if (self.peekKind() == .Semicolon) {
                _ = self.advance();
                continue;
            }
            if (self.parseItem()) |item| {
                items.append(self.arena, item) catch {};
            } else {
                self.synchronize();
            }
        }

        const owned = items.toOwnedSlice(self.arena) catch @panic("out of memory parsing crate");
        return .{ .items = owned };
    }

    fn parseItem(self: *Parser) ?ast.Item {
        const tok = self.peek();

        switch (tok.kind) {
            .KwFn => return self.parseFnItem(),
            .KwStruct => return self.parseStructItem(),
            .KwImpl => return self.parseImplItem(),
            .KwType => return self.parseTypeAliasItem(),
            .Semicolon => {
                _ = self.advance();
                return ast.Item{ .tag = .Empty, .span = tok.span, .data = .Empty };
            },
            else => {
                self.reportError(tok.span, "expected item");
                return null;
            },
        }
    }

    fn parseFnItem(self: *Parser) ?ast.Item {
        const fn_token = self.expectConsume(.KwFn, "expected 'fn'") orelse return null;
        const name_tok = self.expectConsume(.Identifier, "expected function name") orelse return null;
        const name = self.makeIdent(name_tok);
        const generics = self.parseGenericParams();
        const params = self.parseParamList();
        const ret_ty = if (self.match(.Arrow)) self.parseType() else null;
        const body = self.parseBlock() orelse return null;
        const span = Span{ .file_id = fn_token.span.file_id, .start = fn_token.span.start, .end = body.span.end };

        return ast.Item{
            .tag = .Fn,
            .span = span,
            .data = .{ .Fn = .{
                .name = name,
                .generics = generics,
                .params = params,
                .return_type = ret_ty,
                .body = body,
                .span = span,
            } },
        };
    }

    fn parseStructItem(self: *Parser) ?ast.Item {
        const kw = self.expectConsume(.KwStruct, "expected 'struct'") orelse return null;
        const name_tok = self.expectConsume(.Identifier, "expected struct name") orelse return null;
        const name = self.makeIdent(name_tok);
        const generics = self.parseGenericParams();
        _ = self.expectConsume(.LBrace, "expected '{' after struct name") orelse return null;

        var fields = std.ArrayListUnmanaged(ast.Field){};
        while (!self.check(.RBrace) and !self.isAtEnd()) {
            const field_name_tok = self.expectConsume(.Identifier, "expected field name") orelse break;
            _ = self.expectConsume(.Colon, "expected ':' after field name") orelse break;
            const ty = self.parseType() orelse break;
            const field_span = Span{ .file_id = field_name_tok.span.file_id, .start = field_name_tok.span.start, .end = ty.span.end };
            fields.append(self.arena, .{ .name = self.makeIdent(field_name_tok), .ty = ty, .span = field_span }) catch {};
            if (self.match(.Comma)) {} else break;
        }
        const rbrace = self.expectConsume(.RBrace, "expected '}' to close struct") orelse return null;
        const span = Span{ .file_id = kw.span.file_id, .start = kw.span.start, .end = rbrace.span.end };
        return ast.Item{ .tag = .Struct, .span = span, .data = .{ .Struct = .{ .name = name, .generics = generics, .fields = fields.toOwnedSlice(self.arena) catch @panic("out of memory"), .span = span } } };
    }

    fn parseImplItem(self: *Parser) ?ast.Item {
        const kw = self.expectConsume(.KwImpl, "expected 'impl'") orelse return null;
        const generics = self.parseGenericParams();
        const target_ty = self.parseType() orelse return null;
        _ = self.expectConsume(.LBrace, "expected '{' in impl body") orelse return null;

        var methods = std.ArrayListUnmanaged(ast.FnItem){};
        while (!self.check(.RBrace) and !self.isAtEnd()) {
            if (self.check(.KwFn)) {
                if (self.parseFnItem()) |fn_item| {
                    methods.append(self.arena, fn_item.data.Fn) catch {};
                } else {
                    self.synchronize();
                }
            } else if (self.match(.Semicolon)) {
                continue;
            } else {
                self.reportError(self.peek().span, "expected function in impl");
                self.synchronize();
            }
        }

        const rbrace = self.expectConsume(.RBrace, "expected '}' to close impl") orelse return null;
        const span = Span{ .file_id = kw.span.file_id, .start = kw.span.start, .end = rbrace.span.end };
        return ast.Item{ .tag = .Impl, .span = span, .data = .{ .Impl = .{ .generics = generics, .target = target_ty, .methods = methods.toOwnedSlice(self.arena) catch @panic("out of memory"), .span = span } } };
    }

    fn parseTypeAliasItem(self: *Parser) ?ast.Item {
        const kw = self.expectConsume(.KwType, "expected 'type'") orelse return null;
        const name_tok = self.expectConsume(.Identifier, "expected type alias name") orelse return null;
        const name = self.makeIdent(name_tok);
        const generics = self.parseGenericParams();
        _ = self.expectConsume(.Eq, "expected '=' in type alias") orelse return null;
        const ty = self.parseType() orelse return null;
        const semi = self.expectConsume(.Semicolon, "expected ';' after type alias") orelse return null;
        const span = Span{ .file_id = kw.span.file_id, .start = kw.span.start, .end = semi.span.end };
        return ast.Item{ .tag = .TypeAlias, .span = span, .data = .{ .TypeAlias = .{ .name = name, .generics = generics, .aliased_type = ty, .span = span } } };
    }

    fn parseGenericParams(self: *Parser) []ast.Identifier {
        if (!self.match(.Lt)) return &[_]ast.Identifier{};
        var params = std.ArrayListUnmanaged(ast.Identifier){};
        while (!self.check(.Gt) and !self.isAtEnd()) {
            const tok = self.expectConsume(.Identifier, "expected generic parameter") orelse break;
            params.append(self.arena, self.makeIdent(tok)) catch {};
            if (!self.match(.Comma)) break;
        }
        _ = self.expectConsume(.Gt, "expected '>' after generics") orelse {};
        return params.toOwnedSlice(self.arena) catch @panic("out of memory");
    }

    fn parseParamList(self: *Parser) []ast.Param {
        _ = self.expectConsume(.LParen, "expected '(' after function name") orelse return &[_]ast.Param{};
        var params = std.ArrayListUnmanaged(ast.Param){};
        if (!self.check(.RParen)) {
            while (true) {
                if (self.check(.KwSelf) or self.checkSelfRef()) {
                    const param = self.parseSelfParam();
                    params.append(self.arena, param) catch {};
                } else {
                    _ = self.match(.KwMut);
                    const pattern = self.parsePattern() orelse break;
                    const ty = if (self.match(.Colon)) self.parseType() else null;
                    const span_end = if (ty) |t| t.span.end else pattern.span.end;
                    const param_span = Span{ .file_id = pattern.span.file_id, .start = pattern.span.start, .end = span_end };
                    params.append(self.arena, .{ .pattern = pattern, .ty = ty, .span = param_span, .kind = .Normal }) catch {};
                }
                if (!self.match(.Comma)) break;
            }
        }
        _ = self.expectConsume(.RParen, "expected ')' after parameters") orelse {};
        return params.toOwnedSlice(self.arena) catch @panic("out of memory");
    }

    fn parseSelfParam(self: *Parser) ast.Param {
        var span_start: usize = self.peek().span.start;
        const amp = self.match(.Amp);
        if (amp) span_start = self.previous().span.start;
        const mut = self.match(.KwMut);
        const self_tok = self.expectConsume(.KwSelf, "expected 'self'") orelse self.peek();
        const span = Span{ .file_id = self_tok.span.file_id, .start = span_start, .end = self_tok.span.end };
        const kind: ast.Param.Kind = if (amp and mut)
            .SelfRefMut
        else if (amp)
            .SelfRef
        else
            .SelfValue;
        return .{ .pattern = .{ .tag = .Identifier, .span = self_tok.span, .data = .{ .Identifier = self.makeIdent(self_tok) } }, .ty = null, .span = span, .kind = kind };
    }

    fn parsePattern(self: *Parser) ?ast.Pattern {
        const tok = self.peek();
        switch (tok.kind) {
            .Identifier => {
                _ = self.advance();
                if (std.mem.eql(u8, tok.lexeme, "_")) {
                    return ast.Pattern{ .tag = .Wildcard, .span = tok.span, .data = .Wildcard };
                }
                return ast.Pattern{ .tag = .Identifier, .span = tok.span, .data = .{ .Identifier = self.makeIdent(tok) } };
            },
            else => {
                self.reportError(tok.span, "expected pattern");
                return null;
            },
        }
    }

    fn parseType(self: *Parser) ?ast.Type {
        const tok = self.peek();
        switch (tok.kind) {
            .Star => {
                const star = self.advance();
                // Handle *mut T or *const T - if neither, treat as mutable by default (like Rust's raw pointers)
                var is_mut = false;
                if (self.match(.KwMut)) {
                    is_mut = true;
                } else if (self.match(.KwConst)) {
                    is_mut = false;
                }
                const child = self.parseType() orelse return null;
                const span = Span{ .file_id = star.span.file_id, .start = star.span.start, .end = child.span.end };
                const node = ast.Type{ .tag = .Pointer, .span = span, .data = .{ .Pointer = .{ .is_mut = is_mut, .child = self.copyType(child) } } };
                return node;
            },
            .Amp => {
                const amp = self.advance();
                const mut_tok = self.match(.KwMut);
                const child = self.parseType() orelse return null;
                const span = Span{ .file_id = amp.span.file_id, .start = amp.span.start, .end = child.span.end };
                const node = ast.Type{ .tag = .Reference, .span = span, .data = .{ .Reference = .{ .is_mut = mut_tok, .child = self.copyType(child) } } };
                return node;
            },
            .KwFn => {
                const start = self.advance();
                _ = self.expectConsume(.LParen, "expected '(' after 'fn'") orelse return null;
                var params = std.ArrayListUnmanaged(ast.Type){};
                if (!self.check(.RParen)) {
                    while (true) {
                        const ty = self.parseType() orelse break;
                        params.append(self.arena, ty) catch {};
                        if (!self.match(.Comma)) break;
                    }
                }
                _ = self.expectConsume(.RParen, "expected ')' in function type") orelse {};
                _ = self.expectConsume(.Arrow, "expected '->' in function type") orelse return null;
                const ret = self.parseType() orelse return null;
                const span = Span{ .file_id = start.span.file_id, .start = start.span.start, .end = ret.span.end };
                return ast.Type{ .tag = .Function, .span = span, .data = .{ .Function = .{ .params = params.toOwnedSlice(self.arena) catch @panic("out of memory"), .return_type = self.copyType(ret) } } };
            },
            .LBracket => {
                const lbrack = self.advance();
                const elem_ty = self.parseType() orelse return null;
                _ = self.expectConsume(.Semicolon, "expected ';' in array type") orelse return null;
                const size_expr = self.parseExpr() orelse return null;
                const rbrack = self.expectConsume(.RBracket, "expected ']' in array type") orelse return null;
                const span = Span{ .file_id = lbrack.span.file_id, .start = lbrack.span.start, .end = rbrack.span.end };
                return ast.Type{ .tag = .Array, .span = span, .data = .{ .Array = .{ .element_type = self.copyType(elem_ty), .size_expr = size_expr } } };
            },
            else => {
                if (self.parsePath()) |p| {
                    const prim = primFromPath(p);
                    if (prim) |pr| {
                        return ast.Type{ .tag = .Primitive, .span = p.span, .data = .{ .Primitive = pr } };
                    }
                    return ast.Type{ .tag = .Path, .span = p.span, .data = .{ .Path = p } };
                }
                self.reportError(tok.span, "expected type");
                return null;
            },
        }
    }

    fn primFromPath(path: ast.Path) ?ast.Primitive {
        if (path.segments.len != 1) return null;
        const name = path.segments[0].name;
        return std.meta.stringToEnum(ast.Primitive, name);
    }

    fn parsePath(self: *Parser) ?ast.Path {
        if (!self.check(.Identifier) and !self.check(.KwSelf)) return null;
        var segments = std.ArrayListUnmanaged(ast.Identifier){};
        const first = self.advance();
        segments.append(self.arena, self.makeIdent(first)) catch {};
        while (self.match(.DoubleColon)) {
            const seg_tok = self.expectConsume(.Identifier, "expected path segment") orelse break;
            segments.append(self.arena, self.makeIdent(seg_tok)) catch {};
        }

        var args = std.ArrayListUnmanaged(ast.Type){};
        if (self.match(.Lt)) {
            while (!self.check(.Gt) and !self.isAtEnd()) {
                const arg = self.parseType() orelse break;
                args.append(self.arena, arg) catch {};
                if (!self.match(.Comma)) break;
            }
            _ = self.expectConsume(.Gt, "expected '>' after generic arguments") orelse {};
        }

        const last_span = segments.items[segments.items.len - 1].span;
        const end_pos: usize = if (args.items.len > 0) args.items[args.items.len - 1].span.end else last_span.end;
        const span = Span{ .file_id = last_span.file_id, .start = first.span.start, .end = end_pos };
        return .{ .segments = segments.toOwnedSlice(self.arena) catch @panic("out of memory"), .generic_args = args.toOwnedSlice(self.arena) catch @panic("out of memory"), .span = span };
    }

    fn parseBlock(self: *Parser) ?ast.Block {
        const lbrace = self.expectConsume(.LBrace, "expected '{'") orelse return null;
        var stmts = std.ArrayListUnmanaged(ast.Stmt){};
        var result_expr: ?*ast.Expr = null;
        while (!self.check(.RBrace) and !self.isAtEnd()) {
            if (self.match(.Semicolon)) {
                stmts.append(self.arena, .{ .tag = .Empty, .span = lbrace.span, .data = .Empty }) catch {};
                continue;
            }

            if (self.check(.KwLet)) {
                if (self.parseLetStmt()) |stmt| stmts.append(self.arena, stmt) catch {} else self.synchronize();
                continue;
            }
            if (self.check(.KwWhile)) {
                if (self.parseWhileStmt()) |stmt| stmts.append(self.arena, stmt) catch {} else self.synchronize();
                continue;
            }
            if (self.check(.KwIf)) {
                if (self.parseIfStmt()) |stmt| stmts.append(self.arena, stmt) catch {} else self.synchronize();
                continue;
            }
            if (self.check(.KwFor)) {
                if (self.parseForStmt()) |stmt| stmts.append(self.arena, stmt) catch {} else self.synchronize();
                continue;
            }
            if (self.check(.KwReturn)) {
                if (self.parseReturnStmt()) |stmt| stmts.append(self.arena, stmt) catch {} else self.synchronize();
                continue;
            }

            // If we see a '{' here, we treat the whole nested block as a statement,
            // not as a tail expression of the enclosing block.
            if (self.check(.LBrace)) {
                if (self.parseBlock()) |blk| {
                    const expr_val = ast.Expr{
                        .tag = .Block,
                        .span = blk.span,
                        .data = .{ .Block = blk },
                    };
                    stmts.append(self.arena, .{
                        .tag = .Expr,
                        .span = blk.span,
                        .data = .{ .Expr = .{ .expr = expr_val } },
                    }) catch {};
                    // Optional trailing semicolon after the block, e.g. "{ ... };"
                    _ = self.match(.Semicolon);
                } else {
                    self.synchronize();
                }
                continue;
            }

            // Generic expression in statement or tail position
            if (self.isStartOfExpr()) {
                if (self.parseExpr()) |expr| {
                    if (self.match(.Semicolon)) {
                        // Expression statement: `expr;`
                        stmts.append(self.arena, .{
                            .tag = .Expr,
                            .span = expr.span,
                            .data = .{ .Expr = .{ .expr = expr.* } },
                        }) catch {};
                    } else {
                        // Tail expression of the block (must be last)
                        result_expr = expr;
                        break;
                    }
                } else {
                    self.synchronize();
                }
            } else {
                self.reportError(self.peek().span, "unexpected token in block");
                self.synchronize();
            }
        }

        const rbrace = self.expectConsume(.RBrace, "expected '}' to close block") orelse return null;
        const span = Span{
            .file_id = lbrace.span.file_id,
            .start = lbrace.span.start,
            .end = rbrace.span.end,
        };

        return ast.Block{
            .stmts = stmts.toOwnedSlice(self.arena) catch @panic("out of memory"),
            .result = result_expr,
            .span = span,
        };
    }

    fn parseLetStmt(self: *Parser) ?ast.Stmt {

        const kw = self.expectConsume(.KwLet, "expected 'let'") orelse return null;
        const is_mut = self.match(.KwMut);
        const pattern = self.parsePattern() orelse return null;
        var ty: ?ast.Type = null;
        if (self.match(.Colon)) {
            ty = self.parseType();
        }
        var value: ?*ast.Expr = null;
        if (self.match(.Eq)) {
            value = self.parseExpr();
        }
        const semi = self.expectConsume(.Semicolon, "expected ';' after let statement") orelse return null;
        const span = Span{ .file_id = kw.span.file_id, .start = kw.span.start, .end = semi.span.end };
        return ast.Stmt{ .tag = .Let, .span = span, .data = .{ .Let = .{ .mutable = is_mut, .pattern = pattern, .ty = ty, .value = value } } };
    }

    fn parseWhileStmt(self: *Parser) ?ast.Stmt {
        const kw = self.expectConsume(.KwWhile, "expected 'while'") orelse return null;
        const cond = self.parseExprNoStructLiteral() orelse return null;
        const body = self.parseBlock() orelse return null;
        const span = Span{ .file_id = kw.span.file_id, .start = kw.span.start, .end = body.span.end };
        return ast.Stmt{ .tag = .While, .span = span, .data = .{ .While = .{ .condition = cond, .body = body } } };
    }

    fn parseIfStmt(self: *Parser) ?ast.Stmt {
        if (self.parseIfExpr()) |expr| {
            return ast.Stmt{ .tag = .If, .span = expr.span, .data = .{ .If = expr.data.If } };
        }
        return null;
    }

    fn parseForStmt(self: *Parser) ?ast.Stmt {
        const kw = self.expectConsume(.KwFor, "expected 'for'") orelse return null;
        const pat = self.parsePattern() orelse return null;
        _ = self.expectConsume(.KwIn, "expected 'in' after pattern") orelse return null;
        const iter = self.parseExprNoStructLiteral() orelse return null;
        const body = self.parseBlock() orelse return null;
        const span = Span{ .file_id = kw.span.file_id, .start = kw.span.start, .end = body.span.end };
        return ast.Stmt{ .tag = .For, .span = span, .data = .{ .For = .{ .pattern = pat, .iterator = iter, .body = body } } };
    }

    fn parseReturnStmt(self: *Parser) ?ast.Stmt {
        const kw = self.expectConsume(.KwReturn, "expected 'return'") orelse return null;
        var value: ?*ast.Expr = null;
        if (!self.check(.Semicolon)) {
            value = self.parseExpr();
        }
        const semi = self.expectConsume(.Semicolon, "expected ';' after return") orelse return null;
        const span = Span{ .file_id = kw.span.file_id, .start = kw.span.start, .end = semi.span.end };
        return ast.Stmt{ .tag = .Return, .span = span, .data = .{ .Return = .{ .value = value } } };
    }

    /// Main entry point for parsing expressions. Delegates to the assignment parser.
    fn parseExpr(self: *Parser) ?*ast.Expr {

        // unsafe block
        if (self.match(.KwUnsafe)) {
            const unsafe_tok = self.previous();
            const block = self.parseBlock() orelse return null;
            const span = Span{ .file_id = unsafe_tok.span.file_id, .start = unsafe_tok.span.start, .end = block.span.end };
            return self.allocExpr(.{ .tag = .Unsafe, .span = span, .data = .{ .Unsafe = .{ .block = block } } });
        }

        return self.parseAssign();
    }

    /// Parse an expression in a context where struct literals are not allowed
    /// (e.g., condition of if/while, iterator of for).
    fn parseExprNoStructLiteral(self: *Parser) ?*ast.Expr {
        const prev_no_struct_literal = self.no_struct_literal;
        self.no_struct_literal = true;
        defer self.no_struct_literal = prev_no_struct_literal;
        return self.parseAssign();
    }

    fn parseAssign(self: *Parser) ?*ast.Expr {
        const left = self.parseIfExpr() orelse return null;
        if (self.match(.Eq) or self.match(.PlusEq) or self.match(.MinusEq) or self.match(.StarEq) or self.match(.SlashEq)) {
            const op_token = self.previous();
            const value = self.parseAssign() orelse return null;
            const op = switch (op_token.kind) {
                .Eq => ast.AssignOp.Assign,
                .PlusEq => ast.AssignOp.AddAssign,
                .MinusEq => ast.AssignOp.SubAssign,
                .StarEq => ast.AssignOp.MulAssign,
                .SlashEq => ast.AssignOp.DivAssign,
                else => ast.AssignOp.Assign,
            };
            const span = Span{ .file_id = left.span.file_id, .start = left.span.start, .end = value.span.end };
            return self.allocExpr(.{ .tag = .Assignment, .span = span, .data = .{ .Assignment = .{ .target = left, .op = op, .value = value } } });
        }
        return left;
    }

    fn parseIfExpr(self: *Parser) ?*ast.Expr {
        if (!self.match(.KwIf)) {
            return self.parseOr();
        }
        const if_tok = self.previous();
        const cond = self.parseExprNoStructLiteral() orelse return null;
        const then_block = self.parseBlock() orelse return null;
        var else_expr: ?*ast.Expr = null;
        if (self.match(.KwElse)) {
            if (self.check(.KwIf)) {
                else_expr = self.parseIfExpr();
            } else {
                if (self.parseBlock()) |blk| {
                    else_expr = self.allocExpr(.{ .tag = .Block, .span = blk.span, .data = .{ .Block = blk } });
                }
            }
        }
        const end_span = if (else_expr) |e| e.span.end else then_block.span.end;
        const span = Span{ .file_id = if_tok.span.file_id, .start = if_tok.span.start, .end = end_span };
        return self.allocExpr(.{ .tag = .If, .span = span, .data = .{ .If = .{ .condition = cond, .then_block = then_block, .else_expr = else_expr } } });
    }

    fn parseOr(self: *Parser) ?*ast.Expr {
        var expr = self.parseAnd() orelse return null;
        while (self.match(.DoublePipe)) {
            const rhs = self.parseAnd() orelse break;
            const span = Span{ .file_id = expr.span.file_id, .start = expr.span.start, .end = rhs.span.end };
            expr = self.allocExpr(.{ .tag = .Binary, .span = span, .data = .{ .Binary = .{ .op = .LogicalOr, .left = expr, .right = rhs } } });
        }
        return expr;
    }

    fn parseAnd(self: *Parser) ?*ast.Expr {
        var expr = self.parseEq() orelse return null;
        while (self.match(.DoubleAmp)) {
            const rhs = self.parseEq() orelse break;
            const span = Span{ .file_id = expr.span.file_id, .start = expr.span.start, .end = rhs.span.end };
            expr = self.allocExpr(.{ .tag = .Binary, .span = span, .data = .{ .Binary = .{ .op = .LogicalAnd, .left = expr, .right = rhs } } });
        }
        return expr;
    }

    fn parseEq(self: *Parser) ?*ast.Expr {
        var expr = self.parseRel() orelse return null;
        while (self.match(.DoubleEq) or self.match(.BangEq)) {
            const op_tok = self.previous();
            const rhs = self.parseRel() orelse break;
            const op: ast.BinaryOp = if (op_tok.kind == .DoubleEq) .Eq else .Ne;
            const span = Span{ .file_id = expr.span.file_id, .start = expr.span.start, .end = rhs.span.end };
            expr = self.allocExpr(.{ .tag = .Binary, .span = span, .data = .{ .Binary = .{ .op = op, .left = expr, .right = rhs } } });
        }
        return expr;
    }

    fn parseRel(self: *Parser) ?*ast.Expr {
        var expr = self.parseRange() orelse return null;
        while (self.match(.Lt) or self.match(.Le) or self.match(.Gt) or self.match(.Ge)) {
            const op_tok = self.previous();
            const rhs = self.parseRange() orelse break;
            const op = switch (op_tok.kind) {
                .Lt => ast.BinaryOp.Lt,
                .Le => ast.BinaryOp.Le,
                .Gt => ast.BinaryOp.Gt,
                .Ge => ast.BinaryOp.Ge,
                else => ast.BinaryOp.Lt,
            };
            const span = Span{ .file_id = expr.span.file_id, .start = expr.span.start, .end = rhs.span.end };
            expr = self.allocExpr(.{ .tag = .Binary, .span = span, .data = .{ .Binary = .{ .op = op, .left = expr, .right = rhs } } });
        }
        return expr;
    }

    fn parseRange(self: *Parser) ?*ast.Expr {
        var expr = self.parseAdd() orelse return null;
        while (self.match(.DotDot) or self.match(.DotDotEq)) {
            const op_tok = self.previous();
            const rhs = self.parseAdd() orelse break;
            const span = Span{ .file_id = expr.span.file_id, .start = expr.span.start, .end = rhs.span.end };
            expr = self.allocExpr(.{ .tag = .Range, .span = span, .data = .{ .Range = .{ .inclusive = op_tok.kind == .DotDotEq, .start = expr, .end = rhs } } });
        }
        return expr;
    }

    fn parseAdd(self: *Parser) ?*ast.Expr {
        var expr = self.parseMul() orelse return null;
        while (self.match(.Plus) or self.match(.Minus)) {
            const op_tok = self.previous();
            const rhs = self.parseMul() orelse break;
            const op = if (op_tok.kind == .Plus) ast.BinaryOp.Add else ast.BinaryOp.Sub;
            const span = Span{ .file_id = expr.span.file_id, .start = expr.span.start, .end = rhs.span.end };
            expr = self.allocExpr(.{ .tag = .Binary, .span = span, .data = .{ .Binary = .{ .op = op, .left = expr, .right = rhs } } });
        }
        return expr;
    }

    fn parseMul(self: *Parser) ?*ast.Expr {
        var expr = self.parseCast() orelse return null;
        while (self.match(.Star) or self.match(.Slash) or self.match(.Percent)) {
            const op_tok = self.previous();
            const rhs = self.parseCast() orelse break;
            const op = switch (op_tok.kind) {
                .Star => ast.BinaryOp.Mul,
                .Slash => ast.BinaryOp.Div,
                .Percent => ast.BinaryOp.Mod,
                else => ast.BinaryOp.Mul,
            };
            const span = Span{ .file_id = expr.span.file_id, .start = expr.span.start, .end = rhs.span.end };
            expr = self.allocExpr(.{ .tag = .Binary, .span = span, .data = .{ .Binary = .{ .op = op, .left = expr, .right = rhs } } });
        }
        return expr;
    }

    fn parseCast(self: *Parser) ?*ast.Expr {
        var expr = self.parseUnary() orelse return null;
        while (self.match(.KwAs)) {
            const ty = self.parseType() orelse break;
            const span = Span{ .file_id = expr.span.file_id, .start = expr.span.start, .end = ty.span.end };
            expr = self.allocExpr(.{ .tag = .Cast, .span = span, .data = .{ .Cast = .{ .expr = expr, .ty = self.copyType(ty) } } });
        }
        return expr;
    }

    fn parseUnary(self: *Parser) ?*ast.Expr {
        if (self.match(.Bang) or self.match(.Minus) or self.match(.Star) or self.match(.Amp)) {
            const op_tok = self.previous();
            const mut_ref = op_tok.kind == .Amp and self.match(.KwMut);
            const rhs = self.parseUnary() orelse return null;
            const op = switch (op_tok.kind) {
                .Bang => ast.UnaryOp.Not,
                .Minus => ast.UnaryOp.Neg,
                .Star => ast.UnaryOp.Deref,
                .Amp => if (mut_ref) ast.UnaryOp.RefMut else ast.UnaryOp.Ref,
                else => ast.UnaryOp.Not,
            };
            const span = Span{ .file_id = op_tok.span.file_id, .start = op_tok.span.start, .end = rhs.span.end };
            return self.allocExpr(.{ .tag = .Unary, .span = span, .data = .{ .Unary = .{ .op = op, .expr = rhs } } });
        }
        return self.parsePostfix();
    }

    fn parsePostfix(self: *Parser) ?*ast.Expr {
        var expr = self.parsePrimary() orelse return null;
        while (true) {
            if (self.match(.LParen)) {
                expr = self.finishCall(expr, "expected ')' after call") orelse return null;
                continue;
            }
            if (self.match(.LBracket)) {
                const index_expr = self.parseExpr() orelse return null;
                const rbrack = self.expectConsume(.RBracket, "expected ']' after index") orelse return null;
                const span = Span{ .file_id = expr.span.file_id, .start = expr.span.start, .end = rbrack.span.end };
                expr = self.allocExpr(.{ .tag = .Index, .span = span, .data = .{ .Index = .{ .target = expr, .index = index_expr } } });
                continue;
            }
            if (self.match(.Dot)) {
                const field_tok = self.expectConsume(.Identifier, "expected field name") orelse return null;
                const span = Span{ .file_id = expr.span.file_id, .start = expr.span.start, .end = field_tok.span.end };
                expr = self.allocExpr(.{ .tag = .Field, .span = span, .data = .{ .Field = .{ .target = expr, .field = self.makeIdent(field_tok) } } });
                continue;
            }
            if (self.match(.Bang)) {
                const bang = self.previous();
                if (!self.match(.LParen)) {
                    self.reportError(bang.span, "expected '(' after macro invocation");
                    return null;
                }
                expr = self.finishCall(expr, "expected ')' after macro invocation") orelse return null;
                continue;
            }
            // Struct literal: Path { field: value, ... }
            // Skip struct literal parsing when we're in a no-struct-literal context
            // (e.g., condition of if/while, iterator of for)
            if (expr.tag == .Path and self.check(.LBrace) and !self.no_struct_literal) {
                expr = self.finishStructInit(expr) orelse return null;
                continue;
            }
            break;
        }
        return expr;
    }

    fn finishStructInit(self: *Parser, path_expr: *ast.Expr) ?*ast.Expr {
        const lbrace = self.expectConsume(.LBrace, "expected '{'") orelse return null;
        _ = lbrace;
        var fields = std.ArrayListUnmanaged(ast.StructInitField){};
        while (!self.check(.RBrace) and !self.isAtEnd()) {
            const field_name_tok = self.expectConsume(.Identifier, "expected field name") orelse break;
            _ = self.expectConsume(.Colon, "expected ':' after field name") orelse break;
            const value = self.parseExpr() orelse break;
            fields.append(self.arena, .{ .name = self.makeIdent(field_name_tok), .value = value.* }) catch {};
            if (!self.match(.Comma)) break;
        }
        const rbrace = self.expectConsume(.RBrace, "expected '}' to close struct literal") orelse return null;
        const span = Span{ .file_id = path_expr.span.file_id, .start = path_expr.span.start, .end = rbrace.span.end };
        return self.allocExpr(.{ .tag = .StructInit, .span = span, .data = .{ .StructInit = .{ .path = path_expr.data.Path, .fields = fields.toOwnedSlice(self.arena) catch @panic("out of memory") } } });
    }

    fn finishCall(self: *Parser, callee: *ast.Expr, close_message: []const u8) ?*ast.Expr {
        var args = std.ArrayListUnmanaged(ast.Expr){};
        if (!self.check(.RParen)) {
            while (true) {
                const arg = self.parseExpr() orelse break;
                args.append(self.arena, arg.*) catch {};
                if (!self.match(.Comma)) break;
            }
        }
        const rparen = self.expectConsume(.RParen, close_message) orelse return null;
        const span = Span{ .file_id = callee.span.file_id, .start = callee.span.start, .end = rparen.span.end };
        return self.allocExpr(.{ .tag = .Call, .span = span, .data = .{ .Call = .{ .callee = callee, .args = args.toOwnedSlice(self.arena) catch @panic("out of memory") } } });
    }

    fn parsePrimary(self: *Parser) ?*ast.Expr {
        const tok = self.peek();
        switch (tok.kind) {
            .IntLit, .FloatLit, .BoolLit, .CharLit, .StringLit => {
                _ = self.advance();
                const lit_kind: ast.Literal.Kind = switch (tok.kind) {
                    .IntLit => .Int,
                    .FloatLit => .Float,
                    .BoolLit => .Bool,
                    .CharLit => .Char,
                    .StringLit => .String,
                    else => .Int,
                };
                const expr = ast.Expr{ .tag = .Literal, .span = tok.span, .data = .{ .Literal = .{ .kind = lit_kind, .lexeme = self.dup(tok.lexeme) } } };
                return self.copyExpr(expr);
            },
            .Identifier, .KwSelf => {
                if (self.parsePath()) |p| {
                    return self.allocExpr(.{ .tag = .Path, .span = p.span, .data = .{ .Path = p } });
                }
                return null;
            },
            .LParen => {
                _ = self.advance();
                const expr = self.parseExpr() orelse return null;
                const rparen = self.expectConsume(.RParen, "expected ')' after expression") orelse return expr;
                const span = Span{ .file_id = tok.span.file_id, .start = tok.span.start, .end = rparen.span.end };
                return self.allocExpr(.{ .tag = .Paren, .span = span, .data = .{ .Paren = expr } });
            },
            .LBrace => {
                if (self.parseBlock()) |blk| {
                    return self.allocExpr(.{ .tag = .Block, .span = blk.span, .data = .{ .Block = blk } });
                }
                return null;
            },
            .KwUnsafe => {
                // unsafe blocks are parsed as regular blocks for now (we don't enforce safety)
                const unsafe_tok = self.advance();
                if (self.parseBlock()) |blk| {
                    const span = Span{ .file_id = unsafe_tok.span.file_id, .start = unsafe_tok.span.start, .end = blk.span.end };
                    return self.allocExpr(.{ .tag = .Block, .span = span, .data = .{ .Block = blk } });
                }
                return null;
            },
            .LBracket => {
                const lbrack = self.advance();
                var elements = std.ArrayListUnmanaged(ast.Expr){};
                if (!self.check(.RBracket)) {
                    while (true) {
                        const elem = self.parseExpr() orelse break;
                        elements.append(self.arena, elem.*) catch {};
                        if (!self.match(.Comma)) break;
                    }
                }
                const rbrack = self.expectConsume(.RBracket, "expected ']' after array literal") orelse return null;
                const span = Span{ .file_id = lbrack.span.file_id, .start = lbrack.span.start, .end = rbrack.span.end };
                return self.allocExpr(.{ .tag = .Array, .span = span, .data = .{ .Array = .{ .elements = elements.toOwnedSlice(self.arena) catch @panic("out of memory") } } });
            },
            .Pipe => {
                _ = self.advance();
                var params = std.ArrayListUnmanaged(ast.Param){};
                if (!self.check(.Pipe)) {
                    while (true) {
                        const pat = self.parsePattern() orelse break;
                        const ty = if (self.match(.Colon)) self.parseType() else null;
                        const span_end = if (ty) |t| t.span.end else pat.span.end;
                        const param_span = Span{ .file_id = pat.span.file_id, .start = pat.span.start, .end = span_end };
                        params.append(self.arena, .{ .pattern = pat, .ty = ty, .span = param_span, .kind = .Normal }) catch {};
                        if (!self.match(.Comma)) break;
                    }
                }
                _ = self.expectConsume(.Pipe, "expected '|' to close lambda parameters") orelse return null;
                if (self.check(.LBrace)) {
                    const blk = self.parseBlock() orelse return null;
                    const span = Span{ .file_id = tok.span.file_id, .start = tok.span.start, .end = blk.span.end };
                    return self.allocExpr(.{ .tag = .Lambda, .span = span, .data = .{ .Lambda = .{ .params = params.toOwnedSlice(self.arena) catch @panic("out of memory"), .body = .{ .Block = blk } } } });
                }
                if (self.parseExpr()) |body_expr| {
                    const span = Span{ .file_id = tok.span.file_id, .start = tok.span.start, .end = body_expr.span.end };
                    return self.allocExpr(.{ .tag = .Lambda, .span = span, .data = .{ .Lambda = .{ .params = params.toOwnedSlice(self.arena) catch @panic("out of memory"), .body = .{ .Expr = body_expr } } } });
                }
                return null;
            },
            else => {
                self.reportError(tok.span, "expected expression");
                return null;
            },
        }
    }

    fn isStartOfExpr(self: *Parser) bool {
        return switch (self.peekKind()) {
            .LParen, .LBrace, .LBracket, .Identifier, .IntLit, .FloatLit, .BoolLit, .CharLit, .StringLit, .KwIf, .KwWhile, .KwFor, .KwReturn, .Bang, .Minus, .Star, .Amp, .Pipe, .KwSelf, .KwUnsafe => true,
            else => false,
        };
    }

    fn expectConsume(self: *Parser, kind: TokenKind, message: []const u8) ?Token {
        if (self.check(kind)) return self.advance();
        self.reportError(self.peek().span, message);
        return null;
    }

    fn reportError(self: *Parser, span: Span, message: []const u8) void {
        self.diagnostics.reportError(span, message);
    }

    fn synchronize(self: *Parser) void {
        if (self.tokens.len == 0) return;

        if (self.pos == 0) {
            _ = self.advance();
        }

        while (!self.isAtEnd()) {
            //this line
            if (self.previous().kind == .Semicolon) return;
            switch (self.peekKind()) {
                .KwFn, .KwStruct, .KwImpl, .KwType, .KwLet, .KwWhile, .KwFor, .KwIf, .KwReturn => return,
                .RBrace => return,
                else => {},
            }

            _ = self.advance();
        }
    }

    fn allocExpr(self: *Parser, expr: ast.Expr) *ast.Expr {
        const ptr = self.arena.create(ast.Expr) catch @panic("out of memory");
        ptr.* = expr;
        return ptr;
    }

    fn copyExpr(self: *Parser, expr: ast.Expr) *ast.Expr {
        const ptr = self.arena.create(ast.Expr) catch @panic("out of memory");
        ptr.* = expr;
        return ptr;
    }

    fn copyType(self: *Parser, ty: ast.Type) *ast.Type {
        const ptr = self.arena.create(ast.Type) catch @panic("out of memory");
        ptr.* = ty;
        return ptr;
    }

    fn makeIdent(self: *Parser, tok: Token) ast.Identifier {
        return .{ .name = self.dup(tok.lexeme), .span = tok.span };
    }

    fn dup(self: *Parser, slice: []const u8) []const u8 {
        return self.arena.dupe(u8, slice) catch slice;
    }

    fn match(self: *Parser, kind: TokenKind) bool {
        if (!self.check(kind)) return false;
        _ = self.advance();
        return true;
    }

    fn check(self: *Parser, kind: TokenKind) bool {
        if (self.isAtEnd()) return false;
        return self.peekKind() == kind;
    }

    fn checkSelfRef(self: *Parser) bool {
        if (!self.check(.Amp)) return false;
        if (self.pos + 1 >= self.tokens.len) return false;
        const next = self.tokens[self.pos + 1];
        return next.kind == .KwSelf or (next.kind == .KwMut and self.pos + 2 < self.tokens.len and self.tokens[self.pos + 2].kind == .KwSelf);
    }

    fn advance(self: *Parser) Token {
        if (self.isAtEnd()) return self.sentinelToken();
        self.pos += 1;
        return self.tokens[self.pos - 1];
    }

    fn isAtEnd(self: *Parser) bool {
        return self.pos >= self.tokens.len;
    }

    fn peek(self: *Parser) Token {
        if (self.isAtEnd()) return self.sentinelToken();
        return self.tokens[self.pos];
    }

    fn peekKind(self: *Parser) TokenKind {
        return self.peek().kind;
    }

    fn previous(self: *Parser) Token {
        if (self.pos == 0) return self.sentinelToken();
        return self.tokens[self.pos - 1];
    }

    fn sentinelToken(self: *Parser) Token {
        if (self.tokens.len > 0) return self.tokens[self.tokens.len - 1];
        return .{ .kind = .Semicolon, .lexeme = "", .span = .{ .file_id = 0, .start = 0, .end = 0 } };
    }
};

// --------------------------
// Tests
// --------------------------

const lexer = @import("lexer.zig");

const ParseFixture = struct {
    arena: std.heap.ArenaAllocator,
    diagnostics: diag.Diagnostics,
    crate: ast.Crate,
};

fn parseSource(src: []const u8) !ParseFixture {
    const allocator = std.testing.allocator;
    var diagnostics = diag.Diagnostics.init(allocator);
    const file_id: source_map.FileId = 0;
    const toks = try lexer.lex(allocator, file_id, src, &diagnostics);
    var arena = std.heap.ArenaAllocator.init(allocator);
    const crate = parseCrate(arena.allocator(), toks, &diagnostics);
    allocator.free(toks);
    return .{ .arena = arena, .diagnostics = diagnostics, .crate = crate };
}

fn expectNoErrors(src: []const u8) !ParseFixture {
    var fixture = try parseSource(src);
    try std.testing.expect(!fixture.diagnostics.hasErrors());
    return fixture;
}

test "parse simple fn" {
    var fixture = try expectNoErrors("fn main() {}");
    defer {
        fixture.diagnostics.deinit();
        fixture.arena.deinit();
    }

    try std.testing.expectEqual(@as(usize, 1), fixture.crate.items.len);
    try std.testing.expectEqual(ast.Item.Tag.Fn, fixture.crate.items[0].tag);
}

test "parse let binding in block" {
    var fixture = try expectNoErrors("fn main() { let x: i32 = 1; }");
    defer {
        fixture.diagnostics.deinit();
        fixture.arena.deinit();
    }
    const fn_item = fixture.crate.items[0].data.Fn;
    try std.testing.expectEqual(@as(usize, 1), fn_item.body.stmts.len);
    try std.testing.expectEqual(ast.Stmt.Tag.Let, fn_item.body.stmts[0].tag);
}

test "parse binary expressions" {
    var fixture = try expectNoErrors("fn main() { let x = 1 + 2 * 3; }");
    defer {
        fixture.diagnostics.deinit();
        fixture.arena.deinit();
    }
    const stmt = fixture.crate.items[0].data.Fn.body.stmts[0].data.Let;
    try std.testing.expect(stmt.value != null);
    const expr = stmt.value.?;
    try std.testing.expect(expr.data.Binary.op == ast.BinaryOp.Add);
}

test "report error on missing semicolon" {
    var fixture = try parseSource("fn main() { let x = 1 }");
    defer {
        fixture.diagnostics.deinit();
        fixture.arena.deinit();
    }
    try std.testing.expect(fixture.diagnostics.hasErrors());
}

test "parse macro calls with arguments" {
    var fixture = try expectNoErrors("fn main() { println!(\"hello\"); println!(\"{} {}\", 1, 2); }");
    defer {
        fixture.diagnostics.deinit();
        fixture.arena.deinit();
    }

    const stmts = fixture.crate.items[0].data.Fn.body.stmts;
    try std.testing.expectEqual(@as(usize, 2), stmts.len);

    const first_expr_stmt = stmts[0].data.Expr.expr;
    try std.testing.expectEqual(ast.Expr.Tag.Call, first_expr_stmt.tag);
    try std.testing.expectEqual(@as(usize, 1), first_expr_stmt.data.Call.args.len);
    try std.testing.expectEqual(ast.Expr.Tag.Path, first_expr_stmt.data.Call.callee.*.tag);

    const second_expr_stmt = stmts[1].data.Expr.expr;
    try std.testing.expectEqual(ast.Expr.Tag.Call, second_expr_stmt.tag);
    try std.testing.expectEqual(@as(usize, 3), second_expr_stmt.data.Call.args.len);
    try std.testing.expectEqual(ast.Expr.Tag.Path, second_expr_stmt.data.Call.callee.*.tag);
}

test "diagnose macro invocation without parentheses" {
    var fixture = try parseSource("fn main() { println! \"oops\"; }");
    defer {
        fixture.diagnostics.deinit();
        fixture.arena.deinit();
    }

    try std.testing.expect(fixture.diagnostics.hasErrors());
    try std.testing.expectEqualStrings("expected '(' after macro invocation", fixture.diagnostics.entries.items[0].message);
}
