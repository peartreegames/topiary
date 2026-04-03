const std = @import("std");
const ast = @import("ast.zig");
const Tree = ast.Tree;
const Statement = ast.Statement;
const Expression = ast.Expression;
const BinaryOp = ast.BinaryOp;
const UnaryOp = ast.UnaryOp;
const tok = @import("token.zig");
const Token = tok.Token;
const TokenType = tok.TokenType;

pub const Formatter = struct {
    buf: std.ArrayList(u8),
    source: []const u8,
    indent: usize,
    indent_width: usize,
    allocator: std.mem.Allocator,
    suppress_indent: bool,

    pub fn format(source: []const u8, tree: Tree, allocator: std.mem.Allocator, indent_width: usize) ![]const u8 {
        var self = Formatter{
            .buf = .empty,
            .source = source,
            .indent = 0,
            .indent_width = indent_width,
            .allocator = allocator,
            .suppress_indent = false,
        };

        for (tree.root, 0..) |stmt, i| {
            if (i > 0) {
                if (isTopLevelDecl(stmt) or isTopLevelDecl(tree.root[i - 1])) {
                    try self.write("\n");
                }
            }
            try self.writeStatement(stmt);
            try self.write("\n");
        }

        return self.buf.toOwnedSlice(self.allocator);
    }

    fn isTopLevelDecl(stmt: Statement) bool {
        return switch (stmt.type) {
            .bough, .function, .class, .@"enum" => true,
            else => false,
        };
    }

    const Error = std.mem.Allocator.Error;

    fn writeStatement(self: *Formatter, stmt: Statement) Error!void {
        switch (stmt.type) {
            .include => |path| {
                try self.writeIndent();
                try self.write("include \"");
                try self.write(path);
                try self.write("\"");
            },
            .variable => |v| {
                try self.writeIndent();
                try self.write(if (v.is_mutable) "var " else "const ");
                try self.write(v.name);
                try self.write(" = ");
                try self.writeExpression(v.initializer);
            },
            .function => |f| {
                try self.writeIndent();
                if (f.is_extern) try self.write("extern ");
                try self.write("fn ");
                try self.write(f.name);
                try self.write(" |");
                for (f.parameters, 0..) |p, i| {
                    if (i > 0) try self.write(", ");
                    try self.write(p);
                }
                try self.write("|");
                try self.writeBody(f.body);
            },
            .class => |c| {
                try self.writeIndent();
                try self.write("class ");
                try self.write(c.name);
                try self.write(" {\n");
                self.indent += 1;
                for (c.field_names, c.fields, 0..) |name, field, i| {
                    try self.writeIndent();
                    try self.write(name);
                    try self.write(" = ");
                    try self.writeExpression(field);
                    try self.write(",");
                    if (i < c.field_names.len - 1 or c.methods.len > 0) try self.write("\n");
                }
                if (c.methods.len > 0 and c.field_names.len > 0) {
                    try self.write("\n");
                }
                for (c.methods, 0..) |method, i| {
                    try self.write("\n");
                    try self.writeStatement(method);
                    if (i < c.methods.len - 1) try self.write("\n");
                }
                try self.write("\n");
                self.indent -= 1;
                try self.writeIndent();
                try self.write("}");
            },
            .@"enum" => |e| {
                try self.writeIndent();
                try self.write(if (e.is_seq) "enumseq " else "enum ");
                try self.write(e.name);
                try self.write(" {\n");
                self.indent += 1;
                for (e.values, 0..) |v, i| {
                    try self.writeIndent();
                    try self.write(v);
                    if (i < e.values.len - 1) try self.write(",");
                    try self.write("\n");
                }
                self.indent -= 1;
                try self.writeIndent();
                try self.write("}");
            },
            .bough => |b| {
                try self.writeIndent();
                try self.write("=== ");
                try self.write(b.name);
                try self.write(" {\n");
                self.indent += 1;
                try self.writeStatements(b.body);
                self.indent -= 1;
                try self.writeIndent();
                try self.write("}");
            },
            .fork => |f| {
                try self.writeIndent();
                try self.write("fork");
                if (f.is_backup) try self.write("^");
                if (f.name) |name| {
                    try self.write(" ");
                    try self.write(name);
                }
                try self.write(" {\n");
                self.indent += 1;
                try self.writeStatements(f.body);
                self.indent -= 1;
                try self.writeIndent();
                try self.write("}");
            },
            .choice => |c| {
                try self.writeIndent();
                if (c.is_unique) {
                    try self.write("~* ");
                } else {
                    try self.write("~ ");
                }
                if (c.name) |name| {
                    try self.write("\"");
                    try self.write(name);
                    try self.write("\"");
                } else {
                    try self.writeExpression(c.content);
                }
                for (c.tags) |tag| {
                    try self.write(" #");
                    try self.write(tag);
                }
                if (c.body.len > 0) {
                    if (isSingleLineBody(c.body)) {
                        try self.write(" ");
                        self.suppress_indent = true;
                        try self.writeStatement(c.body[0]);
                    } else {
                        try self.write(" {\n");
                        self.indent += 1;
                        try self.writeStatements(c.body);
                        self.indent -= 1;
                        try self.writeIndent();
                        try self.write("}");
                    }
                }
            },
            .dialogue => |d| {
                try self.writeIndent();
                try self.write(":");
                if (d.speaker) |speaker| {
                    try self.write(speaker);
                }
                try self.write(": ");
                try self.writeExpression(d.content.*);
                for (d.tags) |tag| {
                    try self.write(" #");
                    try self.write(tag);
                }
            },
            .divert => |d| {
                try self.writeIndent();
                try self.writeDivert(d);
            },
            .@"if" => |i| {
                try self.writeIndent();
                try self.writeIf(i.condition, i.then_branch, i.else_branch);
            },
            .@"while" => |w| {
                try self.writeIndent();
                try self.write("while ");
                try self.writeExpression(w.condition);
                try self.writeBodyBraces(w.body);
            },
            .@"for" => |f| {
                try self.writeIndent();
                try self.write("for ");
                try self.writeExpression(f.iterator);
                try self.write(" |");
                try self.write(f.capture);
                try self.write("|");
                try self.writeBodyBraces(f.body);
            },
            .@"switch" => |s| {
                try self.writeIndent();
                try self.write("switch ");
                try self.writeExpression(s.capture);
                try self.write(" {\n");
                self.indent += 1;
                try self.writeStatements(s.prongs);
                self.indent -= 1;
                try self.writeIndent();
                try self.write("}");
            },
            .switch_prong => |p| {
                try self.writeIndent();
                if (p.values) |vals| {
                    for (vals, 0..) |v, i| {
                        if (i > 0) try self.write(", ");
                        try self.writeExpression(v);
                    }
                } else {
                    try self.write("else");
                }
                try self.write(":");
                if (p.body.len == 1) {
                    try self.write(" ");
                    try self.writeStatement(p.body[0]);
                } else {
                    try self.write("\n");
                    self.indent += 1;
                    try self.writeStatements(p.body);
                    self.indent -= 1;
                }
            },
            .return_expression => |e| {
                try self.writeIndent();
                try self.write("return ");
                try self.writeExpression(e);
            },
            .return_void => {
                try self.writeIndent();
                try self.write("return void");
            },
            .fin => {
                try self.writeIndent();
                try self.write("fin");
            },
            .@"break" => {
                try self.writeIndent();
                try self.write("break");
            },
            .@"continue" => {
                try self.writeIndent();
                try self.write("continue");
            },
            .block => |b| {
                try self.writeIndent();
                try self.write("{\n");
                self.indent += 1;
                try self.writeStatements(b);
                self.indent -= 1;
                try self.writeIndent();
                try self.write("}");
            },
            .comment => |c| {
                try self.writeIndent();
                try self.write(c);
            },
            .expression => |e| {
                try self.writeIndent();
                try self.writeExpression(e);
            },
        }
    }

    fn writeIf(self: *Formatter, condition: *Expression, then_branch: []const Statement, else_branch: ?[]const Statement) Error!void {
        try self.write("if ");
        try self.writeExpression(condition.*);
        try self.writeBody(then_branch);
        if (else_branch) |eb| {
            if (eb.len == 1 and eb[0].type == .@"if") {
                const inner = eb[0].type.@"if";
                try self.write(" else ");
                try self.writeIf(inner.condition, inner.then_branch, inner.else_branch);
            } else {
                try self.write(" else");
                try self.writeBody(eb);
            }
        }
    }

    fn writeBody(self: *Formatter, body: []const Statement) Error!void {
        try self.writeBodyOpts(body, true);
    }

    fn writeBodyBraces(self: *Formatter, body: []const Statement) Error!void {
        try self.writeBodyOpts(body, false);
    }

    fn writeBodyOpts(self: *Formatter, body: []const Statement, allow_inline: bool) Error!void {
        if (allow_inline and isSingleLineBody(body)) {
            try self.write(" ");
            self.suppress_indent = true;
            try self.writeStatement(body[0]);
        } else {
            try self.write(" {\n");
            self.indent += 1;
            try self.writeStatements(body);
            self.indent -= 1;
            try self.writeIndent();
            try self.write("}");
        }
    }

    fn isSingleLineBody(body: []const Statement) bool {
        return body.len == 1 and switch (body[0].type) {
            .dialogue, .divert, .fin, .@"break", .@"continue", .return_expression, .return_void => true,
            else => false,
        };
    }

    fn writeDivert(self: *Formatter, d: anytype) Error!void {
        try self.write("=>");
        if (d.is_backup) try self.write("^");
        try self.write(" ");
        for (d.path, 0..) |p, i| {
            if (i > 0) try self.write(".");
            try self.write(p);
        }
    }

    fn writeStatements(self: *Formatter, stmts: []const Statement) Error!void {
        for (stmts) |stmt| {
            try self.writeStatement(stmt);
            try self.write("\n");
        }
    }

    fn writeExpression(self: *Formatter, expr: Expression) Error!void {
        switch (expr.type) {
            .binary => |b| {
                try self.writeExpression(b.left.*);
                try self.write(" ");
                try self.write(b.operator.toString());
                try self.write(" ");
                try self.writeExpression(b.right.*);
            },
            .unary => |u| {
                try self.write(u.operator.toString());
                try self.writeExpression(u.value.*);
            },
            .call => |c| {
                try self.writeExpression(c.target.*);
                try self.write("(");
                for (c.arguments, 0..) |arg, i| {
                    if (i > 0) try self.write(", ");
                    try self.writeExpression(arg);
                }
                try self.write(")");
            },
            .indexer => |ix| {
                try self.writeExpression(ix.target.*);
                if (expr.token.token_type == .dot) {
                    try self.write(".");
                    try self.writeExpression(ix.index.*);
                } else {
                    try self.write("[");
                    try self.writeExpression(ix.index.*);
                    try self.write("]");
                }
            },
            .identifier => |id| {
                try self.write(id);
            },
            .number => {
                try self.write(self.source[expr.token.start..expr.token.end]);
            },
            .boolean => |b| {
                try self.write(if (b) "true" else "false");
            },
            .nil => {
                try self.write("nil");
            },
            .string => |s| {
                try self.write("\"");
                try self.write(s.raw);
                try self.write("\"");
            },
            .list => |l| {
                try self.write("List{");
                for (l, 0..) |item, i| {
                    if (i > 0) try self.write(", ");
                    try self.writeExpression(item);
                }
                try self.write("}");
            },
            .set => |s| {
                try self.write("Set{");
                for (s, 0..) |item, i| {
                    if (i > 0) try self.write(", ");
                    try self.writeExpression(item);
                }
                try self.write("}");
            },
            .map => |m| {
                try self.write("Map{");
                for (m, 0..) |pair, i| {
                    if (i > 0) try self.write(", ");
                    try self.writeExpression(pair);
                }
                try self.write("}");
            },
            .map_pair => |p| {
                try self.writeExpression(p.key.*);
                try self.write(": ");
                try self.writeExpression(p.value.*);
            },
            .instance => |inst| {
                try self.write("new ");
                try self.write(inst.name);
                try self.write("{");
                for (inst.field_names, inst.fields, 0..) |name, field, i| {
                    if (i > 0) try self.write(", ");
                    try self.write(" ");
                    try self.write(name);
                    try self.write(" = ");
                    try self.writeExpression(field);
                }
                if (inst.field_names.len > 0) try self.write(" ");
                try self.write("}");
            },
            .range => |r| {
                try self.writeExpression(r.left.*);
                try self.write("..");
                try self.writeExpression(r.right.*);
            },
            .@"if" => |i| {
                try self.write("if ");
                try self.writeExpression(i.condition.*);
                try self.write(" ");
                try self.writeExpression(i.then_value.*);
                try self.write(" else ");
                try self.writeExpression(i.else_value.*);
            },
            .@"extern" => {
                try self.write("extern");
            },
        }
    }

    fn writeIndent(self: *Formatter) Error!void {
        if (self.suppress_indent) {
            self.suppress_indent = false;
            return;
        }
        const total = self.indent * self.indent_width;
        var i: usize = 0;
        while (i < total) : (i += 1) {
            try self.write(" ");
        }
    }

    fn write(self: *Formatter, bytes: []const u8) Error!void {
        try self.buf.appendSlice(self.allocator, bytes);
    }
};
