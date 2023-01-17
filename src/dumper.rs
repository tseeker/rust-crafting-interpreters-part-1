use std::fmt::Write;

use crate::{
    ast::{BinaryExpr, ClassMemberDecl, ClassMemberKind, ExprNode, ProgramNode, StmtNode},
    tokens::Token,
};

/* -------------------------------- *
 * Dumper trait and implementations *
 * -------------------------------- */

#[allow(dead_code)]
pub fn dump_program(ast: &ProgramNode) -> String {
    let mut dumper = Dumper::default();
    dump_statement_list(&mut dumper, &ast.0);
    dumper
        .lines
        .iter()
        .map(DumperLine::to_string)
        .collect::<Vec<String>>()
        .join("\n")
}

fn fun_decl_params(params: &[Token]) -> String {
    params
        .iter()
        .map(|token| &token.lexeme as &str)
        .collect::<Vec<&str>>()
        .join(" ")
}

fn dump_statement_list(dumper: &mut Dumper, statements: &[StmtNode]) {
    for stmt in statements {
        dump_statement(dumper, stmt);
    }
}

fn dump_substatement(dumper: &mut Dumper, statement: &StmtNode) {
    let depth_change = match statement {
        StmtNode::Block(_) => 0,
        _ => 1,
    };
    dumper.depth += depth_change;
    dump_statement(dumper, statement);
    dumper.depth -= depth_change;
}

fn member_header(member: &ClassMemberDecl) -> String {
    match member.kind {
        ClassMemberKind::Method => format!("({})", fun_decl_params(&member.fun_decl.params)),
        ClassMemberKind::Getter => ">".to_owned(),
        ClassMemberKind::Setter => "<".to_owned(),
    }
}

fn dump_member(dumper: &mut Dumper, member: &ClassMemberDecl) {
    dumper.add_line(format!(
        "{}{} ({}) {{",
        if member.is_static { "static " } else { "" },
        member.fun_decl.name.lexeme,
        member_header(member),
    ));
    dumper.depth += 1;
    dump_statement_list(dumper, &member.fun_decl.body);
    dumper.depth -= 1;
    dumper.add_line("}".to_owned());
}

fn dump_statement(dumper: &mut Dumper, stmt: &StmtNode) {
    match stmt {
        StmtNode::VarDecl(name, Some(expr)) => {
            dumper.integrate(
                &format!("var {} = ", name.lexeme),
                dump_expression(expr),
                ";",
            );
        }
        StmtNode::VarDecl(name, None) => {
            dumper.add_line(format!("var {};", name.lexeme));
        }

        StmtNode::FunDecl(fun_decl) => {
            dumper.add_line(format!(
                "fun {} ({}) {{",
                fun_decl.name.lexeme,
                fun_decl_params(&fun_decl.params),
            ));
            if !fun_decl.body.is_empty() {
                dumper.depth += 1;
                dump_statement_list(dumper, &fun_decl.body);
                dumper.depth -= 1;
                dumper.add_line(String::default());
            }
            dumper.current_line().push('}');
        }

        StmtNode::ClassDecl(decl) => {
            dumper.add_line(format!("class {}", decl.name.lexeme));
            if let Some(superclass) = &decl.superclass {
                dumper
                    .current_line()
                    .push_str(&format!(" < {}", superclass.token.lexeme));
            }
            dumper.current_line().push_str(" {");
            if !decl.members.is_empty() {
                dumper.depth += 1;
                for member in decl.members.iter() {
                    dump_member(dumper, member);
                }
                dumper.depth -= 1;
                dumper.add_line(String::default());
            }
            dumper.current_line().push('}');
        }

        StmtNode::Expression(expr) => {
            dumper.integrate("", dump_expression(expr), ";");
        }

        StmtNode::Print(expr) => {
            dumper.integrate("print ", dump_expression(expr), ";");
        }

        StmtNode::Block(stmts) => {
            dumper.add_line("{".to_owned());
            dumper.depth += 1;
            dump_statement_list(dumper, stmts);
            dumper.depth -= 1;
            dumper.add_line("}".to_owned());
        }

        StmtNode::If {
            condition,
            then_branch,
            else_branch,
        } => {
            dumper.integrate("if (", dump_expression(condition), ")");
            dump_substatement(dumper, then_branch);
            if let Some(else_branch) = else_branch {
                dumper.add_line("else".to_owned());
                dump_substatement(dumper, else_branch);
            }
        }

        StmtNode::Loop {
            label,
            condition,
            body,
            after_body,
        } => {
            if let Some(label) = label {
                dumper.add_line(format!("@{} ", label.lexeme));
            };
            dumper.integrate("while (", dump_expression(condition), ")");
            if let Some(after_body) = after_body {
                let mut statements = match body.as_ref() {
                    StmtNode::Block(block) => block.clone(),
                    other => vec![other.clone()],
                };
                match after_body.as_ref() {
                    StmtNode::Block(block) => statements.extend(block.clone()),
                    other => statements.push(other.clone()),
                };
                if statements.len() == 1 {
                    dump_substatement(dumper, &statements[0]);
                } else {
                    dump_substatement(dumper, &StmtNode::Block(statements));
                }
            } else {
                dump_substatement(dumper, body);
            }
        }

        StmtNode::LoopControl {
            is_break,
            loop_name,
        } => {
            let stmt = if *is_break { "break" } else { "continue" };
            dumper.add_line(match loop_name {
                Some(name) => format!("{} {};", stmt, name.lexeme),
                None => format!("{};", stmt),
            });
        }

        StmtNode::Return { token: _, value } => match value {
            Some(expr) => dumper.integrate("return ", dump_expression(expr), ";"),
            None => dumper.add_line("return;".to_owned()),
        },
    }
}

/* ----------- *
 * Expressions *
 * ----------- */

fn dump_expression(expr: &ExprNode) -> Dumper {
    let mut dumper = Dumper::default();
    dumper.add_line(String::default());
    dump_expr_node(&mut dumper, expr);
    dumper
}

fn dump_binary_expr(dumper: &mut Dumper, binary_expr: &BinaryExpr) {
    dump_expr_node(dumper, &binary_expr.left);
    dumper.current_line().push(' ');
    dumper.current_line().push_str(&binary_expr.operator.lexeme);
    dumper.current_line().push(' ');
    dump_expr_node(dumper, &binary_expr.right);
}

fn dump_expr_node(dumper: &mut Dumper, expr: &ExprNode) {
    match expr {
        ExprNode::Assignment { name, value, id: _ } => {
            dumper
                .current_line()
                .write_fmt(format_args!("{} = ", name.lexeme))
                .unwrap();
            dump_expr_node(dumper, value);
        }

        ExprNode::Logical(binary_expr) => dump_binary_expr(dumper, binary_expr),
        ExprNode::Binary(binary_expr) => dump_binary_expr(dumper, binary_expr),

        ExprNode::Unary { operator, right } => {
            dumper.current_line().push_str(&operator.lexeme);
            dump_expr_node(dumper, right);
        }

        ExprNode::Grouping { expression } => {
            dumper.current_line().push('(');
            dump_expr_node(dumper, expression);
            dumper.current_line().push(')');
        }

        ExprNode::Litteral { value } => {
            dumper.current_line().push_str(&value.lexeme);
        }

        ExprNode::Variable(var) | ExprNode::This(var) => {
            dumper.current_line().push_str(&var.token.lexeme);
        }

        ExprNode::Lambda { params, body } => {
            dumper
                .current_line()
                .write_fmt(format_args!("fun ({}) {{", fun_decl_params(params)))
                .unwrap();
            if !body.is_empty() {
                dumper.depth += 1;
                dump_statement_list(dumper, body);
                dumper.add_line(String::default());
                dumper.depth -= 1;
            }
            dumper.current_line().push('}');
        }

        ExprNode::Call {
            callee,
            right_paren: _,
            arguments,
        } => {
            dump_expr_node(dumper, callee);
            dumper.current_line().push('(');
            for (i, argument) in arguments.iter().enumerate() {
                dump_expr_node(dumper, argument);
                if arguments.len() - 1 > i {
                    dumper.current_line().push_str(", ");
                }
            }
            dumper.current_line().push(')');
        }

        ExprNode::Get(get_expr) => {
            dump_expr_node(dumper, &get_expr.instance);
            dumper.current_line().push('.');
            dumper.current_line().push_str(&get_expr.name.lexeme);
        }

        ExprNode::Set(set_expr) => {
            dump_expr_node(dumper, &set_expr.instance);
            dumper.current_line().push('.');
            dumper.current_line().push_str(&set_expr.name.lexeme);
            dumper.current_line().push_str(" = ");
            dump_expr_node(dumper, &set_expr.value);
        }

        ExprNode::Super(super_expr) => {
            dumper.current_line().push_str("super.");
            dumper.current_line().push_str(&super_expr.method.lexeme);
        }
    }
}

/* ------------ *
 * Dumper state *
 * ------------ */

/// The state of the program dumper.
#[derive(Debug, Default)]
struct Dumper {
    depth: usize,
    lines: Vec<DumperLine>,
}

/// A line generated while dumping the program.
#[derive(Debug)]
struct DumperLine {
    text: String,
    depth: usize,
}

// Convert dumped lines to strings.
impl ToString for DumperLine {
    fn to_string(&self) -> String {
        "\t".repeat(self.depth) + &self.text
    }
}

impl Dumper {
    fn integrate(&mut self, prefix: &str, other: Dumper, suffix: &str) {
        match other.lines.len() {
            0 => self.add_line(format!("{}{}", prefix, suffix)),
            1 => self.add_line(format!("{}{}{}", prefix, other.lines[0].text, suffix)),
            len => {
                other
                    .lines
                    .into_iter()
                    .enumerate()
                    .for_each(|(lnb, mut line)| match lnb {
                        0 => self.add_line(format!("{}{}", prefix, line.text)),
                        l if l == len - 1 => self.add_line(format!("{}{}", line.text, suffix)),
                        _ => {
                            line.depth += self.depth;
                            self.lines.push(line);
                        }
                    });
            }
        }
    }

    fn add_line(&mut self, line: String) {
        self.lines.push(DumperLine {
            text: line,
            depth: self.depth,
        })
    }

    fn current_line(&mut self) -> &mut String {
        let line = self.lines.len() - 1;
        &mut self.lines[line].text
    }
}
