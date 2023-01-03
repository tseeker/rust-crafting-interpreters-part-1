use std::collections::HashMap;

use crate::{ast, errors::ParserError, tokens::Token};

pub fn resolve_variables(program: &ast::ProgramNode) -> ResolverResult {
    let mut state = ResolverState::default();
    program.resolve(&mut state)?;
    Ok(())
}

type ResolverResult = Result<(), ParserError>;

#[derive(Default)]
struct ResolverState {
    scopes: Vec<HashMap<String, bool>>,
    resolved: HashMap<*const ast::ExprNode, usize>,
}

impl ResolverState {
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) {
        if !self.scopes.is_empty() {
            let idx = self.scopes.len() - 1;
            let top = &mut self.scopes[idx];
            top.insert(name.lexeme.clone(), false);
        }
    }

    fn define(&mut self, name: &Token) {
        if !self.scopes.is_empty() {
            let idx = self.scopes.len() - 1;
            let top = &mut self.scopes[idx];
            top.insert(name.lexeme.clone(), true);
        }
    }

    fn check(&self, name: &str) -> Option<bool> {
        if self.scopes.is_empty() {
            None
        } else {
            let idx = self.scopes.len() - 1;
            self.scopes[idx].get(name).cloned()
        }
    }

    fn resolve_local(&mut self, expr: &ast::ExprNode, name: &Token) {
        let start = self.scopes.len() - 1;
        let mut i = start;
        while i != 0 {
            if self.scopes[i].contains_key(&name.lexeme as &str) {
                self.mark_resolved(expr, self.scopes.len() - 1 - i);
                return;
            }
            i -= 1;
        }
    }

    fn resolve_function(&mut self, params: &[Token], body: &Vec<ast::StmtNode>) -> ResolverResult {
        self.begin_scope();
        for param in params {
            self.declare(param);
            self.define(param);
        }
        let result = body.resolve(self);
        self.end_scope();
        result
    }

    fn mark_resolved(&mut self, expr: &ast::ExprNode, depth: usize) {
        self.resolved.insert(expr as *const ast::ExprNode, depth);
    }
}

trait VarResolver {
    fn resolve(&self, rs: &mut ResolverState) -> ResolverResult;
}

impl VarResolver for ast::ProgramNode {
    fn resolve(&self, rs: &mut ResolverState) -> ResolverResult {
        self.0.resolve(rs)
    }
}

impl VarResolver for Vec<ast::StmtNode> {
    fn resolve(&self, rs: &mut ResolverState) -> ResolverResult {
        for stmt in self.iter() {
            stmt.resolve(rs)?;
        }
        Ok(())
    }
}

impl VarResolver for ast::StmtNode {
    fn resolve(&self, rs: &mut ResolverState) -> ResolverResult {
        match self {
            ast::StmtNode::Block(stmts) => {
                rs.begin_scope();
                let result = stmts.resolve(rs);
                rs.end_scope();
                result
            }

            ast::StmtNode::VarDecl(name, None) => {
                rs.declare(name);
                rs.define(name);
                Ok(())
            }
            ast::StmtNode::VarDecl(name, Some(init)) => {
                rs.declare(name);
                init.resolve(rs)?;
                rs.define(name);
                Ok(())
            }

            ast::StmtNode::FunDecl { name, params, body } => {
                rs.declare(name);
                rs.define(name);
                rs.resolve_function(params, body)
            }

            ast::StmtNode::If {
                condition,
                then_branch,
                else_branch: None,
            } => condition.resolve(rs).and_then(|_| then_branch.resolve(rs)),
            ast::StmtNode::If {
                condition,
                then_branch,
                else_branch: Some(else_branch),
            } => condition
                .resolve(rs)
                .and_then(|_| then_branch.resolve(rs))
                .and_then(|_| else_branch.resolve(rs)),

            ast::StmtNode::Loop {
                label: _,
                condition,
                body,
                after_body,
            } => condition
                .resolve(rs)
                .and_then(|_| {
                    if let Some(stmt) = after_body {
                        stmt.resolve(rs)
                    } else {
                        Ok(())
                    }
                })
                .and_then(|_| body.resolve(rs)),

            ast::StmtNode::Return {
                token: _,
                value: None,
            } => Ok(()),
            ast::StmtNode::Return {
                token: _,
                value: Some(expr),
            } => expr.resolve(rs),

            ast::StmtNode::Expression(expr) => expr.resolve(rs),
            ast::StmtNode::Print(expr) => expr.resolve(rs),
            ast::StmtNode::LoopControl {
                is_break: _,
                loop_name: _,
            } => Ok(()),
        }
    }
}

impl VarResolver for ast::ExprNode {
    fn resolve(&self, rs: &mut ResolverState) -> ResolverResult {
        match self {
            ast::ExprNode::Variable { name } => {
                if rs.check(&name.lexeme) == Some(false) {
                    Err(ParserError::new(
                        name,
                        "can't read local variable in its own initializer",
                    ))
                } else {
                    rs.resolve_local(self, name);
                    Ok(())
                }
            }

            ast::ExprNode::Assignment { name, value } => {
                value.resolve(rs)?;
                rs.resolve_local(self, name);
                Ok(())
            }

            ast::ExprNode::Lambda { params, body } => rs.resolve_function(params, body),

            ast::ExprNode::Logical {
                left,
                operator: _,
                right,
            } => left.resolve(rs).and_then(|_| right.resolve(rs)),
            ast::ExprNode::Binary {
                left,
                operator: _,
                right,
            } => left.resolve(rs).and_then(|_| right.resolve(rs)),
            ast::ExprNode::Unary { operator: _, right } => right.resolve(rs),
            ast::ExprNode::Grouping { expression } => expression.resolve(rs),
            ast::ExprNode::Litteral { value: _ } => Ok(()),
            ast::ExprNode::Call {
                callee,
                right_paren: _,
                arguments,
            } => callee.resolve(rs).and_then(|_| arguments.resolve(rs)),
        }
    }
}

impl VarResolver for Vec<ast::ExprNode> {
    fn resolve(&self, rs: &mut ResolverState) -> ResolverResult {
        for expr in self.iter() {
            expr.resolve(rs)?;
        }
        Ok(())
    }
}
