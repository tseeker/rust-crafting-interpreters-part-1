use std::collections::HashMap;

use crate::{
    ast,
    errors::{ErrorKind, SloxError, SloxResult},
    tokens::Token,
};

/// Resolved variables. Pointers to the AST nodes using the variables are
/// associated with the relative depth at which the variable definition will be
/// found.
pub type ResolvedVariables = HashMap<usize, usize>;

/// Resolve all variables in a program's AST.
pub fn resolve_variables(program: &ast::ProgramNode) -> SloxResult<ResolvedVariables> {
    let mut state = ResolverState::default();
    state
        .with_scope(|rs| program.resolve(rs))
        .map(|_| state.resolved)
}

type ResolverResult = SloxResult<()>;

/// The state of a symbol in a scope.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SymState {
    /// The symbol has been declared but no value has been assigned to it.
    Declared,
    /// The symbol has been defined, but it hasn't been accessed.
    Defined,
    /// The symbol has been used.
    Used,
}

/// The kind of a symbol.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SymKind {
    Variable,
    Function,
}

/// General information about a symbol.
#[derive(Clone, Debug)]
struct SymInfo {
    decl: Token,
    kind: SymKind,
    state: SymState,
}

/// The state of the resolver.
#[derive(Default)]
struct ResolverState {
    /// The stack of scopes. Each scope maps symbols to information which
    /// includes the kind of symbol it is and its current state.
    scopes: Vec<HashMap<String, SymInfo>>,
    /// The result of the resolver pass.
    resolved: ResolvedVariables,
}

impl ResolverState {
    /// Execute some function with a new scope. The scope will be disposed
    /// of after the function has been executed.
    fn with_scope<F>(&mut self, f: F) -> ResolverResult
    where
        F: FnOnce(&mut Self) -> ResolverResult,
    {
        self.scopes.push(HashMap::new());
        let result = f(self).and_then(|_| self.check_unused());
        self.scopes.pop();
        result
    }

    /// Check for unused symbols in the scope. If an unused symbol is found and
    /// its name does not begin with an underscore, generate an error.
    fn check_unused(&self) -> ResolverResult {
        self.scopes[self.scopes.len() - 1]
            .values()
            .filter(|v| v.state != SymState::Used)
            .filter(|v| !v.decl.lexeme.starts_with("_"))
            .nth(0)
            .map_or(Ok(()), |v| {
                self.error(
                    &v.decl,
                    "unused symbol; prefix its name with '_' to avoid this error",
                )
            })
    }

    /// Try to declare a symbol. If the scope already contains a declaration
    /// for the same name, return an error.
    fn declare(&mut self, name: &Token, kind: SymKind) -> ResolverResult {
        assert!(!self.scopes.is_empty());
        let idx = self.scopes.len() - 1;
        let scope = &mut self.scopes[idx];
        if scope.contains_key(&name.lexeme as &str) {
            Err(SloxError::with_token(
                ErrorKind::Parse,
                name,
                "already a symbol with this name in this scope".to_owned(),
            ))
        } else {
            scope.insert(
                name.lexeme.clone(),
                SymInfo {
                    decl: name.clone(),
                    kind,
                    state: SymState::Declared,
                },
            );
            Ok(())
        }
    }

    /// Mark a symbol as defined. If the symbol has already been defined or
    /// used, its state isn't affected.
    fn define(&mut self, name: &Token) {
        assert!(!self.scopes.is_empty());
        let idx = self.scopes.len() - 1;
        let top = &mut self.scopes[idx];
        if let Some(info) = top.get_mut(&name.lexeme as &str) {
            if info.state == SymState::Declared {
                info.state = SymState::Defined;
            }
        }
    }

    /// Resolve a symbol when it is being used. If the symbol is local,
    /// the lookup distance will be stored to the resolution map.
    fn resolve_use(&mut self, expr_id: &usize, name: &Token) -> ResolverResult {
        let mut i = self.scopes.len();
        while i != 0 {
            i -= 1;
            if let Some(info) = self.scopes[i].get_mut(&name.lexeme as &str) {
                if info.state == SymState::Declared {
                    return self.error(name, "symbol accessed before definition");
                }
                info.state = SymState::Used;
                self.mark_resolved(expr_id, i);
                return Ok(());
            }
        }
        self.symbol_not_found(name)
    }

    /// Resolve a symbol when it is being assigned to. If the symbol is local,
    /// the lookup distance will be stored to the resolution map. Trying to
    /// assign to something that isn't a variable will cause an error.
    fn resolve_assignment(&mut self, expr_id: &usize, name: &Token) -> ResolverResult {
        let mut i = self.scopes.len();
        while i != 0 {
            i -= 1;
            if let Some(info) = self.scopes[i].get_mut(&name.lexeme as &str) {
                if info.kind != SymKind::Variable {
                    return self.error(name, "cannot assign to this symbol");
                }
                if info.state == SymState::Declared {
                    info.state = SymState::Defined;
                }
                self.mark_resolved(expr_id, i);
                return Ok(());
            }
        }
        self.symbol_not_found(name)
    }

    /// Add an entry to the resolution map for an AST node.
    fn mark_resolved(&mut self, expr_id: &usize, depth: usize) {
        // Only mark symbols as locals if we're not at the top-level scope.
        if depth != 0 {
            self.resolved
                .insert(*expr_id, self.scopes.len() - 1 - depth);
        }
    }

    /// Return an error corresponding to an undeclared symbol.
    fn symbol_not_found(&mut self, name: &Token) -> ResolverResult {
        self.error(name, "undeclared symbol")
    }

    /// Return an error.
    fn error(&self, name: &Token, message: &str) -> ResolverResult {
        Err(SloxError::with_token(
            ErrorKind::Parse,
            name,
            message.to_owned(),
        ))
    }
}

/// Process a function declaration.
fn resolve_function(
    rs: &mut ResolverState,
    params: &[Token],
    body: &Vec<ast::StmtNode>,
) -> ResolverResult {
    for param in params {
        rs.declare(param, SymKind::Variable)?;
        rs.define(param);
    }
    // Unlike the original Lox, function arguments and function bodies do
    // not use the same environment.
    rs.with_scope(|rs| body.resolve(rs))
}

/// Helper trait used to visit the various AST nodes with the resolver.
trait VarResolver {
    /// Try to resolve local variables under some AST node.
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
            ast::StmtNode::Block(stmts) => rs.with_scope(|rs| stmts.resolve(rs)),

            ast::StmtNode::VarDecl(name, None) => {
                rs.declare(name, SymKind::Variable)?;
                Ok(())
            }
            ast::StmtNode::VarDecl(name, Some(init)) => {
                rs.declare(name, SymKind::Variable)?;
                init.resolve(rs)?;
                rs.define(name);
                Ok(())
            }

            ast::StmtNode::FunDecl { name, params, body } => {
                rs.declare(name, SymKind::Function)?;
                rs.define(name);
                rs.with_scope(|rs| resolve_function(rs, params, body))
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
            ast::ExprNode::Variable { name, id } => rs.resolve_use(id, name),

            ast::ExprNode::Assignment { name, value, id } => {
                value.resolve(rs)?;
                rs.resolve_assignment(id, name)
            }

            ast::ExprNode::Lambda { params, body } => {
                rs.with_scope(|rs| resolve_function(rs, params, body))
            }

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
