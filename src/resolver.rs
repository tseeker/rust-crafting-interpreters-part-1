use std::collections::{HashMap, HashSet};

use crate::{
    ast::{ClassMemberDecl, ClassMemberKind, ExprNode, ProgramNode, StmtNode, VariableExpr},
    errors::{ErrorKind, SloxError, SloxResult},
    special,
    tokens::Token,
};

/// Resolved variables. Pointers to the AST nodes using the variables are
/// associated with the relative depth at which the variable definition will be
/// found.
pub type ResolvedVariables = HashMap<usize, usize>;

/// Resolve all variables in a program's AST.
pub fn resolve_variables(program: &ProgramNode) -> SloxResult<ResolvedVariables> {
    let mut state = ResolverState::default();
    state
        .with_scope(|rs| program.resolve(rs), ScopeType::TopLevel)
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
    Class,
    Special,
}

/// The type of a scope
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ScopeType {
    /// The scope is contained by the top-level scope.
    TopLevel,
    /// The scope is contained by a function.
    Function,
    /// The scope is contained by an instance initializer.
    Initializer,
    /// The scope is contained by a method.
    Method,
}

/// The type of a class.
#[derive(Clone, Debug, PartialEq, Eq, Copy)]
enum ClassType {
    /// No class is being visited.
    None,
    /// A top-level class is being visited.
    Class,
    /// A sub-class is being visited.
    Subclass,
}

/// General information about a symbol.
#[derive(Clone, Debug)]
struct SymInfo<'a> {
    decl: Option<&'a Token>,
    kind: SymKind,
    state: SymState,
}

/// A resolver scope.
#[derive(Clone, Debug)]
struct SymScope<'a> {
    /// The type of scope we are in.
    scope_type: ScopeType,
    /// The symbols that are defined inside the current scope.
    symbols: HashMap<String, SymInfo<'a>>,
}

/// The state of the resolver.
#[derive(Default)]
struct ResolverState<'a> {
    /// The stack of scopes. Each scope maps symbols to information which
    /// includes the kind of symbol it is and its current state.
    scopes: Vec<SymScope<'a>>,
    /// The result of the resolver pass.
    resolved: ResolvedVariables,
    /// The type of class being visited.
    current_class_type: ClassType,
}

impl<'a> SymScope<'a> {
    /// Initialize a new scope of the specified type.
    fn new(scope_type: ScopeType) -> Self {
        Self {
            scope_type,
            symbols: HashMap::default(),
        }
    }
}

impl Default for ClassType {
    fn default() -> Self {
        ClassType::None
    }
}

impl<'a> ResolverState<'a> {
    /// Execute some function with a new scope. The scope will be disposed
    /// of after the function has been executed.
    fn with_scope<F>(&mut self, f: F, scope_type: ScopeType) -> ResolverResult
    where
        F: FnOnce(&mut Self) -> ResolverResult,
    {
        self.scopes.push(SymScope::new(scope_type));
        let result = f(self).and_then(|_| self.check_unused());
        self.scopes.pop();
        result
    }

    /// Get the type of the current scope.
    fn current_type(&self) -> ScopeType {
        let pos = self.scopes.len() - 1;
        self.scopes[pos].scope_type
    }

    /// Check for unused symbols in the scope. If an unused symbol is found and
    /// its name does not begin with an underscore, generate an error.
    fn check_unused(&self) -> ResolverResult {
        self.scopes[self.scopes.len() - 1]
            .symbols
            .values()
            .filter(|v| v.state != SymState::Used)
            .filter(|v| v.decl.is_some())
            .find(|v| !v.decl.unwrap().lexeme.starts_with('_'))
            .map_or(Ok(()), |v| {
                self.error(
                    v.decl.unwrap(),
                    "unused symbol; prefix its name with '_' to avoid this error",
                )
            })
    }

    /// Try to declare a symbol. If the scope already contains a declaration
    /// for the same name, return an error.
    fn declare<'b>(&mut self, name: &'b Token, kind: SymKind) -> ResolverResult
    where
        'b: 'a,
    {
        assert!(!self.scopes.is_empty());
        let idx = self.scopes.len() - 1;
        let scope = &mut self.scopes[idx];
        if scope.symbols.contains_key(&name.lexeme as &str) {
            Err(SloxError::with_token(
                ErrorKind::Parse,
                name,
                "already a symbol with this name in this scope".to_owned(),
            ))
        } else {
            scope.symbols.insert(
                name.lexeme.clone(),
                SymInfo {
                    decl: Some(name),
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
        if let Some(info) = top.symbols.get_mut(&name.lexeme as &str) {
            if info.state == SymState::Declared {
                info.state = SymState::Defined;
            }
        }
    }

    /// Declare and define a special symbol (e.g. this) for the current scope.
    fn define_special(&mut self, name: &str) {
        assert!(!self.scopes.is_empty());
        let idx = self.scopes.len() - 1;
        let scope = &mut self.scopes[idx];
        assert!(!scope.symbols.contains_key(name));
        scope.symbols.insert(
            name.to_owned(),
            SymInfo {
                decl: None,
                kind: SymKind::Special,
                state: SymState::Defined,
            },
        );
    }

    /// Resolve a symbol when it is being used. If the symbol is local,
    /// the lookup distance will be stored to the resolution map.
    fn resolve_use(&mut self, expr: &VariableExpr) -> ResolverResult {
        let mut i = self.scopes.len();
        while i != 0 {
            i -= 1;
            if let Some(info) = self.scopes[i].symbols.get_mut(&expr.token.lexeme as &str) {
                if info.state == SymState::Declared {
                    return self.error(&expr.token, "symbol accessed before definition");
                }
                info.state = SymState::Used;
                self.mark_resolved(&expr.id, i);
                return Ok(());
            }
        }
        self.symbol_not_found(&expr.token)
    }

    /// Resolve a symbol when it is being assigned to. If the symbol is local,
    /// the lookup distance will be stored to the resolution map. Trying to
    /// assign to something that isn't a variable will cause an error.
    fn resolve_assignment(&mut self, expr_id: &usize, name: &Token) -> ResolverResult {
        let mut i = self.scopes.len();
        while i != 0 {
            i -= 1;
            if let Some(info) = self.scopes[i].symbols.get_mut(&name.lexeme as &str) {
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
fn resolve_function<'a, 'b>(
    rs: &mut ResolverState<'a>,
    params: &'b [Token],
    body: &'b Vec<StmtNode>,
) -> ResolverResult
where
    'b: 'a,
{
    for param in params {
        rs.declare(param, SymKind::Variable)?;
        rs.define(param);
    }
    // Unlike the original Lox, function arguments and function bodies do
    // not use the same environment.
    rs.with_scope(|rs| body.resolve(rs), rs.current_type())
}

/// Determine which error should be returned if the specified class member is
/// a duplicate.
fn class_member_uniqueness_error(member: &ClassMemberDecl) -> &'static str {
    match (&member.kind, member.is_static) {
        (ClassMemberKind::Method, _) => "duplicate method",
        (ClassMemberKind::Getter, true) => "duplicate static property getter",
        (ClassMemberKind::Getter, false) => "duplicate property getter",
        (ClassMemberKind::Setter, true) => "duplicate static property setter",
        (ClassMemberKind::Setter, false) => "duplicate property setter",
    }
}

/// Check a special class member's characteristics, and return a scope type to be used when
/// resolving that class member.
fn handle_special_members<'a, 'b>(member: &'b ClassMemberDecl) -> SloxResult<ScopeType> {
    if member.kind != ClassMemberKind::Method {
        return Ok(ScopeType::Method);
    }

    let scm = match special::SPECIAL_MEMBER_IDENTIFIERS.get(&member.fun_decl.name.lexeme as &str) {
        None => return Ok(ScopeType::Method),
        Some(scm) => scm,
    };
    if member.is_static {
        return Err(SloxError::with_token(
            ErrorKind::Parse,
            &member.fun_decl.name,
            "special members may not be static".to_owned(),
        ));
    }
    let def = special::SPECIAL_MEMBERS
        .get(scm)
        .expect("Missing special member definition");
    let n_args = member.fun_decl.params.len();
    if n_args < def.min_args {
        return Err(SloxError::with_token(
            ErrorKind::Parse,
            &member.fun_decl.name,
            format!(
                "this special member requires at least {} argument(s)",
                def.min_args
            ),
        ));
    }
    if n_args > def.max_args {
        return Err(SloxError::with_token(
            ErrorKind::Parse,
            &member.fun_decl.name,
            format!(
                "this special member requires at most {} argument(s)",
                def.max_args
            ),
        ));
    }

    Ok(match def.which {
        special::SpecialClassMember::Init => ScopeType::Initializer,
        _ => ScopeType::Method,
    })
}

/// Process a class member's definition. A set is used to identify potential
/// duplicates.
fn resolve_class_member<'a, 'b>(
    rs: &mut ResolverState<'a>,
    member: &'b ClassMemberDecl,
    uniqueness: &mut HashSet<(ClassMemberKind, bool, String)>,
) -> ResolverResult
where
    'b: 'a,
{
    let scope_type = handle_special_members(member)?;
    let static_key = match member.kind {
        ClassMemberKind::Method => false,
        _ => member.is_static,
    };

    if uniqueness.insert((member.kind, static_key, member.fun_decl.name.lexeme.clone())) {
        rs.with_scope(
            |rs| resolve_function(rs, &member.fun_decl.params, &member.fun_decl.body),
            scope_type,
        )
    } else {
        rs.error(&member.fun_decl.name, class_member_uniqueness_error(member))
    }
}

/// Process all method definitions in a class.
fn resolve_class<'a, 'b>(
    rs: &mut ResolverState<'a>,
    has_superclass: bool,
    methods: &'b [ClassMemberDecl],
) -> ResolverResult
where
    'b: 'a,
{
    let mut uniqueness = HashSet::new();
    let mut rc = |rs: &mut ResolverState<'a>| {
        rs.define_special("this");
        methods
            .iter()
            .try_for_each(|member| resolve_class_member(rs, member, &mut uniqueness))
    };
    if has_superclass {
        rs.with_scope(
            |rs| {
                rs.define_special("super");
                rc(rs)
            },
            rs.current_type(),
        )
    } else {
        rc(rs)
    }
}

/// Helper trait used to visit the various AST nodes with the resolver.
trait VarResolver {
    /// Try to resolve local variables under some AST node.
    fn resolve<'a, 'b>(&'a self, rs: &mut ResolverState<'b>) -> ResolverResult
    where
        'a: 'b;
}

impl VarResolver for ProgramNode {
    fn resolve<'a, 'b>(&'a self, rs: &mut ResolverState<'b>) -> ResolverResult
    where
        'a: 'b,
    {
        self.0.resolve(rs)
    }
}

impl VarResolver for Vec<StmtNode> {
    fn resolve<'a, 'b>(&'a self, rs: &mut ResolverState<'b>) -> ResolverResult
    where
        'a: 'b,
    {
        for stmt in self.iter() {
            stmt.resolve(rs)?;
        }
        Ok(())
    }
}

impl VarResolver for StmtNode {
    fn resolve<'a, 'b>(&'a self, rs: &mut ResolverState<'b>) -> ResolverResult
    where
        'a: 'b,
    {
        match self {
            StmtNode::Block(stmts) => rs.with_scope(|rs| stmts.resolve(rs), rs.current_type()),

            StmtNode::VarDecl(name, None) => {
                rs.declare(name, SymKind::Variable)?;
                Ok(())
            }
            StmtNode::VarDecl(name, Some(init)) => {
                rs.declare(name, SymKind::Variable)?;
                init.resolve(rs)?;
                rs.define(name);
                Ok(())
            }

            StmtNode::FunDecl(decl) => {
                rs.declare(&decl.name, SymKind::Function)?;
                rs.define(&decl.name);
                rs.with_scope(
                    |rs| resolve_function(rs, &decl.params, &decl.body),
                    ScopeType::Function,
                )
            }

            StmtNode::ClassDecl(decl) => {
                rs.declare(&decl.name, SymKind::Class)?;
                let enclosing = rs.current_class_type;
                if let Some(superclass) = &decl.superclass {
                    rs.resolve_use(superclass)?;
                    rs.current_class_type = ClassType::Subclass;
                } else {
                    rs.current_class_type = ClassType::Class;
                }
                rs.define(&decl.name);
                let result = rs.with_scope(
                    |rs| resolve_class(rs, decl.superclass.is_some(), &decl.members),
                    rs.current_type(),
                );
                rs.current_class_type = enclosing;
                result
            }

            StmtNode::If {
                condition,
                then_branch,
                else_branch: None,
            } => condition.resolve(rs).and_then(|_| then_branch.resolve(rs)),
            StmtNode::If {
                condition,
                then_branch,
                else_branch: Some(else_branch),
            } => condition
                .resolve(rs)
                .and_then(|_| then_branch.resolve(rs))
                .and_then(|_| else_branch.resolve(rs)),

            StmtNode::Loop {
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

            StmtNode::Return { token, value: None } => match rs.current_type() {
                ScopeType::TopLevel => rs.error(token, "'return' not allowed here"),
                _ => Ok(()),
            },
            StmtNode::Return {
                token,
                value: Some(expr),
            } => match rs.current_type() {
                ScopeType::TopLevel => rs.error(token, "'return' not allowed here"),
                ScopeType::Initializer => {
                    rs.error(token, "'return' with value is not allowed here")
                }
                _ => expr.resolve(rs),
            },

            StmtNode::Expression(expr) => expr.resolve(rs),
            StmtNode::Print(expr) => expr.resolve(rs),
            StmtNode::LoopControl {
                is_break: _,
                loop_name: _,
            } => Ok(()),
        }
    }
}

impl VarResolver for ExprNode {
    fn resolve<'a, 'b>(&'a self, rs: &mut ResolverState<'b>) -> ResolverResult
    where
        'a: 'b,
    {
        match self {
            ExprNode::Variable(var_expr) | ExprNode::This(var_expr) => rs.resolve_use(var_expr),

            ExprNode::Assignment { name, value, id } => {
                value.resolve(rs)?;
                rs.resolve_assignment(id, name)
            }

            ExprNode::Lambda { params, body } => {
                rs.with_scope(|rs| resolve_function(rs, params, body), ScopeType::Function)
            }

            ExprNode::Logical(binary_expr) | ExprNode::Binary(binary_expr) => binary_expr
                .left
                .resolve(rs)
                .and_then(|_| binary_expr.right.resolve(rs)),

            ExprNode::Unary { operator: _, right } => right.resolve(rs),

            ExprNode::Grouping { expression } => expression.resolve(rs),

            ExprNode::Litteral { value: _ } => Ok(()),

            ExprNode::Call {
                callee,
                right_paren: _,
                arguments,
            } => callee.resolve(rs).and_then(|_| arguments.resolve(rs)),

            ExprNode::Get(get_expr) => get_expr.instance.resolve(rs),

            ExprNode::Set(set_expr) => set_expr
                .instance
                .resolve(rs)
                .and_then(|_| set_expr.value.resolve(rs)),

            ExprNode::Super(expr) => match rs.current_class_type {
                ClassType::Subclass => rs.resolve_use(&expr.keyword),
                ClassType::None => {
                    rs.error(&expr.keyword.token, "can't use 'super' outside of a class")
                }
                ClassType::Class => rs.error(
                    &expr.keyword.token,
                    "can't use 'super' in a top-level class",
                ),
            },
        }
    }
}

impl VarResolver for Vec<ExprNode> {
    fn resolve<'a, 'b>(&'a self, rs: &mut ResolverState<'b>) -> ResolverResult
    where
        'a: 'b,
    {
        for expr in self.iter() {
            expr.resolve(rs)?;
        }
        Ok(())
    }
}
