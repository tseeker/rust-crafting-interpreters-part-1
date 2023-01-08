use crate::tokens::Token;

/* --------- *
 * AST nodes *
 * --------- */

/// The AST node for the program
#[derive(Default, Debug, Clone)]
pub struct ProgramNode(pub Vec<StmtNode>);

/// A function declaration.
#[derive(Debug, Clone)]
pub struct FunDecl {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<StmtNode>,
}

/// A class declaration.
#[derive(Debug, Clone)]
pub struct ClassDecl {
    pub name: Token,
    pub methods: Vec<FunDecl>,
}

/// An AST node that represents a statement.
#[derive(Debug, Clone)]
pub enum StmtNode {
    /// A variable declaration
    VarDecl(Token, Option<ExprNode>),
    /// A function declaration
    FunDecl(FunDecl),
    /// A class declaration
    ClassDecl(ClassDecl),
    /// An single expression
    Expression(ExprNode),
    /// The print statement
    Print(ExprNode),
    /// A block containing multiple statements.
    Block(Vec<StmtNode>),
    /// A conditional statement.
    If {
        condition: ExprNode,
        then_branch: Box<StmtNode>,
        else_branch: Option<Box<StmtNode>>,
    },
    /// Loop statement.
    Loop {
        label: Option<Token>,
        condition: ExprNode,
        body: Box<StmtNode>,
        after_body: Option<Box<StmtNode>>,
    },
    /// Break or continue statement.
    LoopControl {
        is_break: bool,
        loop_name: Option<Token>,
    },
    /// Return statement.
    Return {
        token: Token,
        value: Option<ExprNode>,
    },
}

impl StmtNode {
    /// Extract the list of statements from a block. Panic if the statement
    /// is not a block.
    pub fn extract_block_statements(self) -> Vec<StmtNode> {
        match self {
            Self::Block(stmts) => stmts,
            _ => panic!("Statement is not a block"),
        }
    }
}

/// A getter expression.
#[derive(Debug, Clone)]
pub struct GetExpr {
    /// The instance being accessed.
    pub instance: Box<ExprNode>,
    /// The name of the property.
    pub name: Token,
}

/// An AST node that represents an expression.
#[derive(Debug, Clone)]
pub enum ExprNode {
    /// Assignment to a variable.
    Assignment {
        name: Token,
        value: Box<ExprNode>,
        /// Identifier used for variable resolution.
        id: usize,
    },

    /// Logical binary expression.
    Logical {
        left: Box<ExprNode>,
        operator: Token,
        right: Box<ExprNode>,
    },

    /// Binary expression.
    Binary {
        left: Box<ExprNode>,
        operator: Token,
        right: Box<ExprNode>,
    },

    /// Unary expression.
    Unary {
        operator: Token,
        right: Box<ExprNode>,
    },

    /// Grouping expression, containing a sub-expression.
    Grouping { expression: Box<ExprNode> },

    /// A litteral value, represented by the corresponding token.
    Litteral { value: Token },

    /// A reference to a variable.
    Variable {
        name: Token,
        /// Identifier used for variable resolution.
        id: usize,
    },

    /// A lambda function.
    Lambda {
        params: Vec<Token>,
        body: Vec<StmtNode>,
    },

    /// A function call.
    Call {
        /// Expression that corresponds to the callable.
        callee: Box<ExprNode>,
        /// Right parenthesis that closes the list of arguments. Used to
        /// report errors.
        right_paren: Token,
        /// The list of function arguments.
        arguments: Vec<ExprNode>,
    },

    /// A get expression.
    Get(GetExpr),
}

/* -------------------------------- *
 * Dumper trait and implementations *
 * -------------------------------- */

/// This trait should be implemented by nodes to allow AST dumps.
pub trait AstDumper {
    /// Dump the node as a string.
    fn dump(&self) -> String;
}

impl AstDumper for ProgramNode {
    fn dump(&self) -> String {
        self.0
            .iter()
            .map(|node| node.dump())
            .collect::<Vec<String>>()
            .join(" ")
    }
}

/// Function formatting macro.
macro_rules! fmt_fun_decl {
    ($fs:literal, $fd:expr) => {
        format!(
            $fs,
            $fd.name.lexeme,
            $fd.params
                .iter()
                .map(|token| &token.lexeme as &str)
                .collect::<Vec<&str>>()
                .join(" "),
            $fd.body
                .iter()
                .map(|stmt| stmt.dump())
                .collect::<Vec<String>>()
                .join(" ")
        )
    };
}

impl AstDumper for StmtNode {
    fn dump(&self) -> String {
        match self {
            Self::VarDecl(name, Some(expr)) => format!("( var {} {} )", name.lexeme, expr.dump()),
            Self::VarDecl(name, None) => format!("( var {} nil )", name.lexeme),

            Self::FunDecl(fun_decl) => fmt_fun_decl!("( fun {} ({}) {} )", fun_decl),

            Self::ClassDecl(decl) => format!(
                "( class {} {} )",
                decl.name.lexeme,
                decl.methods
                    .iter()
                    .map(|fun_decl| fmt_fun_decl!("( {} ({}) {} )", fun_decl))
                    .collect::<Vec<String>>()
                    .join(" "),
            ),

            Self::Expression(expr) => expr.dump(),
            Self::Print(expr) => format!("(print {})", expr.dump()),

            Self::Block(stmts) => format!(
                "( {} )",
                stmts
                    .iter()
                    .map(|s| s.dump())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),

            Self::If {
                condition,
                then_branch,
                else_branch,
            } => match else_branch {
                None => format!("( if {} {} () )", condition.dump(), then_branch.dump()),
                Some(stmt) => format!(
                    "( if {} {} {} )",
                    condition.dump(),
                    then_branch.dump(),
                    stmt.dump()
                ),
            },

            Self::Loop {
                label,
                condition,
                body,
                after_body,
            } => {
                let ltxt = if let Some(label) = label {
                    format!("@{} ", label.lexeme)
                } else {
                    "".to_string()
                };
                let abtxt = if let Some(after_body) = after_body {
                    format!("{} ", after_body.dump())
                } else {
                    "".to_string()
                };
                format!(
                    "( {}loop {} {} {})",
                    ltxt,
                    condition.dump(),
                    body.dump(),
                    abtxt
                )
            }

            Self::LoopControl {
                is_break,
                loop_name,
            } => {
                let stmt = if *is_break { "break" } else { "continue" };
                match loop_name {
                    Some(name) => format!("( {} {} )", stmt, name.lexeme),
                    None => format!("( {} )", stmt),
                }
            }

            Self::Return { token: _, value } => match value {
                Some(expr) => format!("( return {} )", expr.dump()),
                None => "( return )".to_owned(),
            },
        }
    }
}

impl AstDumper for ExprNode {
    fn dump(&self) -> String {
        match self {
            Self::Assignment { name, value, id } => {
                format!("{{#{}}}( = {} {} )", id, name.lexeme, value.dump())
            }
            Self::Logical {
                left,
                operator,
                right,
            } => format!("( {} {} {} )", operator.lexeme, left.dump(), right.dump()),
            Self::Binary {
                left,
                operator,
                right,
            } => format!("( {} {} {} )", operator.lexeme, left.dump(), right.dump()),
            Self::Unary { operator, right } => format!("( {} {} )", operator.lexeme, right.dump()),
            Self::Grouping { expression } => format!("( {} )", expression.dump()),
            Self::Variable { name, id } => format!("{{#{}}}{}", id, name.lexeme),
            Self::Litteral { value } => {
                if value.is_litteral() {
                    value.lexeme.clone()
                } else {
                    panic!("Unexpected token type for token {:#?}", value)
                }
            }

            ExprNode::Lambda { params, body } => {
                format!(
                    "( fun ({}) {} )",
                    params
                        .iter()
                        .map(|token| &token.lexeme as &str)
                        .collect::<Vec<&str>>()
                        .join(" "),
                    body.iter()
                        .map(|stmt| stmt.dump())
                        .collect::<Vec<String>>()
                        .join(" ")
                )
            }

            ExprNode::Call {
                callee,
                right_paren: _,
                arguments,
            } => {
                let callee = callee.dump();
                if arguments.is_empty() {
                    format!("( call {} )", callee)
                } else {
                    format!(
                        "( call {} {} )",
                        callee,
                        arguments
                            .iter()
                            .map(|arg| arg.dump())
                            .collect::<Vec<String>>()
                            .join(" ")
                    )
                }
            }

            ExprNode::Get(get_expr) => {
                format!(
                    "( get {} {} )",
                    get_expr.instance.dump(),
                    get_expr.name.lexeme
                )
            }
        }
    }
}
