use crate::tokens::Token;

/* --------- *
 * AST nodes *
 * --------- */

/// The AST node for the program
#[derive(Default, Debug, Clone)]
pub struct ProgramNode(pub Vec<StmtNode>);

/// An AST node that represents a statement.
#[derive(Debug, Clone)]
pub enum StmtNode {
    /// A variable declaration
    VarDecl(Token, Option<ExprNode>),
    /// An single expression
    Expression(ExprNode),
    /// The print statement
    Print(ExprNode),
    /// A block containing multiple statements.
    Block(Vec<Box<StmtNode>>),
    /// A conditional statement.
    IfStmt {
        condition: ExprNode,
        then_branch: Box<StmtNode>,
        else_branch: Option<Box<StmtNode>>,
    },
    /// While loop statement.
    WhileStmt {
        label: Option<Token>,
        condition: ExprNode,
        body: Box<StmtNode>,
    },
    /// Break or continue statement.
    LoopControlStmt {
        is_break: bool,
        loop_name: Option<Token>,
    },
}

/// An AST node that represents an expression.
#[derive(Debug, Clone)]
pub enum ExprNode {
    /// Assignment to a variable.
    Assignment { name: Token, value: Box<ExprNode> },

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
    Variable { name: Token },
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

impl AstDumper for StmtNode {
    fn dump(&self) -> String {
        match self {
            Self::VarDecl(name, Some(expr)) => format!("( var {} {} )", name.lexeme, expr.dump()),
            Self::VarDecl(name, None) => format!("( var {} nil )", name.lexeme),
            Self::Expression(expr) => format!("{}", expr.dump()),
            Self::Print(expr) => format!("(print {})", expr.dump()),

            Self::Block(stmts) => format!(
                "( {} )",
                stmts
                    .iter()
                    .map(|s| s.dump())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),

            Self::IfStmt {
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

            Self::WhileStmt {
                label,
                condition,
                body,
            } => {
                let ltxt = if let Some(label) = label {
                    format!("@{} ", label.lexeme)
                } else {
                    "".to_string()
                };
                format!("( {}while {} {} )", ltxt, condition.dump(), body.dump())
            }

            Self::LoopControlStmt {
                is_break,
                loop_name,
            } => {
                let stmt = if *is_break { "break" } else { "continue" };
                match loop_name {
                    Some(name) => format!("( {} {} )", stmt, name.lexeme),
                    None => format!("( {} )", stmt),
                }
            }
        }
    }
}

impl AstDumper for ExprNode {
    fn dump(&self) -> String {
        match self {
            Self::Assignment { name, value } => format!("( = {} {} )", name.lexeme, value.dump()),
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
            Self::Variable { name } => name.lexeme.clone(),
            Self::Litteral { value } => {
                if value.is_litteral() {
                    value.lexeme.clone()
                } else {
                    panic!("Unexpected token type for token {:#?}", value)
                }
            }
        }
    }
}
