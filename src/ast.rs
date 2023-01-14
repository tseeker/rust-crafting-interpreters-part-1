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

/// The declaration of a class member.
#[derive(Debug, Clone)]
pub enum ClassMemberDecl {
    Method(FunDecl),
    StaticMethod(FunDecl),
}

/// A class declaration.
#[derive(Debug, Clone)]
pub struct ClassDecl {
    /// The token that represents the name of the class.
    pub name: Token,
    /// The list of class members.
    pub members: Vec<ClassMemberDecl>,
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

/// A setter expression.
#[derive(Debug, Clone)]
pub struct SetExpr {
    /// The instance being accessed.
    pub instance: Box<ExprNode>,
    /// The name of the property.
    pub name: Token,
    /// The value to set
    pub value: Box<ExprNode>,
}

/// A binary expression.
#[derive(Debug, Clone)]
pub struct BinaryExpr {
    /// The left side expression
    pub left: Box<ExprNode>,
    /// The operator
    pub operator: Token,
    /// The right side expression
    pub right: Box<ExprNode>,
}

/// A variable reference expression.
#[derive(Debug, Clone)]
pub struct VariableExpr {
    /// The name of the variable, or the "this" keyword.
    pub token: Token,
    /// Identifier used for variable resolution.
    pub id: usize,
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
    Logical(BinaryExpr),

    /// Binary expression.
    Binary(BinaryExpr),

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
    Variable(VariableExpr),
    /// The "this" keyword.
    This(VariableExpr),

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
    /// A set expression.
    Set(SetExpr),
}
