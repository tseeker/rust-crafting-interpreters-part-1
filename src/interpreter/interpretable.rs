use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{ClassDecl, ExprNode, FunDecl, GetExpr, ProgramNode, SetExpr, StmtNode, VariableExpr},
    errors::{ErrorKind, SloxError, SloxResult},
    resolver::ResolvedVariables,
    tokens::{Token, TokenType},
};

use super::{
    classes::{Class, ClassMemberKey},
    functions::Function,
    Environment, EnvironmentRef, Value,
};

/// Evaluate an interpretable, returning its value.
pub fn evaluate(ast: &ProgramNode, vars: ResolvedVariables) -> SloxResult<Value> {
    let mut state = InterpreterState::new(&vars);
    ast.interpret(&mut state).map(|v| v.result())
}

/* ------- *
 * HELPERS *
 * ------- */

/// The state of the interpreter.
#[derive(Debug)]
pub struct InterpreterState<'a> {
    pub(super) globals: EnvironmentRef,
    pub(super) environment: EnvironmentRef,
    pub(super) locals: &'a ResolvedVariables,
}

impl<'a> InterpreterState<'a> {
    /// Initialize the interpreter state from the resolved variables map.
    fn new(locals: &'a ResolvedVariables) -> Self {
        let env = Rc::new(RefCell::new(Environment::default()));
        Self {
            environment: env.clone(),
            globals: env,
            locals,
        }
    }

    /// Create a child state.
    pub(super) fn create_child<'b>(parent: &InterpreterState<'b>) -> Self
    where
        'b: 'a,
    {
        InterpreterState {
            environment: Environment::create_child(&parent.environment),
            globals: parent.globals.clone(),
            locals: parent.locals,
        }
    }

    fn lookup_var(&self, expr: &VariableExpr) -> SloxResult<Value> {
        match self.locals.get(&expr.id) {
            Some(distance) => self.environment.borrow().get_at(*distance, &expr.token),
            None => self.globals.borrow().get(&expr.token),
        }
    }

    fn assign_var(&self, name: &Token, expr_id: &usize, value: Value) -> SloxResult<()> {
        match self.locals.get(expr_id) {
            Some(distance) => self
                .environment
                .borrow_mut()
                .assign_at(*distance, name, value),
            None => self.globals.borrow_mut().assign(name, value),
        }
    }
}

/// Interpreter flow control, which may be either a value, a loop break or a
/// loop continuation.
#[derive(Debug)]
pub(super) enum InterpreterFlowControl {
    Result(Value),
    Break(Option<String>),
    Continue(Option<String>),
    Return(Value),
}

impl InterpreterFlowControl {
    /// Return the result's value. If the flow control value does not represent
    /// a result, panic.
    pub(super) fn result(self) -> Value {
        match self {
            Self::Result(v) => v,
            other => panic!("Result expected, {:?} found instead", other),
        }
    }

    /// Check whether a flow control value contains actual flow control
    /// information.
    fn is_flow_control(&self) -> bool {
        matches!(self, Self::Break(_) | Self::Continue(_) | Self::Return(_))
    }
}

impl Default for InterpreterFlowControl {
    fn default() -> Self {
        Self::Result(Value::Nil)
    }
}

impl From<Value> for InterpreterFlowControl {
    fn from(value: Value) -> Self {
        Self::Result(value)
    }
}

/// A result returned by some part of the interpreter.
pub(super) type InterpreterResult = SloxResult<InterpreterFlowControl>;

/// An Interpretable can be evaluated and will return a value.
pub(super) trait Interpretable {
    fn interpret(&self, es: &mut InterpreterState) -> InterpreterResult;
}

/// Generate an error with a static message.
fn error<T>(token: &Token, message: &str) -> SloxResult<T> {
    Err(SloxError::with_token(
        ErrorKind::Runtime,
        token,
        message.to_owned(),
    ))
}

/* ----------------------------- *
 * INTERPRETER FOR PROGRAM NODES *
 * ----------------------------- */

impl Interpretable for ProgramNode {
    fn interpret(&self, es: &mut InterpreterState) -> InterpreterResult {
        for stmt in self.0.iter() {
            stmt.interpret(es)?;
        }
        Ok(InterpreterFlowControl::default())
    }
}

/* ------------------------------- *
 * INTERPRETER FOR STATEMENT NODES *
 * ------------------------------- */

impl Interpretable for StmtNode {
    fn interpret(&self, es: &mut InterpreterState) -> InterpreterResult {
        match self {
            StmtNode::VarDecl(name, expr) => self.on_var_decl(es, name, expr),
            StmtNode::FunDecl(decl) => self.on_fun_decl(es, decl),
            StmtNode::ClassDecl(decl) => self.on_class_decl(es, decl),
            StmtNode::Expression(expr) => expr.interpret(es),
            StmtNode::Print(expr) => self.on_print(es, expr),
            StmtNode::Block(statements) => self.on_block(es, statements),
            StmtNode::If {
                condition,
                then_branch,
                else_branch,
            } => self.on_if_statement(es, condition, then_branch, else_branch),
            StmtNode::Loop {
                label,
                condition,
                body,
                after_body,
            } => self.on_loop_statement(es, label, condition, body, after_body),
            StmtNode::LoopControl {
                is_break,
                loop_name,
            } => self.on_loop_control_statemement(*is_break, loop_name),
            StmtNode::Return { token: _, value } => self.on_return_statement(es, value),
        }
    }
}

/// Extract members from a class declaration, generating a map of
/// functions.
fn extract_members(
    es: &mut InterpreterState,
    decl: &ClassDecl,
) -> HashMap<ClassMemberKey, Function> {
    decl.members
        .iter()
        .map(|member| {
            let fnd = &member.fun_decl;
            (
                (member.kind, member.is_static, fnd.name.lexeme.clone()),
                Function::new(
                    Some(&fnd.name),
                    &fnd.params,
                    &fnd.body,
                    es.environment.clone(),
                    fnd.name.lexeme == "init",
                ),
            )
        })
        .collect()
}

impl StmtNode {
    /// Handle the `print` statement.
    fn on_print(&self, es: &mut InterpreterState, expr: &ExprNode) -> InterpreterResult {
        let value = expr.interpret(es)?.result();
        let output = value.to_string();
        println!("{}", output);
        Ok(InterpreterFlowControl::default())
    }

    /// Handle a variable declaration.
    fn on_var_decl(
        &self,
        es: &mut InterpreterState,
        name: &Token,
        initializer: &Option<ExprNode>,
    ) -> InterpreterResult {
        let variable = match initializer {
            Some(expr) => Some(expr.interpret(es)?.result()),
            None => None,
        };
        es.environment.borrow_mut().define(name, variable)?;
        Ok(InterpreterFlowControl::default())
    }

    /// Handle a class declaration
    fn on_class_decl(&self, es: &mut InterpreterState, decl: &ClassDecl) -> InterpreterResult {
        es.environment.borrow_mut().define(&decl.name, None)?;
        let class = match &decl.superclass {
            None => Class::new(decl.name.lexeme.clone(), None, extract_members(es, decl)),
            Some(superclass) => {
                let sc_value = superclass.interpret(es)?.result();
                let sc_ref = if let Some(sc_ref) = sc_value.as_class_ref() {
                    Some(sc_ref)
                } else {
                    return error(&superclass.token, "superclass must be a class");
                };
                let mut sub_env = InterpreterState::create_child(es);
                sub_env
                    .environment
                    .borrow_mut()
                    .set("super", sc_value.clone());
                Class::new(
                    decl.name.lexeme.clone(),
                    sc_ref,
                    extract_members(&mut sub_env, decl),
                )
            }
        };
        es.environment
            .borrow_mut()
            .assign(&decl.name, class.into())?;
        Ok(InterpreterFlowControl::default())
    }

    /// Handle a function declaration.
    fn on_fun_decl(&self, es: &mut InterpreterState, decl: &FunDecl) -> InterpreterResult {
        let fun = Function::new(
            Some(&decl.name),
            &decl.params,
            &decl.body,
            es.environment.clone(),
            false,
        );
        es.environment
            .borrow_mut()
            .define(&decl.name, Some(fun.into()))?;
        Ok(InterpreterFlowControl::default())
    }

    /// Execute the contents of a block.
    fn on_block(&self, es: &mut InterpreterState, stmts: &[StmtNode]) -> InterpreterResult {
        let mut child = InterpreterState::create_child(es);
        for stmt in stmts.iter() {
            let result = stmt.interpret(&mut child)?;
            if result.is_flow_control() {
                return Ok(result);
            }
        }
        Ok(InterpreterFlowControl::default())
    }

    /// Execute an if statement.
    fn on_if_statement(
        &self,
        es: &mut InterpreterState,
        condition: &ExprNode,
        then_branch: &StmtNode,
        else_branch: &Option<Box<StmtNode>>,
    ) -> InterpreterResult {
        if condition.interpret(es)?.result().is_truthy() {
            then_branch.interpret(es)
        } else if let Some(else_stmt) = else_branch {
            else_stmt.interpret(es)
        } else {
            Ok(InterpreterFlowControl::default())
        }
    }

    /// Execute a while statement.
    fn on_loop_statement(
        &self,
        es: &mut InterpreterState,
        label: &Option<Token>,
        condition: &ExprNode,
        body: &StmtNode,
        after_body: &Option<Box<StmtNode>>,
    ) -> InterpreterResult {
        let ln = label.as_ref().map(|token| token.lexeme.clone());
        while condition.interpret(es)?.result().is_truthy() {
            let result = body.interpret(es)?;
            match &result {
                InterpreterFlowControl::Result(_) => (),
                InterpreterFlowControl::Continue(lv) if lv == &ln => (),
                InterpreterFlowControl::Break(lv) if lv == &ln => break,
                _ => return Ok(result),
            }
            if let Some(stmt) = after_body {
                let result = stmt.interpret(es)?;
                match &result {
                    InterpreterFlowControl::Result(_) => (),
                    InterpreterFlowControl::Continue(lv) if lv == &ln => (),
                    InterpreterFlowControl::Break(lv) if lv == &ln => break,
                    _ => return Ok(result),
                }
            }
        }
        Ok(InterpreterFlowControl::default())
    }

    /// Execute a loop control statement.
    fn on_loop_control_statemement(
        &self,
        is_break: bool,
        label: &Option<Token>,
    ) -> InterpreterResult {
        let name = label.as_ref().map(|token| token.lexeme.clone());
        if is_break {
            Ok(InterpreterFlowControl::Break(name))
        } else {
            Ok(InterpreterFlowControl::Continue(name))
        }
    }

    /// Execute a return statement.
    fn on_return_statement(
        &self,
        es: &mut InterpreterState,
        value: &Option<ExprNode>,
    ) -> InterpreterResult {
        let rv = match value {
            None => Value::Nil,
            Some(expr) => expr.interpret(es)?.result(),
        };
        Ok(InterpreterFlowControl::Return(rv))
    }
}

/* -------------------------------- *
 * INTERPRETER FOR EXPRESSION NODES *
 * -------------------------------- */

impl Interpretable for ExprNode {
    fn interpret(&self, es: &mut InterpreterState) -> InterpreterResult {
        match self {
            ExprNode::Assignment { name, value, id } => {
                let value = value.interpret(es)?.result();
                es.assign_var(name, id, value)?;
                Ok(InterpreterFlowControl::default())
            }
            ExprNode::Logical(binary_expr) => self.on_logic(
                es,
                &binary_expr.left,
                &binary_expr.operator,
                &binary_expr.right,
            ),
            ExprNode::Binary(binary_expr) => self.on_binary(
                es,
                &binary_expr.left,
                &binary_expr.operator,
                &binary_expr.right,
            ),
            ExprNode::Unary { operator, right } => self.on_unary(es, operator, right),
            ExprNode::Grouping { expression } => expression.interpret(es),
            ExprNode::Litteral { value } => self.on_litteral(value),
            ExprNode::Variable(var_expr) | ExprNode::This(var_expr) => var_expr.interpret(es),
            ExprNode::Call {
                callee,
                right_paren,
                arguments,
            } => self.on_call(es, callee, right_paren, arguments),
            ExprNode::Lambda { params, body } => {
                let lambda = Function::new(None, params, body, es.environment.clone(), false);
                Ok(Value::from(lambda).into())
            }
            ExprNode::Get(get_expr) => self.on_get_expression(es, get_expr),
            ExprNode::Set(set_expr) => self.on_set_expression(es, set_expr),
            ExprNode::Super(_) => todo!(),
        }
    }
}

impl Interpretable for VariableExpr {
    fn interpret(&self, es: &mut InterpreterState) -> InterpreterResult {
        Ok(es.lookup_var(self)?.into())
    }
}

impl ExprNode {
    /// Evaluate a logical operator.
    fn on_logic(
        &self,
        es: &mut InterpreterState,
        left: &ExprNode,
        operator: &Token,
        right: &ExprNode,
    ) -> InterpreterResult {
        let left_value = left.interpret(es)?.result();
        if operator.token_type == TokenType::Or && left_value.is_truthy()
            || operator.token_type == TokenType::And && !left_value.is_truthy()
        {
            Ok(left_value.into())
        } else {
            right.interpret(es)
        }
    }

    /// Evaluate a binary operator.
    fn on_binary(
        &self,
        es: &mut InterpreterState,
        left: &ExprNode,
        operator: &Token,
        right: &ExprNode,
    ) -> InterpreterResult {
        let left_value = left.interpret(es)?.result();
        let right_value = right.interpret(es)?.result();
        match operator.token_type {
            TokenType::Plus => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b).into()),
                (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b).into()),
                _ => error(operator, "type error"),
            },

            TokenType::Minus => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b).into()),
                _ => error(operator, "type error"),
            },

            TokenType::Star => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b).into()),
                (Value::String(a), Value::Number(b)) => {
                    Ok(Value::String(a.repeat(b as usize)).into())
                }
                _ => error(operator, "type error"),
            },

            TokenType::Slash => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => {
                    if b == 0. {
                        error(operator, "division by zero")
                    } else {
                        Ok(Value::Number(a / b).into())
                    }
                }
                _ => error(operator, "type error"),
            },

            TokenType::Greater => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b).into()),
                _ => error(operator, "type error"),
            },

            TokenType::GreaterEqual => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a >= b).into()),
                _ => error(operator, "type error"),
            },

            TokenType::Less => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a < b).into()),
                _ => error(operator, "type error"),
            },

            TokenType::LessEqual => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a <= b).into()),
                _ => error(operator, "type error"),
            },

            TokenType::EqualEqual => Ok(Value::Boolean(left_value == right_value).into()),
            TokenType::BangEqual => Ok(Value::Boolean(left_value != right_value).into()),

            _ => panic!(
                "Unsupported token type for binary operator (token {:?})",
                operator
            ),
        }
    }

    /// Evaluate an unary operator.
    fn on_unary(
        &self,
        es: &mut InterpreterState,
        operator: &Token,
        right: &ExprNode,
    ) -> InterpreterResult {
        let right_value = right.interpret(es)?.result();
        match operator.token_type {
            TokenType::Minus => {
                if let Value::Number(n) = right_value {
                    Ok(Value::Number(-n).into())
                } else {
                    error(operator, "number expected")
                }
            }

            TokenType::Bang => Ok(Value::Boolean(!right_value.is_truthy()).into()),

            _ => panic!(
                "Unsupported token type for unary operator (token {:?})",
                operator
            ),
        }
    }

    /// Evaluate a litteral.
    fn on_litteral(&self, value: &Token) -> InterpreterResult {
        let out_value = match &value.token_type {
            TokenType::Nil => Value::Nil,
            TokenType::True => Value::Boolean(true),
            TokenType::False => Value::Boolean(false),
            TokenType::Number(n) => Value::Number(*n),
            TokenType::String(s) => Value::String(s.clone()),
            _ => panic!("Unsupported token type for litteral (token {:?})", value),
        };
        Ok(out_value.into())
    }

    /// Evaluate a function call.
    fn on_call(
        &self,
        es: &mut InterpreterState,
        callee: &ExprNode,
        right_paren: &Token,
        arguments: &Vec<ExprNode>,
    ) -> InterpreterResult {
        let callee = callee.interpret(es)?.result();
        let arg_values = {
            let mut v = Vec::with_capacity(arguments.len());
            for argument in arguments.iter() {
                v.push(argument.interpret(es)?.result());
            }
            v
        };
        callee.with_callable(
            |callable| {
                if callable.arity() != arg_values.len() {
                    Err(SloxError::with_token(
                        ErrorKind::Runtime,
                        right_paren,
                        format!(
                            "expected {} arguments, found {}",
                            arg_values.len(),
                            callable.arity()
                        ),
                    ))
                } else {
                    Ok(callable.call(es, arg_values)?.into())
                }
            },
            || error(right_paren, "expression result is not callable"),
        )
    }

    /// Evaluate a get expression.
    fn on_get_expression(
        &self,
        itpr_state: &mut InterpreterState,
        get_expr: &GetExpr,
    ) -> InterpreterResult {
        let instance = get_expr.instance.interpret(itpr_state)?.result();
        instance.with_property_carrier(
            |inst| inst.get(itpr_state, &get_expr.name).map(|v| v.into()),
            || error(&get_expr.name, "this object doesn't have properties"),
        )
    }

    /// Evaluate a set expression.
    fn on_set_expression(
        &self,
        itpr_state: &mut InterpreterState,
        set_expr: &SetExpr,
    ) -> InterpreterResult {
        let instance = set_expr.instance.interpret(itpr_state)?.result();
        instance.with_property_carrier(
            |instance| {
                let value = set_expr.value.interpret(itpr_state)?.result();
                instance.set(itpr_state, &set_expr.name, value.clone());
                Ok(value.into())
            },
            || error(&set_expr.name, "this object doesn't have properties"),
        )
    }
}
