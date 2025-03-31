//! Core interpetter

use std::{
    collections::HashMap,
    error::Error,
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};

use crate::parser::ast::{BinaryOp, Expr, Literal, UnaryOp};

/// An AST interpretter
#[derive(Debug, Default, Clone)]
pub struct Interpretter<'a> {
    /// Local variables
    context: HashMap<String, Literal<'a>>,
}

/// An error during execution
#[derive(Debug, Clone, Copy)]
pub struct RuntimeError;

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Runtime Error")
    }
}

impl Error for RuntimeError {}

impl<'a> Interpretter<'a> {
    /// Interprets an AST
    pub fn eval(&mut self, ast: &Expr<'a>) -> Result<Literal<'a>, RuntimeError> {
        match ast {
            Expr::ForLoop {
                init,
                cond,
                inc,
                exec,
            } => {
                self.eval(init)?;
                while self.eval(cond)?.bool()? {
                    self.eval(exec)?;
                    self.eval(inc)?;
                }

                Ok(Literal::Null)
            }
            Expr::WhileLoop { condition, eval } => {
                while self.eval(condition)?.bool()? {
                    self.eval(eval)?;
                }

                Ok(Literal::Null)
            }
            Expr::Conditional {
                condition,
                true_branch,
                else_branch,
            } => {
                let condition = self.eval(condition)?.bool()?;

                if condition {
                    self.eval(true_branch)
                } else if let Some(else_branch) = else_branch {
                    self.eval(else_branch)
                } else {
                    Ok(Literal::Null)
                }
            }
            Expr::Block(all) => {
                for val in all {
                    self.eval(val)?;
                }

                Ok(Literal::Null)
            }
            Expr::Variable(var) => Ok(self.context[*var]),
            Expr::Print(node) => {
                println!("{}", self.eval(node)?);
                Ok(Literal::Null)
            }
            Expr::Assignment(name, val) => {
                let val = self.eval(val)?;
                self.context.insert(name.to_string(), val);
                Ok(Literal::Null)
            }
            Expr::Literal(l) => Ok(*l),
            Expr::Grouping(inner) => self.eval(inner),
            Expr::Binary { op, left, right } => op.eval(self.eval(left)?, self.eval(right)?),
            Expr::Unary { op, node } => op.eval(self.eval(node)?),
        }
    }
}

impl BinaryOp {
    /// Evaluates a binary operation
    pub fn eval<'a>(
        &self,
        left: Literal<'a>,
        right: Literal<'a>,
    ) -> Result<Literal<'a>, RuntimeError> {
        match self {
            Self::Add => left + right,
            Self::Sub => left - right,
            Self::Mul => left * right,
            Self::Div => left / right,

            Self::Gt => Ok(Literal::Bool(left.number()? > right.number()?)),
            Self::Gte => Ok(Literal::Bool(left.number()? >= right.number()?)),
            Self::Lt => Ok(Literal::Bool(left.number()? < right.number()?)),
            Self::Lte => Ok(Literal::Bool(left.number()? <= right.number()?)),

            Self::Eq => left.equals(&right),
            Self::Neq => left.not_equals(&right),
        }
    }
}

impl UnaryOp {
    /// Evaluates a unary operation
    pub fn eval<'a>(&self, node: Literal<'a>) -> Result<Literal<'a>, RuntimeError> {
        match self {
            Self::Neg => Ok(Literal::Number(-node.number()?)),
            Self::Not => Ok(Literal::Bool(!node.bool()?)),
        }
    }
}

impl Add for Literal<'_> {
    type Output = Result<Self, RuntimeError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => Ok(Literal::Number(n1 + n2)),

            _ => Err(RuntimeError),
        }
    }
}

impl Sub for Literal<'_> {
    type Output = Result<Self, RuntimeError>;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => Ok(Self::Number(n1 - n2)),

            _ => Err(RuntimeError),
        }
    }
}

impl Mul for Literal<'_> {
    type Output = Result<Self, RuntimeError>;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => Ok(Self::Number(n1 * n2)),

            _ => Err(RuntimeError),
        }
    }
}

impl Div for Literal<'_> {
    type Output = Result<Self, RuntimeError>;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => Ok(Self::Number(n1 / n2)),
            _ => Err(RuntimeError),
        }
    }
}

impl<'a> Literal<'a> {
    /// Returns the inner literal if it's a proper unsigned integer truly, asserting a runtime
    /// error if not
    pub fn uint(&self) -> Result<usize, RuntimeError> {
        match self {
            Self::Number(n) if *n >= 0.0 && n.round() == *n => Ok(*n as usize),
            _ => Err(RuntimeError),
        }
    }
    /// Returns the inner literal if it's numeric, asserting a runtime error if not
    pub fn number(&self) -> Result<f64, RuntimeError> {
        match self {
            Self::Number(n) => Ok(*n),
            _ => Err(RuntimeError),
        }
    }

    /// Returns the inner literal if it's boolean, asserting a runtime error if not
    pub fn bool(&self) -> Result<bool, RuntimeError> {
        match self {
            Self::Bool(val) => Ok(*val),
            Self::Number(0.0) => Ok(false),
            Self::Number(_) => Ok(true),
            _ => Err(RuntimeError),
        }
    }

    /// Returns the True literal if the two literals are losely equal
    pub fn equals(&self, other: &Self) -> Result<Literal<'a>, RuntimeError> {
        match (self, other) {
            (Self::Number(n1), Self::Number(n2)) => Ok(Self::Bool(n1 == n2)),
            (Self::Bool(b1), Self::Bool(b2)) => Ok(Self::Bool(b1 == b2)),

            (Self::Null, Self::Null) => Ok(Self::Bool(true)),

            (crazy1, crazy2) => Ok(Literal::Bool(crazy1.to_string() == crazy2.to_string())),
        }
    }

    /// Returns the True literal if the two literals are not losely equal
    pub fn not_equals(&self, other: &Self) -> Result<Literal<'a>, RuntimeError> {
        match (self, other) {
            (Self::Number(n1), Self::Number(n2)) => Ok(Self::Bool(n1 != n2)),
            (Self::Bool(b1), Self::Bool(b2)) => Ok(Self::Bool(b1 != b2)),

            (Self::Null, Self::Null) => Ok(Self::Bool(false)),

            (crazy1, crazy2) => Ok(Literal::Bool(crazy1.to_string() != crazy2.to_string())),
        }
    }
}
