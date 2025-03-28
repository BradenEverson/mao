//! AST definition and parser rules implemented

use std::{error::Error, fmt::Display};

use crate::tokenizer::{Token, TokenTag};

use super::Parser;

/// A node in the abstract syntax tree, represents all possible operations that can occur
pub enum Expr<'src> {
    /// A binary operation between two expressions
    Binary {
        /// The operator
        op: BinaryOp,
        /// Left hand side
        left: Box<Expr<'src>>,
        /// Right hand side
        right: Box<Expr<'src>>,
    },
    /// A unary operation on a single expression
    Unary {
        /// The operator
        op: UnaryOp,
        /// The expression being acted on
        node: Box<Expr<'src>>,
    },
    /// ( `expr` )
    Grouping(Box<Expr<'src>>),
    /// A literal
    Literal(Literal<'src>),
}

/// A literal type
pub enum Literal<'src> {
    /// String
    String(&'src str),
    /// Real number
    Number(f32),
    /// Boolean
    Bool(bool),
}

/// All operations that can occur between two targets
pub enum UnaryOp {
    /// Number negation
    Neg,
    /// Boolean not-ing
    Not,
}

/// All operations that can occur between two targets
pub enum BinaryOp {
    /// Add two expressions
    Add,
    /// Subtract two expressions
    Sub,
}

/// An error that occurs whilst parsing
#[derive(Clone, PartialEq, Debug, Default)]
pub struct ParseError {
    /// The error message
    pub message: String,
    /// Line of the error
    pub line: usize,
    /// Column of the error
    pub col: usize,
    /// Length of the error
    pub len: usize,
}

impl From<(&str, Token<'_>)> for ParseError {
    fn from(value: (&str, Token<'_>)) -> Self {
        let (
            want,
            Token {
                line,
                col,
                len,
                tag,
            },
        ) = value;
        Self {
            message: format!("Expected `{want}` after `{tag:?}`"),
            line,
            col,
            len,
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "parser error: {}", self.message)
    }
}

impl Error for ParseError {}

impl<'src> Parser<'src> {
    /// Parse a series of statements
    pub fn parse(&mut self) -> Result<Vec<Expr<'src>>, ParseError> {
        let mut statements = vec![];

        while self.peek() != TokenTag::EOF {
            statements.push(self.statement()?);
        }

        Ok(statements)
    }

    /// Parse a singular statement
    pub fn statement(&mut self) -> Result<Expr<'src>, ParseError> {
        todo!()
    }
}
