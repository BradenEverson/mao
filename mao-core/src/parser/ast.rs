//! AST definition and parser rules implemented

use std::{error::Error, fmt::Display};

use crate::tokenizer::{TokenTag, keyword::Keyword};

use super::Parser;

/// A node in the abstract syntax tree, represents all possible operations that can occur
pub enum Expr<'src> {
    /// An assignment from an identifier to an expression
    Assignment(&'src str, Box<Expr<'src>>),
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
    /// Equality
    Eq,
    /// Inequality
    Neq,
    /// Greater than
    Gt,
    /// Greater than or equal to
    Gte,
    /// Less than
    Lt,
    /// Less than or equal to
    Lte,
}

/// An error that occurs whilst parsing
#[derive(Clone, PartialEq, Debug, Default)]
pub struct ParseError;

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "parser :(")
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
        match self.peek() {
            TokenTag::Keyword(Keyword::Print) => todo!("printing"),
            _ => {
                let res = self.expression()?;
                self.consume_end()?;
                Ok(res)
            }
        }
    }

    /// an expression is equality  | var ident = equality
    fn expression(&mut self) -> Result<Expr<'src>, ParseError> {
        match self.peek() {
            TokenTag::Keyword(Keyword::VariableDeclaration) => {
                self.advance();
                if let TokenTag::Identifier(name) = self.advance() {
                    self.consume(&TokenTag::Keyword(Keyword::Equal))?;
                    let assignment = self.equality()?;

                    Ok(Expr::Assignment(name, Box::new(assignment)))
                } else {
                    Err(ParseError)
                }
            }

            _ => self.equality(),
        }
    }

    /// An equality is comparison ( (!= | ==) comparison)*
    fn equality(&mut self) -> Result<Expr<'src>, ParseError> {
        let mut expr = self.comparison()?;

        while matches!(
            self.peek(),
            TokenTag::Keyword(Keyword::EqualEqual) | TokenTag::Keyword(Keyword::BangEqual)
        ) {
            let op = match self.advance() {
                TokenTag::Keyword(Keyword::EqualEqual) => BinaryOp::Eq,
                TokenTag::Keyword(Keyword::BangEqual) => BinaryOp::Neq,
                _ => unreachable!(),
            };

            let right = Box::new(self.comparison()?);

            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right,
            }
        }

        Ok(expr)
    }

    /// A comparison is term ( ( ">" | ">=" | "<" | "<=" ) term )*
    fn comparison(&mut self) -> Result<Expr<'src>, ParseError> {
        let mut expr = self.term()?;

        while matches!(
            self.peek(),
            TokenTag::Keyword(Keyword::Greater)
                | TokenTag::Keyword(Keyword::GreaterEqual)
                | TokenTag::Keyword(Keyword::Less)
                | TokenTag::Keyword(Keyword::LessEqual)
        ) {
            let op = match self.advance() {
                TokenTag::Keyword(Keyword::Greater) => BinaryOp::Gt,
                TokenTag::Keyword(Keyword::GreaterEqual) => BinaryOp::Gte,
                TokenTag::Keyword(Keyword::Less) => BinaryOp::Lt,
                TokenTag::Keyword(Keyword::LessEqual) => BinaryOp::Lte,
                _ => unreachable!(),
            };

            let right = Box::new(self.term()?);

            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right,
            }
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr<'src>, ParseError> {
        todo!()
    }

    fn factor(&mut self) -> Result<Expr<'src>, ParseError> {
        todo!()
    }

    fn unary(&mut self) -> Result<Expr<'src>, ParseError> {
        todo!()
    }

    fn primary(&mut self) -> Result<Expr<'src>, ParseError> {
        todo!()
    }
}
