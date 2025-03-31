//! AST definition and parser rules implemented

use std::{error::Error, fmt::Display};

use crate::tokenizer::{Token, TokenTag, keyword::Keyword};

use super::Parser;

/// A node in the abstract syntax tree, represents all possible operations that can occur
#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'src> {
    /// A for loop
    ForLoop {
        /// An inistialization
        init: Box<Expr<'src>>,
        /// The conditional check
        cond: Box<Expr<'src>>,
        /// The update after each run
        inc: Box<Expr<'src>>,
    },
    /// A while loop
    WhileLoop {
        /// The condition checked before running
        condition: Box<Expr<'src>>,
        /// What runs each time around
        eval: Box<Expr<'src>>,
    },
    /// A conditional executor
    Conditional {
        /// The condition being checked
        condition: Box<Expr<'src>>,
        /// What is executed if the condition is true
        true_branch: Box<Expr<'src>>,
        /// What is optionally executed if else
        else_branch: Option<Box<Expr<'src>>>,
    },
    /// A block to be executed
    Block(Vec<Expr<'src>>),
    /// Print an expression's literal result
    Print(Box<Expr<'src>>),
    /// A variable reference
    Variable(&'src str),
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
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Literal<'src> {
    /// String
    String(&'src str),
    /// Real number
    Number(f64),
    /// Boolean
    Bool(bool),
    /// Null, nil, None, etc
    Null,
}

impl Display for Literal<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{b}"),
            Self::String(s) => write!(f, "{s}"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Null => write!(f, "null"),
        }
    }
}

/// All operations that can occur between two targets
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    /// Number negation
    Neg,
    /// Boolean not-ing
    Not,
}

/// All operations that can occur between two targets
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOp {
    /// Add two expressions
    Add,
    /// Subtract two expressions
    Sub,
    /// Multiply
    Mul,
    /// Divide
    Div,
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
pub struct ParseError {
    /// The error message
    pub message: String,
    /// Error token's line
    pub line: usize,
    /// Error token's column
    pub col: usize,
    /// Error token's len
    pub len: usize,
}

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
            TokenTag::Keyword(Keyword::Print) => {
                self.advance();
                self.consume_open_paren_if_necessary()?;
                let next = self.expression()?;
                self.consume_close_paren_if_necessary()?;
                self.consume_end()?;
                Ok(Expr::Print(Box::new(next)))
            }
            TokenTag::Keyword(Keyword::OpenBrace) => self.block(),
            TokenTag::Keyword(Keyword::ConditionalCheck) => self.if_statement(),
            TokenTag::Keyword(Keyword::WhileLoopInit) => self.while_loop(),
            TokenTag::Keyword(Keyword::ForLoopInit) => self.for_loop(),
            _ => {
                let res = self.expression()?;
                self.consume_end()?;
                Ok(res)
            }
        }
    }

    /// A for loop is `for` (statment; expression; statement) block
    fn for_loop(&mut self) -> Result<Expr<'src>, ParseError> {
        todo!()
    }

    /// A while loop is `while` (expression) block
    fn while_loop(&mut self) -> Result<Expr<'src>, ParseError> {
        self.consume(&TokenTag::Keyword(Keyword::WhileLoopInit))?;
        self.consume_open_paren_if_necessary()?;
        let condition = Box::new(self.expression()?);
        self.consume_close_paren_if_necessary()?;
        let eval = Box::new(self.block()?);

        Ok(Expr::WhileLoop { condition, eval })
    }

    /// A block is `{ (expression)* }`
    fn block(&mut self) -> Result<Expr<'src>, ParseError> {
        self.consume(&TokenTag::Keyword(Keyword::OpenBrace))?;
        let mut block_items = vec![];

        while self.peek() != TokenTag::Keyword(Keyword::CloseBrace) {
            let expr = self.statement()?;
            block_items.push(expr);
        }

        self.consume(&TokenTag::Keyword(Keyword::CloseBrace))?;
        Ok(Expr::Block(block_items))
    }

    /// An If statement is:
    /// `if ( equality ) { `block` } ( else { `block` })?`
    fn if_statement(&mut self) -> Result<Expr<'src>, ParseError> {
        self.consume(&TokenTag::Keyword(Keyword::ConditionalCheck))?;
        self.consume_open_paren_if_necessary()?;
        let check = self.equality()?;
        self.consume_close_paren_if_necessary()?;
        let block = self.block()?;

        let mut else_branch = None;

        if self.peek() == TokenTag::Keyword(Keyword::ConditionalElse) {
            self.advance();
            else_branch = Some(Box::new(self.statement()?));
        }

        Ok(Expr::Conditional {
            condition: Box::new(check),
            true_branch: Box::new(block),
            else_branch,
        })
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
                    let Token {
                        tag,
                        line,
                        col,
                        len,
                    } = self.peek_token();
                    Err(ParseError {
                        message: format!("Expected a variable expression, found `{tag:?}`"),
                        line,
                        col,
                        len,
                    })
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

    /// A term is factor ( ( "+" | "-" ) factor )*
    fn term(&mut self) -> Result<Expr<'src>, ParseError> {
        let mut expr = self.factor()?;

        while matches!(self.peek(), TokenTag::Plus | TokenTag::Minus) {
            let op = match self.advance() {
                TokenTag::Plus => BinaryOp::Add,
                TokenTag::Minus => BinaryOp::Sub,
                _ => unreachable!(),
            };

            let right = Box::new(self.factor()?);

            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right,
            };
        }

        Ok(expr)
    }

    /// A factor is unary ( ( "*" | "/" ) unary )*
    fn factor(&mut self) -> Result<Expr<'src>, ParseError> {
        let mut expr = self.unary()?;

        while matches!(self.peek(), TokenTag::Star | TokenTag::Slash) {
            let op = match self.advance() {
                TokenTag::Star => BinaryOp::Mul,
                TokenTag::Slash => BinaryOp::Div,
                _ => unreachable!(),
            };

            let right = Box::new(self.unary()?);

            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right,
            };
        }

        Ok(expr)
    }

    /// A unary expression is either (! | -) unary | primary
    fn unary(&mut self) -> Result<Expr<'src>, ParseError> {
        if matches!(
            self.peek(),
            TokenTag::Keyword(Keyword::Bang) | TokenTag::Minus
        ) {
            let op = match self.advance() {
                TokenTag::Keyword(Keyword::Bang) => UnaryOp::Not,
                TokenTag::Minus => UnaryOp::Neg,
                _ => unreachable!(),
            };

            let unary = self.unary()?;

            Ok(Expr::Unary {
                op,
                node: Box::new(unary),
            })
        } else {
            self.primary()
        }
    }

    /// Variable | NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"
    fn primary(&mut self) -> Result<Expr<'src>, ParseError> {
        let advance = self.advance();
        let prim = match advance {
            TokenTag::Number(n) => Expr::Literal(Literal::Number(n)),
            TokenTag::Keyword(Keyword::True) => Expr::Literal(Literal::Bool(true)),
            TokenTag::Keyword(Keyword::False) => Expr::Literal(Literal::Bool(false)),
            TokenTag::Keyword(Keyword::EmptyValue) => Expr::Literal(Literal::Null),
            TokenTag::Identifier(ident) => Expr::Variable(ident),
            TokenTag::String(s) => Expr::Literal(Literal::String(s)),
            TokenTag::OpenParen => {
                let expr = self.expression()?;
                self.consume(&TokenTag::CloseParen)?;

                Expr::Grouping(Box::new(expr))
            }
            _ => {
                let Token {
                    tag,
                    line,
                    col,
                    len,
                } = self.peek_token();
                return Err(ParseError {
                    message: format!("Expected primary statement, found `{tag:?}`"),
                    line,
                    col,
                    len,
                });
            }
        };

        Ok(prim)
    }
}

/** GOOD TO KNOW
 * These are the mappings for when the seed is 42:
 *
 * Lexer:
 *
 *     "NULL": EmptyValue
 *     "true": True
 *     "then": ConditionalElse
 *     "case": ConditionalCheck
 *     "$": VariableDeclaration
 *     "not": Bang
 *     "and": And
 *     "\\/": Or
 *     "inequal": BangEqual
 *     "<": Less
 *     "{": OpenBrace
 *     "}": CloseBrace
 *     "fmt.Println": Print
 *     "each": ForLoopInit
 *     ">": Greater
 *     "gte": GreaterEqual
 *     ":(": False
 *     "=": Equal
 *     "equals": EqualEqual
 *     "lte": LessEqual
 *     "during": WhileLoopInit
 */

#[cfg(test)]
mod tests {
    use rand::SeedableRng;
    use rand_chacha::ChaCha8Rng;

    use crate::{
        parser::{
            Parser,
            ast::{BinaryOp, Expr, Literal, UnaryOp},
        },
        tokenizer::Tokenizable,
    };

    #[test]
    fn while_loop() {
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        let stream = r#"
during (true) {
    fmt.Println ("yay") .
}
            "#
        .tokenize(&mut rng)
        .expect("Valid tokenization");
        let mut parser = Parser::from_rng(&mut rng).with_tokens(&stream);

        let ast = parser.parse().expect("Parse to AST");

        let expected = [Expr::WhileLoop {
            condition: Box::new(Expr::Literal(Literal::Bool(true))),
            eval: Box::new(Expr::Block(vec![Expr::Print(Box::new(Expr::Literal(
                Literal::String("yay"),
            )))])),
        }];

        assert_eq!(ast, expected);
    }

    #[test]
    fn if_statement() {
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        let stream = r#"
case (true) {
    $ foo = 10 .
}
            "#
        .tokenize(&mut rng)
        .expect("Valid tokenization");
        let mut parser = Parser::from_rng(&mut rng).with_tokens(&stream);

        let ast = parser.parse().expect("Parse to AST");

        let expected = [Expr::Conditional {
            condition: Box::new(Expr::Literal(Literal::Bool(true))),
            true_branch: Box::new(Expr::Block(vec![Expr::Assignment(
                "foo",
                Box::new(Expr::Literal(Literal::Number(10.0))),
            )])),
            else_branch: None,
        }];

        assert_eq!(ast, expected);
    }

    #[test]
    fn variable_assignment() {
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        let stream = "$ i = 0 .".tokenize(&mut rng).expect("Valid tokenization");
        let mut parser = Parser::from_rng(&mut rng).with_tokens(&stream);

        let ast = &parser.parse().expect("Parse to AST")[0];

        assert_eq!(
            ast,
            &Expr::Assignment("i", Box::new(Expr::Literal(Literal::Number(0.0))))
        )
    }

    #[test]
    fn print_statement() {
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        let stream = "fmt.Println(42) ."
            .tokenize(&mut rng)
            .expect("Valid tokenization");
        let mut parser = Parser::from_rng(&mut rng).with_tokens(&stream);

        let ast = &parser.parse().expect("Parse to AST")[0];

        assert_eq!(
            ast,
            &Expr::Print(Box::new(Expr::Literal(Literal::Number(42.0))))
        );
    }

    #[test]
    fn binary_operations() {
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        let stream = "1 + 2 * 3 ."
            .tokenize(&mut rng)
            .expect("Valid tokenization");
        let mut parser = Parser::from_rng(&mut rng).with_tokens(&stream);

        let ast = &parser.parse().expect("Parse to AST")[0];

        assert_eq!(
            ast,
            &Expr::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expr::Literal(Literal::Number(1.0))),
                right: Box::new(Expr::Binary {
                    op: BinaryOp::Mul,
                    left: Box::new(Expr::Literal(Literal::Number(2.0))),
                    right: Box::new(Expr::Literal(Literal::Number(3.0))),
                }),
            }
        );
    }

    #[test]
    fn comparison_operations() {
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        let stream = "1 < 2 .".tokenize(&mut rng).expect("Valid tokenization");
        let mut parser = Parser::from_rng(&mut rng).with_tokens(&stream);

        let ast = &parser.parse().expect("Parse to AST")[0];

        assert_eq!(
            ast,
            &Expr::Binary {
                op: BinaryOp::Lt,
                left: Box::new(Expr::Literal(Literal::Number(1.0))),
                right: Box::new(Expr::Literal(Literal::Number(2.0))),
            }
        );
    }

    #[test]
    fn equality_operations() {
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        let stream = "1 equals 1 ."
            .tokenize(&mut rng)
            .expect("Valid tokenization");
        let mut parser = Parser::from_rng(&mut rng).with_tokens(&stream);

        let ast = &parser.parse().expect("Parse to AST")[0];

        assert_eq!(
            ast,
            &Expr::Binary {
                op: BinaryOp::Eq,
                left: Box::new(Expr::Literal(Literal::Number(1.0))),
                right: Box::new(Expr::Literal(Literal::Number(1.0))),
            }
        );
    }

    #[test]
    fn unary_operations() {
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        let stream = "not true .".tokenize(&mut rng).expect("Valid tokenization");
        let mut parser = Parser::from_rng(&mut rng).with_tokens(&stream);

        let ast = &parser.parse().expect("Parse to AST")[0];

        assert_eq!(
            ast,
            &Expr::Unary {
                op: UnaryOp::Not,
                node: Box::new(Expr::Literal(Literal::Bool(true))),
            }
        );
    }

    #[test]
    fn grouping() {
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        let stream = "(1 + 2) * 3 ."
            .tokenize(&mut rng)
            .expect("Valid tokenization");
        let mut parser = Parser::from_rng(&mut rng).with_tokens(&stream);

        let ast = &parser.parse().expect("Parse to AST")[0];
        let expected = Expr::Binary {
            op: BinaryOp::Mul,
            left: Box::new(Expr::Grouping(Box::new(Expr::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expr::Literal(Literal::Number(1.0))),
                right: Box::new(Expr::Literal(Literal::Number(2.0))),
            }))),
            right: Box::new(Expr::Literal(Literal::Number(3.0))),
        };

        assert_eq!(ast, &expected);
    }

    #[test]
    fn variable_reference() {
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        let stream = "x .".tokenize(&mut rng).expect("Valid tokenization");
        let mut parser = Parser::from_rng(&mut rng).with_tokens(&stream);

        let ast = &parser.parse().expect("Parse to AST")[0];

        assert_eq!(ast, &Expr::Variable("x"));
    }

    #[test]
    fn complex_expression() {
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        let stream = "$ x = 5 * (3 + 2) . fmt.Println(x < 10) ."
            .tokenize(&mut rng)
            .expect("Valid tokenization");
        let mut parser = Parser::from_rng(&mut rng).with_tokens(&stream);

        let ast = parser.parse().expect("Parse to AST");

        assert_eq!(
            ast,
            vec![
                Expr::Assignment(
                    "x",
                    Box::new(Expr::Binary {
                        op: BinaryOp::Mul,
                        left: Box::new(Expr::Literal(Literal::Number(5.0))),
                        right: Box::new(Expr::Grouping(Box::new(Expr::Binary {
                            op: BinaryOp::Add,
                            left: Box::new(Expr::Literal(Literal::Number(3.0))),
                            right: Box::new(Expr::Literal(Literal::Number(2.0))),
                        }))),
                    }),
                ),
                Expr::Print(Box::new(Expr::Binary {
                    op: BinaryOp::Lt,
                    left: Box::new(Expr::Variable("x")),
                    right: Box::new(Expr::Literal(Literal::Number(10.0))),
                })),
            ]
        );
    }
}
