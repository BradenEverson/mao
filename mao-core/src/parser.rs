//! The parser for generating an abstract syntax tree, built in with conditional rules

use ast::ParseError;
use rand::{RngCore, seq::IndexedRandom};

use crate::tokenizer::{Token, TokenTag};

pub mod ast;

/// All valid statement ends
pub const VALID_ENDS: &[TokenTag<'static>] = &[
    TokenTag::Semicolon,
    TokenTag::Dot,
    TokenTag::Identifier("done"),
];

/// It's a 2/3 chance that you need parenthesis
pub const PAREN_PROB: &[bool] = &[true, true, false];

/// The core parser with dynamic rules
pub struct Parser<'tok> {
    /// The token stream to parse
    tokens: &'tok [Token<'tok>],
    /// Index into the token stream
    idx: usize,
    /// Dynamic parser rules
    rules: ParserRules,
}

/// Dynamic parsing rules that affect how the AST is generated
pub struct ParserRules {
    /// What should a statement end with? (;, ., !, something crazy :O)
    pub statements_end_with: TokenTag<'static>,
    /// Should parenthesis exist in if statements, loops and function calls?
    pub parenthesis: bool,
}

impl<'tok> Parser<'tok> {
    /// Creates a new parser from RNG
    pub fn from_rng<RNG: RngCore>(rng: &mut RNG) -> Self {
        // Unwrap safety, VALID_ENDS is not empty that would be silly
        let statements_end_with = *VALID_ENDS.choose(rng).unwrap();
        // Unwrap safety, PAREN_PROB is not empty that would be silly
        let parenthesis = *PAREN_PROB.choose(rng).unwrap();

        let rules = ParserRules {
            statements_end_with,
            parenthesis,
        };

        Self {
            tokens: &[],
            idx: 0,
            rules,
        }
    }

    /// Assigns a token stream to the parser
    pub fn with_tokens(mut self, tokens: &'tok [Token<'tok>]) -> Self {
        self.tokens = tokens;

        self
    }

    /// Peeks at the current token
    #[inline]
    pub fn peek(&self) -> TokenTag<'tok> {
        self.tokens[self.idx.min(self.tokens.len() - 1)].tag
    }

    /// Peeks at the previous token, does not advance the index
    #[inline]
    pub fn peek_back(&self) -> TokenTag<'tok> {
        self.tokens[self.idx - 1].tag
    }

    /// Checks if stream is finished
    fn at_end(&self) -> bool {
        self.peek() == TokenTag::EOF
    }

    /// Advances forward and returns the previous token
    pub fn advance(&mut self) -> TokenTag<'tok> {
        if !self.at_end() {
            self.idx += 1
        }

        if self.idx == 0 {
            self.tokens[0].tag
        } else {
            self.tokens[self.idx - 1].tag
        }
    }

    /// Consumes the current token assuming it's the provided Token, failing if not
    pub fn consume(&mut self, token: &TokenTag<'_>) -> Result<(), ParseError> {
        if &self.peek() == token {
            self.idx += 1;
            Ok(())
        } else {
            Err(ParseError)
        }
    }

    /// Consumes the proper end for the tree
    pub fn consume_end(&mut self) -> Result<(), ParseError> {
        let tok = self.rules.statements_end_with;
        self.consume(&tok)
    }
}
