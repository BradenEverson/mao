//! The parser for generating an abstract syntax tree, built in with conditional rules

use crate::tokenizer::Token;

pub struct Parser<'tok> {
    /// The token stream to parse
    tokens: &'tok [Token<'tok>],
    /// Index into the token stream
    idx: usize,
    /// Dynamic parser rules
    rules: ParserRules<'tok>,
}

/// Dynamic parsing rules that affect how the AST is generated
pub struct ParserRules<'tok> {
    pub statements_end_with: Token<'tok>,
    pub parenthesis: bool,
}
