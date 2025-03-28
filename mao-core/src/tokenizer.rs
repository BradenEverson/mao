//! Core tokenizer for the language, defines all `variants` a token may have

use keyword::Keyword;

pub mod keyword;

/// A token with respect to it's location in the token stream
pub struct Token<'src> {
    /// The tag
    pub tag: TokenTag<'src>,
    /// Line
    pub line: usize,
    /// Column in line
    pub col: usize,
    /// Length of token
    pub len: usize,
}

/// All tags for a token
pub enum TokenTag<'src> {
    /// An identifier
    Identifier(&'src str),
    /// A numeric literal
    Number(f64),
    /// A string literal
    String(&'src str),
    /// A keyword
    Keyword(Keyword),
    /// (
    OpenParen,
    /// )
    CloseParen,
    /// [
    OpenBracket,
    /// ]
    CloseBracket,
    /// ;
    Semicolon,
    /// +
    Plus,
    /// ++
    PlusPlus,
    /// +=
    PlusEq,
    /// -
    Minus,
    /// *
    Star,
    ///,
    Comma,
    /// .
    Dot,
    /// /
    Slash,

    /// End of file
    EOF,
}

/// Any object that can be transformed into a token stream
pub trait Tokenizable {
    /// Creates a token stream with respect to `self`, lifetime should match self's lifetime
    fn tokenize(&self) -> Vec<Token<'_>>;
}
