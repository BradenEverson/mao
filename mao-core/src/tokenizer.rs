//! Core tokenizer for the language, defines all `variants` a token may have

use keyword::{Keyword, KeywordRandomizer};
use rand::{RngCore, SeedableRng, thread_rng};
use rand_chacha::ChaCha8Rng;

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
    fn tokenize<RNG: RngCore>(&self, rng: &mut RNG) -> Vec<Token<'_>>;
    /// Creates a token stream with respect to `self`, lifetime should match self's lifetime with
    /// no rng
    fn tokenze_no_rng(&self) -> Vec<Token<'_>> {
        let mut rng = rand::rng();

        self.tokenize(&mut rng)
    }
}

impl<STR> Tokenizable for STR
where
    STR: AsRef<str>,
{
    fn tokenize<RNG: RngCore>(&self, rng: &mut RNG) -> Vec<Token<'_>> {
        let keyword_gen = KeywordRandomizer::seeded_start(rng);

        panic!("{keyword_gen:?}")
    }
}

#[cfg(test)]
mod tests {
    use super::Tokenizable;

    #[test]
    fn basic_tokenizer_test() {
        let mut stream = r#"var i = 0;
var foo = 10;
print("this is a little test")"#
            .tokenze_no_rng();
    }
}
