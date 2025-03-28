//! Core tokenizer for the language, defines all `variants` a token may have

use std::{error::Error, fmt::Display};

use keyword::{Keyword, KeywordRandomizer};
use rand::RngCore;

pub mod keyword;

/// A token with respect to it's location in the token stream
#[derive(Debug, Clone, Copy, PartialEq)]
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
#[derive(Debug, Clone, Copy, PartialEq)]
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

/// An error during tokenization
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenizeError {
    /// The invalid message
    message: String,
    /// The line that is invalid
    pub line: usize,
    /// The column in that line that's invalid
    pub col: usize,
}

impl TokenizeError {
    /// Creates a new token error with context
    pub fn new(msg: impl Into<String>, line: usize, col: usize) -> Self {
        Self {
            message: msg.into(),
            line,
            col,
        }
    }
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} at line {}, col {}",
            self.message, self.line, self.col
        )
    }
}

impl Error for TokenizeError {}

/// An result type for tokenizing
pub type Result<T> = std::result::Result<T, TokenizeError>;

/// Any object that can be transformed into a token stream
pub trait Tokenizable {
    /// Creates a token stream with respect to `self`, lifetime should match self's lifetime
    fn tokenize<RNG: RngCore>(&self, rng: &mut RNG) -> Result<Vec<Token<'_>>>;
    /// Creates a token stream with respect to `self`, lifetime should match self's lifetime with
    /// no rng
    fn tokenze_no_rng(&self) -> Result<Vec<Token<'_>>> {
        let mut rng = rand::rng();

        self.tokenize(&mut rng)
    }
}

impl<STR> Tokenizable for STR
where
    STR: AsRef<str>,
{
    fn tokenize<RNG: RngCore>(&self, rng: &mut RNG) -> Result<Vec<Token<'_>>> {
        let keyword_gen = KeywordRandomizer::seeded_start(rng);
        let mut peek = self.as_ref().chars().enumerate().peekable();
        let stream = &self.as_ref();
        let mut tokens = vec![];

        let mut line = 1;
        let mut col = 0;

        while let Some((idx, ch)) = peek.next() {
            let mut len = 1;
            col += 1;

            let tag = if let Some(kwrd) = keyword_gen.try_parse(&stream, idx, &mut len) {
                for _ in 0..len - 1 {
                    peek.next();
                }

                TokenTag::Keyword(kwrd)
            } else {
                match ch {
                    '[' => TokenTag::OpenBracket,
                    ']' => TokenTag::CloseBracket,

                    '(' => TokenTag::OpenParen,
                    ')' => TokenTag::CloseParen,

                    ';' => TokenTag::Semicolon,
                    '.' => TokenTag::Dot,

                    '+' => match peek.peek() {
                        Some((_, '+')) => {
                            peek.next();
                            TokenTag::PlusPlus
                        }
                        Some((_, '=')) => {
                            peek.next();
                            TokenTag::PlusEq
                        }
                        _ => TokenTag::Plus,
                    },
                    '-' => TokenTag::Minus,
                    '*' => TokenTag::Star,
                    '/' => match peek.peek() {
                        Some((_, '/')) => {
                            for (_, ch) in peek.by_ref() {
                                if ch == '\n' {
                                    break;
                                }
                            }
                            continue;
                        }
                        Some((_, '*')) => {
                            peek.next();
                            while let Some((_, ch)) = peek.next() {
                                if ch == '*' {
                                    if let Some((_, '/')) = peek.peek() {
                                        peek.next();
                                        break;
                                    }
                                }
                            }

                            continue;
                        }
                        _ => TokenTag::Slash,
                    },

                    '\n' => {
                        col = 0;
                        line += 1;
                        continue;
                    }

                    ',' => TokenTag::Comma,

                    ws if ws.is_whitespace() => continue,

                    num if num.is_numeric() => {
                        let mut curr = String::new();
                        curr.push(num);

                        let mut dot = false;
                        while let Some((_, next)) = peek.peek() {
                            if next.is_numeric() {
                                col += 1;
                                len += 1;
                                curr.push(peek.next().unwrap().1);
                            } else if *next == '.' && !dot {
                                col += 1;
                                len += 1;
                                curr.push(peek.next().unwrap().1);
                                dot = true;
                            } else {
                                break;
                            }
                        }

                        // Unwrap safety, as we build the number we are ensuring that only numeric
                        // characters are added to it, this cannot fail
                        TokenTag::Number(curr.parse().unwrap())
                    }

                    '"' => {
                        let mut idx2 = idx;
                        let mut ended = false;

                        for (_, c) in peek.by_ref() {
                            if c != '"' {
                                idx2 += 1;
                                col += 1;
                                len += 1;
                            } else {
                                ended = true;
                                break;
                            }
                        }

                        if ended {
                            TokenTag::String(&self.as_ref()[idx + 1..=idx2])
                        } else {
                            return Err(TokenizeError::new(
                                r#"Expected End of String: `"`"#,
                                line,
                                col,
                            ));
                        }
                    }

                    ch if ch.is_alphanumeric() || ch == '_' => {
                        let mut end = idx;

                        while let Some((idx2, next)) = peek.peek() {
                            if !(next.is_alphanumeric() || *next == '_') {
                                break;
                            }

                            end = *idx2;
                            col += 1;
                            len += 1;
                            peek.next();
                        }

                        let word = &self.as_ref()[idx..=end];
                        if let Err(Some(was)) = keyword_gen.try_from_str(word) {
                            return Err(TokenizeError::new(
                                format!("Invalid keyword {word}, did you mean {was}?"),
                                line,
                                col,
                            ));
                        } else {
                            TokenTag::Identifier(word)
                        }
                    }
                    bad => {
                        return Err(TokenizeError::new(
                            format!("Invalid token {bad}"),
                            line,
                            col,
                        ));
                    }
                }
            };

            let next = Token {
                tag,
                col,
                len,
                line,
            };

            tokens.push(next);
        }

        tokens.push(Token {
            line,
            col,
            len: 0,
            tag: TokenTag::EOF,
        });

        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use rand::SeedableRng;
    use rand_chacha::ChaCha8Rng;

    use crate::tokenizer::keyword::Keyword;

    use super::{Token, TokenTag, Tokenizable};

    fn toks<'a>(tokens: Vec<Token<'a>>) -> Vec<TokenTag<'a>> {
        tokens.into_iter().map(|token| token.tag).collect()
    }

    #[test]
    fn basic_tokenizer_test() {
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        let stream = r#"$ i = 0;
$ foo = 10;
fmt.Println("this is a little test");"#
            .tokenize(&mut rng)
            .expect("Valid tokenization");

        let toks = toks(stream);
        let expected = [
            TokenTag::Keyword(Keyword::VariableDeclaration),
            TokenTag::Identifier("i"),
            TokenTag::Keyword(Keyword::Equal),
            TokenTag::Number(0.0),
            TokenTag::Semicolon,
            TokenTag::Keyword(Keyword::VariableDeclaration),
            TokenTag::Identifier("foo"),
            TokenTag::Keyword(Keyword::Equal),
            TokenTag::Number(10.0),
            TokenTag::Semicolon,
            TokenTag::Keyword(Keyword::Print),
            TokenTag::OpenParen,
            TokenTag::String("this is a little test"),
            TokenTag::CloseParen,
            TokenTag::Semicolon,
            TokenTag::EOF,
        ];

        assert_eq!(toks, expected)
    }
}
