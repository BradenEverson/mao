//! Keyword definitions and global randomizer

use std::collections::HashMap;

use rand::{RngCore, seq::IndexedRandom};

/// All valid keywords (they change a lot so these enum names are the "root" of what they represent)
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    /// and
    And,
    /// false
    False,
    /// true
    True,
    /// for
    ForLoopInit,
    /// if
    ConditionalCheck,
    /// else
    ConditionalElse,
    /// nil, null, None
    EmptyValue,
    /// or
    Or,
    /// print to standard out
    Print,
    /// var
    VariableDeclaration,
    /// while
    WhileLoopInit,

    /// {
    OpenBrace,
    /// }
    CloseBrace,

    /// =
    Equal,
    /// >
    Greater,
    /// >=
    GreaterEqual,
    /// ==
    EqualEqual,
    /// !=
    BangEqual,
    /// !
    Bang,
    /// <
    Less,
    /// <=
    LessEqual,
}

/// All valid assignment variants
const EQUAL_VARIANTS: &[&str] = &["=", "equals", "is"];
/// All valid > variants
const GREATER_VARIANTS: &[&str] = &[">", "gt", "greater"];
/// All valid >= variants
const GREATEREQUAL_VARIANTS: &[&str] = &[">=", "gte"];
/// All valid `==` variants
const EQUALEQUAL_VARIANTS: &[&str] = &["==", "equals"];
/// All valid `!=` variants
const BANGEQUAL_VARIANTS: &[&str] = &["!=", "inequal"];
/// All valid `!` variants
const BANG_VARIANTS: &[&str] = &["!", "not"];
/// All valid `<` variants
const LESS_VARIANTS: &[&str] = &["<", "lt", "less"];
/// All valid `<=` variants
const LESSEQUAL_VARIANTS: &[&str] = &["<=", "lte"];

/// All valid `{` variants
const OPEN_BRACE_VARIANTS: &[&str] = &["{", ":"];
/// All valid `}` variants
const CLOSE_BRACE_VARIANTS: &[&str] = &["}", "end"];

/// All valid `and` variants
const AND_VARIANTS: &[&str] = &["and", "&&", r#"/\"#, "alongside"];
/// All valid `or` variants
const OR_VARIANTS: &[&str] = &["or", "||", r#"\/"#];

/// All valid `false` variants
const FALSE_VARIANTS: &[&str] = &["false", "False", "FALSE", "incorrect", "nah", ":("];
/// All valid `true` variants
const TRUE_VARIANTS: &[&str] = &["true", "True", "TRUE", "correct", "yah", ":)"];

/// All valid `for` variants
const FOR_VARIANTS: &[&str] = &["for", "each"];
/// All valid `if` variants
const IF_VARIANTS: &[&str] = &["if", "case", "check", "cond"];
/// All valid `else` variants
const ELSE_VARIANTS: &[&str] = &["else", "then", "otherwise"];
/// All valid `nil` variants
const EMPTY_VARIANTS: &[&str] = &["nil", "None", "null", "NULL", "undefined", "void"];

/// All valid printing variants
const PRINT_VARIANTS: &[&str] = &[
    "print",
    "puts",
    "echo",
    "Console.WriteLine",
    "std::cout",
    "System.out.println",
    "println",
    "fmt.Println",
    "console.log",
    "say",
];

/// All valid `while` variants
const WHILE_VARIANTS: &[&str] = &["while", "during", "whilst", "until", "as_long_as", "🔁"];

/// All valid `var` variants
const VARIABLE_DECLARATION_VARIANTS: &[&str] = &["var", "let", "auto", "$", "val", "new"];

impl Keyword {
    /// Gets all bindings to keyword variants
    pub fn all() -> Vec<(Keyword, &'static [&'static str])> {
        vec![
            (Self::And, AND_VARIANTS),
            (Self::Or, OR_VARIANTS),
            (Self::True, TRUE_VARIANTS),
            (Self::False, FALSE_VARIANTS),
            (Self::ForLoopInit, FOR_VARIANTS),
            (Self::ConditionalCheck, IF_VARIANTS),
            (Self::ConditionalElse, ELSE_VARIANTS),
            (Self::EmptyValue, EMPTY_VARIANTS),
            (Self::Print, PRINT_VARIANTS),
            (Self::WhileLoopInit, WHILE_VARIANTS),
            (Self::VariableDeclaration, VARIABLE_DECLARATION_VARIANTS),
            (Self::OpenBrace, OPEN_BRACE_VARIANTS),
            (Self::CloseBrace, CLOSE_BRACE_VARIANTS),
            (Self::Equal, EQUAL_VARIANTS),
            (Self::Greater, GREATER_VARIANTS),
            (Self::GreaterEqual, GREATEREQUAL_VARIANTS),
            (Self::EqualEqual, EQUALEQUAL_VARIANTS),
            (Self::BangEqual, BANGEQUAL_VARIANTS),
            (Self::Bang, BANG_VARIANTS),
            (Self::Less, LESS_VARIANTS),
            (Self::LessEqual, LESSEQUAL_VARIANTS),
        ]
    }
}

/// Struct for maintaining "sanity" across random generation, upon initialization it holds the
/// current runs definitions for what each keyword maps to. This keeps syntax consistent in a
/// single run, so variable declaration isn't both `var` and `let` in the same runtime for example
#[derive(Debug)]
pub struct KeywordRandomizer {
    /// The keyword context
    ctx: HashMap<&'static str, Keyword>,
}

impl KeywordRandomizer {
    /// Tries to parse a keyword from a stream
    pub fn try_parse(&self, stream: &str, idx: usize, len: &mut usize) -> Option<Keyword> {
        let characters: Vec<_> = stream.chars().collect();
        let mut idx2 = idx;

        while !characters[idx2].is_whitespace() && idx2 < characters.len() - 1 {
            idx2 += 1
        }

        while idx <= idx2 {
            let pot_kwrd = &stream[idx..=idx2];
            if let Ok(keyword) = self.try_from_str(pot_kwrd) {
                *len = idx2 - idx + 1;
                return Some(keyword);
            }

            if idx2 == 0 {
                break;
            }

            idx2 -= 1;
        }

        None
    }

    /// Seeds all keywords
    pub fn seeded_start<RNG: RngCore>(rng: &mut RNG) -> Self {
        let mut ctx = HashMap::new();

        for (root, variants) in Keyword::all() {
            // unwrap safety, variants are all non-empty
            let selected = variants.choose(rng).unwrap();
            ctx.insert(*selected, root);
        }

        Self { ctx }
    }

    /// Attempts to parse a string into a keyword, if the string is valid for a Keyword but not the
    /// proper variant, the correct variant for this runtime is returned as an error
    pub fn try_from_str(&self, from: &str) -> Result<Keyword, Option<&'static str>> {
        if let Some(keyword) = self.ctx.get(from) {
            Ok(*keyword)
        } else {
            for (root, variants) in Keyword::all() {
                if variants.contains(&from) {
                    if let Some(proper_variant) = self
                        .ctx
                        .iter()
                        .find(|(_, kwrd)| **kwrd == root)
                        .map(|(var, _)| *var)
                    {
                        return Err(Some(proper_variant));
                    }
                }
            }

            Err(None)
        }
    }
}

#[cfg(test)]
mod tests {
    use rand::SeedableRng;
    use rand_chacha::ChaCha8Rng;

    use crate::tokenizer::keyword::Keyword;

    use super::KeywordRandomizer;

    #[test]
    fn try_from_string() {
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        let keywords = KeywordRandomizer::seeded_start(&mut rng);

        let attempt = keywords.try_from_str("var");

        assert_eq!(attempt.unwrap_err(), Some("$"))
    }

    #[test]
    fn try_parse_bottom_up() {
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        let mut keywords = KeywordRandomizer::seeded_start(&mut rng);
        let mut len = 0;
        keywords.ctx.insert(">=", Keyword::GreaterEqual);

        let attempt = keywords.try_parse(">=", 0, &mut len);
        assert_eq!(attempt.unwrap(), Keyword::GreaterEqual);
        assert_eq!(len, 2);

        let attempt = keywords.try_parse(" >=", 1, &mut len);
        assert_eq!(attempt.unwrap(), Keyword::GreaterEqual);
        assert_eq!(len, 2);

        let attempt = keywords.try_parse(" > =", 1, &mut len);
        assert_eq!(attempt.unwrap(), Keyword::Greater);
        assert_eq!(len, 1);

        let attempt = keywords.try_parse(" > =", 3, &mut len);
        assert_eq!(attempt.unwrap(), Keyword::Equal);
        assert_eq!(len, 1);
    }
}
