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
}

const AND_VARIANTS: &[&str] = &["and", "&&", "/\\"];
const OR_VARIANTS: &[&str] = &["or", "||", "\\/"];

const FALSE_VARIANTS: &[&str] = &["false", "False", "FALSE", "incorrect", "nah", "👎"];
const TRUE_VARIANTS: &[&str] = &["true", "True", "TRUE", "correct", "yah", "👍"];

const FOR_VARIANTS: &[&str] = &["for", "each"];
const IF_VARIANTS: &[&str] = &["if", "case", "check", "cond"];
const ELSE_VARIANTS: &[&str] = &["else", "then", "otherwise"];
const EMPTY_VARIANTS: &[&str] = &["nil", "None", "null", "NULL", "undefined", "void"];

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

const WHILE_VARIANTS: &[&str] = &["while", "during", "whilst", "until", "as_long_as", "🔁"];

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

    use super::KeywordRandomizer;

    #[test]
    fn try_from_string() {
        let mut rng = ChaCha8Rng::seed_from_u64(42);
        let keywords = KeywordRandomizer::seeded_start(&mut rng);

        let attempt = keywords.try_from_str("var");

        assert_eq!(attempt.unwrap_err(), Some("$"))
    }
}
