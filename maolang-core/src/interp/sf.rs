//! Stack Frame Implementation

use std::collections::HashMap;

use crate::parser::ast::Literal;

use super::RuntimeError;

/// A runtime's stack
#[derive(Debug, Clone)]
pub struct Stack<'a> {
    /// The stackframes
    stack: Vec<StackFrame<'a>>,
}

impl Default for Stack<'_> {
    fn default() -> Self {
        Self {
            stack: vec![StackFrame::default()],
        }
    }
}

impl<'a> Stack<'a> {
    /// Pushes a new stack frame
    pub fn push(&mut self) {
        self.stack.push(StackFrame::default());
    }

    /// Pops the current stackframe
    pub fn pop(&mut self) {
        self.stack.pop();
    }

    /// Attempts to get a variable from the current context
    pub fn get(&mut self, name: &str) -> Result<&mut Literal<'a>, RuntimeError> {
        let len = self.stack.len() - 1;
        if let Some(val) = self.stack[len].variables.get_mut(name) {
            Ok(val)
        } else {
            Err(RuntimeError(format!(
                "Variable with name {name} does not exist in this scope"
            )))
        }
    }

    /// Attempts to set a variable in the current context
    pub fn set(&mut self, name: String, lit: Literal<'a>) {
        let len = self.stack.len() - 1;
        self.stack[len].variables.insert(name, lit);
    }
}

/// Context for a single stack frame
#[derive(Debug, Default, Clone)]
pub struct StackFrame<'a> {
    /// All local variables to the stack frame
    pub variables: HashMap<String, Literal<'a>>,
}
