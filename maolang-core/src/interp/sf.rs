//! Stack Frame Implementation

use std::collections::HashMap;

use crate::parser::ast::Literal;

/// A runtime's stack
#[derive(Debug, Clone)]
pub struct Stack<'a>(Vec<StackFrame<'a>>);

impl<'a> Default for Stack<'a> {
    fn default() -> Self {
        Self(vec![StackFrame::default()])
    }
}

impl<'a> Stack<'a> {
    /// Pushes a new stack frame
    pub fn push(&mut self) {
        self.0.push(StackFrame::default());
    }

    /// Pops the current stackframe
    pub fn pop(&mut self) {
        self.0.pop();
    }

    /// Attempts to get a variable from the current context
    pub fn get(&mut self, name: &str) -> Option<&mut Literal<'a>> {
        let len = self.0.len() - 1;
        self.0[len].variables.get_mut(name)
    }

    /// Attempts to set a variable in the current context
    pub fn set(&mut self, name: String, lit: Literal<'a>) {
        let len = self.0.len() - 1;
        self.0[len].variables.insert(name, lit);
    }
}

/// Context for a single stack frame
#[derive(Debug, Default, Clone)]
pub struct StackFrame<'a> {
    /// All local variables to the stack frame
    pub variables: HashMap<String, Literal<'a>>,
}
