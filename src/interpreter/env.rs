use std::sync::Arc;
use std::collections::HashMap;

use super::Value;

#[derive(Debug, Default, Clone)]
pub struct Scope {
    values: HashMap<Arc<str>, Value>,
}

impl Scope {
    pub fn insert(&mut self, name: Arc<str>, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.values.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        self.values.get_mut(name)
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    scope_stack: Vec<Scope>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            scope_stack: vec![Scope::default()],
        }
    }
}

impl Environment {
    pub fn insert(&mut self, name: Arc<str>, value: Value) {
        self.top_scope_mut().insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.scope_stack.iter().rev().find_map(|scope| scope.get(name))
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        self.scope_stack.iter_mut().rev().find_map(|scope| scope.get_mut(name))
    }

    pub fn push_scope(&mut self) {
        self.scope_stack.push(Scope::default());
    }

    pub fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }

    pub fn top_scope_mut(&mut self) -> &mut Scope {
        self.scope_stack.last_mut().expect("bug: popped global scope")
    }
}
