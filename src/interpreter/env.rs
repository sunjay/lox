use std::sync::Arc;
use std::collections::HashMap;

use super::Value;

#[derive(Debug, Clone)]
pub struct Scope {
    /// The parent of this scope, represented as an index into `scope_stack`
    parent: Option<usize>,
    values: HashMap<Arc<str>, Value>,
}

impl Scope {
    pub fn global() -> Self {
        Self {
            parent: None,
            values: HashMap::default(),
        }
    }

    pub fn new(parent: usize) -> Self {
        Self {
            parent: Some(parent),
            values: HashMap::default(),
        }
    }

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
            scope_stack: vec![Scope::global()],
        }
    }
}

impl Environment {
    pub fn insert(&mut self, name: Arc<str>, value: Value) {
        self.top_scope_mut().insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        let mut current_scope = Some(self.scope_stack.len() - 1);

        while let Some(scope_id) = current_scope {
            let scope = &self.scope_stack[scope_id];
            match scope.get(name) {
                Some(value) => return Some(value),
                None => current_scope = scope.parent,
            }
        }

        None
    }

    pub fn get_mut<'a>(&'a mut self, name: &str) -> Option<&'a mut Value> {
        let mut current_scope = Some(self.scope_stack.len() - 1);

        while let Some(scope_id) = current_scope {
            let scope = &self.scope_stack[scope_id];
            match scope.get(name) {
                Some(_) => break,
                None => current_scope = scope.parent,
            }
        }

        current_scope.and_then(move |scope_id| self.scope_stack[scope_id].get_mut(name))
    }

    pub fn push_scope_global(&mut self) {
        // The global scope is always at index zero
        let parent = 0;
        self.scope_stack.push(Scope::new(parent));
    }

    pub fn push_scope(&mut self) {
        let parent = self.scope_stack.len() - 1;
        self.scope_stack.push(Scope::new(parent));
    }

    pub fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }

    pub fn top_scope_mut(&mut self) -> &mut Scope {
        self.scope_stack.last_mut().expect("bug: popped global scope")
    }
}
