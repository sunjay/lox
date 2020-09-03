use std::sync::Arc;
use std::collections::HashMap;

use super::Value;

#[derive(Debug, Default, Clone)]
pub struct Environment {
    values: HashMap<Arc<str>, Value>,
}

impl Environment {
    pub fn insert(&mut self, name: Arc<str>, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.values.get(name)
    }
}
