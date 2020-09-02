//! Error message and diagnostics utilities

use thiserror::Error;

#[derive(Debug, Error, Clone)]
#[error("[line {line}] {message}")]
pub struct Diagnostic {
    pub line: usize,
    pub message: String,
}
