//! Error message and diagnostics utilities

use thiserror::Error;

#[derive(Debug, Error, Clone)]
#[error("[line {line}] Error{location}: {message}")]
pub struct Diagnostic {
    pub line: usize,
    pub location: String,
    pub message: String,
}
