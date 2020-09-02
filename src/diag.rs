//! Error message and diagnostics utilities

use thiserror::Error;

#[derive(Debug, Error, Clone)]
#[error("[line {line}] {location}{message}")]
pub struct Diagnostic {
    pub line: usize,
    pub location: String,
    pub message: String,
}

impl Diagnostic {
    pub fn new(
        line: usize,
        location: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self {
            line,
            location: location.into(),
            message: message.into(),
        }
    }

    pub fn line(line: usize, message: impl Into<String>) -> Self {
        Self::new(line, "", message)
    }
}
