
use std::path::Path;
use crate::{parse::token::TokenKind, src::Codebase};

/// Represents a programming language
pub trait Language: Sized + 'static {
    /// The type of tokens this language's source files consist of
    type TokenKind<'s>: TokenKind<'s, Self>;

    /// List of possible file extensions source files of this language may have
    fn file_extensions() -> &'static [&'static str];

    /// Create a new Codebase for this specific language
    fn new_codebase(path: &Path) -> Result<Codebase<Self>, String> {
        Codebase::new_from_path(path)
    }
}
