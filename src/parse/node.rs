
use std::fmt::Debug;

use crate::{lang::Language, src::Span};

pub trait NodeKind<L: Language>: Debug {
    /// Get the span (raw source code string) of this node
    fn span<'s>(&self) -> Span<'s, L>;
}

pub(crate) struct NodePool {
    
}
