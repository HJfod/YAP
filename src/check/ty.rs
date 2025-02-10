
use std::{fmt::Debug, marker::PhantomData};
use crate::parse::{node::NodeKind, token::Error};

pub type ResolveResult<T> = Result<T, Vec<Error>>;

pub trait TypeKind: Debug {
    fn typename(&self) -> String;
    fn convertible_to(&self, other: &Self) -> bool;
}

pub trait Resolve<T: TypeKind>: NodeKind {
    fn resolve(&mut self, resolver: &mut Resolver<T>) -> ResolveResult<T>;
}

pub struct Resolver<T: TypeKind> {
    some_were_resolved: bool,
    _phantom: PhantomData<T>,
}

impl<T: TypeKind> Default for Resolver<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: TypeKind> Resolver<T> {
    pub fn new() -> Self {
        Self {
            some_were_resolved: false,
            _phantom: PhantomData,
        }
    }
    pub fn resolve<N: Resolve<T>>(&mut self, node: &mut N) -> ResolveResult<T> {
        loop {
            self.some_were_resolved = false;
            match node.resolve(self) {
                Ok(ty) => return Ok(ty),
                Err(errors) => {
                    if !self.some_were_resolved {
                        return Err(errors);
                    }
                }
            }
        }
    }
}
