use std::{ops::{Deref, DerefMut}, fmt::Debug};

pub struct Spanned<T> {
    pub obj: T,
    pub start: usize,
    pub len: usize,
}

impl<T> Spanned<T> {
    pub fn new(obj: T, start: usize, len: usize) -> Self {
        Self {
            obj, start, len
        }
    }
}

impl<T> Spanned<T>
where 
    T: Clone
{
    pub fn cloned(&self) -> T {
        self.obj.clone()
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.obj
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.obj
    }
}

impl<T> Debug for Spanned<T> 
where
    T: Debug
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.obj)
    }
}
