use std::{ops::{Deref, DerefMut}, fmt::Debug};

pub struct Spanned<T> {
    pub obj: T,
    pub start: usize,
    pub len: usize,
    pub poisoned: bool,
}

impl<T> Spanned<T> {
    pub fn new(obj: T, start: usize, len: usize) -> Self {
        Self {
            obj, start, len, 
            poisoned: false
        }
    }

    pub fn get_start_and_len(
        o1: &Spanned<T>, o2: &Spanned<T>
    ) -> (usize, usize)
    {
        (o1.start, (o2.start - o1.start) + o2.len)
    }

    pub fn obj_ref(&self) -> &T {
        &self.obj
    }

    pub fn obj_mut(&mut self) -> &mut T {
        &mut self.obj
    }

    pub fn poison(&mut self) {
        self.poisoned = true;
    }

    pub fn from_other_span<E>(obj: T, span: &Spanned<E>) -> Self {
        Spanned::new(obj, span.start, span.len)
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

impl<T> Clone for Spanned<T>
where
    T: Clone
{
    fn clone(&self) -> Self {
        Self::new(self.obj.clone(), self.start, self.len)
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

impl<T> PartialEq for Spanned<T> 
where 
    T: PartialEq
{
    fn eq(&self, other: &Self) -> bool {
        self.obj == other.obj
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}
