use std::{ops::{Deref, DerefMut}, fmt::Debug};

#[derive(Debug)]
pub struct Spanned<T> {
    pub obj: T,
    pub start: usize,
    pub len: usize,
    pub poisoned: bool,
    pub file_id: u8,
}

impl<T> Spanned<T> {
    pub fn new(obj: T, start: usize, len: usize) -> Self {
        Self::new_with_file_id(obj, start, len, 0)
    }

    pub fn new_with_file_id(obj: T, start: usize, len: usize, file_id: u8) -> Self {
        Self {
            obj, start, len, 
            poisoned: false,
            file_id,
        }
    }

    pub fn zeroed(obj: T) -> Self {
        Self::new(obj, 0, 0)
    }

    pub fn new_poisoned(obj: T, start: usize, len: usize) -> Self {
        let mut this = Self::new(obj, start, len);
        this.poison();
        this
    }

    pub fn get_start_and_len<E>(
        o1: &Spanned<T>, o2: &Spanned<E>
    ) -> (usize, usize)
    {
        if o1.poisoned || o2.poisoned {
            (0, 0)
        } else {
            (o1.start, (o2.start - o1.start) + o2.len)
        }
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
        Spanned::new_with_file_id(obj, span.start, span.len, span.file_id)
    }

    pub fn just_span_data(&self) -> Spanned<()> {
        Spanned::from_other_span((), self)
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

impl<T> PartialEq for Spanned<T> 
where 
    T: PartialEq
{
    fn eq(&self, other: &Self) -> bool {
        self.obj == other.obj
    }
}
