// this is terrible
#[derive(Debug, Clone, PartialEq)]
pub struct NamedObjContainer<T> {
    objects: Vec<(String, T)>,
}

impl<T> NamedObjContainer<T> {
    pub fn new() -> Self {
        Self {
            objects: vec![],
        }
    }

    pub fn insert(&mut self, name: &str, obj: T) {
        self.objects.push((name.to_string(), obj));
    }

    pub fn get(&self, name: &str) -> Option<&T> {
        self.objects.iter()
            .find(|(obj_name, _)| obj_name == name)
            .map(|found| &found.1)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut T> {
        self.objects.iter_mut()
            .find(|(obj_name, _)| obj_name == name)
            .map(|found| &mut found.1)
    }

    pub fn iter(&self) -> std::slice::Iter<(String, T)> {
        self.objects.iter()
    }
}

impl<T> IntoIterator for NamedObjContainer<T> {
    type Item = (String, T);

    type IntoIter = std::vec::IntoIter<(String, T)>;

    fn into_iter(self) -> Self::IntoIter {
        self.objects.into_iter()
    }
}
