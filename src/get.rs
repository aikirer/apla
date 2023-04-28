// The '.' operator, "foo.bar", "foo.bar()"

trait Get {
    fn resolve_get(&self);
    fn compile(&self);
    fn runtime_get(&self);
}