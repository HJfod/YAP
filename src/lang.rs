pub trait Language {
    fn file_extensions() -> &'static [&'static str];
}
