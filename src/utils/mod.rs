
pub(crate) struct Lookahead<I: Iterator, const SIZE: usize> {
    iter: I,
    next_items: [Option<I::Item>; SIZE],
}

impl<I: Iterator, const SIZE: usize> Lookahead<I, SIZE> {
    pub fn new(mut iter: I) -> Self {
        Self { next_items: core::array::from_fn(|_| iter.next()), iter }
    }
    pub fn peek(&self) -> Option<&I::Item> {
        self.next_items[0].as_ref()
    }
    pub fn peek_n(&self, n: usize) -> Option<&I::Item> {
        self.next_items[n].as_ref()
    }
}

impl<I: Iterator, const SIZE: usize> Iterator for Lookahead<I, SIZE> {
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_items.rotate_left(1);
        std::mem::replace(&mut self.next_items[SIZE - 1], self.iter.next())
    }
}
