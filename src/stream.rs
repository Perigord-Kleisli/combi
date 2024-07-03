/// Can be seen as sort of a copyable iterator, analogous to
/// uncons operations in other functional languages.
///
/// It's implemented here to more easily provide input for branching
/// operations than with iterators
///
/// # Examples
/// ```
/// if let Some(x,xs) = "fo".uncons() {
///     assert_eq!(x, 'f')
///     assert_eq!(xs, "o")
///     if let Some(x,xs) = xs.uncons() {
///         assert_eq!(x, 'o')
///         assert_eq!(None, xs.uncons())
///     }
/// }
/// ```

pub struct StreamIter<T: Stream>(T);

pub trait Stream: Copy {
    type Item;

    fn uncons(self) -> Option<(Self::Item, Self)>;

    /// # Examples
    /// ```
    /// if let Some(l,r) = "abcd".split(2) {
    ///     assert_eq!(l,"ab");
    ///     assert_eq!(r,"cd");
    /// }
    /// ```
    fn split(self, n: usize) -> Option<(Self, Self)>;

    fn iter(self) -> StreamIter<Self> {
        StreamIter(self)
    }
}

impl Stream for &str {
    type Item = char;

    fn uncons(self) -> Option<(Self::Item, Self)> {
        let c = self.chars().next()?;
        Some((c, &self[c.len_utf8()..]))
    }

    fn split(self, n: usize) -> Option<(Self, Self)> {
        Some((self.get(0..n)?, self.get(n..)?))
    }
}

impl<T: Copy> Stream for &[T] {
    type Item = T;

    fn uncons(self) -> Option<(Self::Item, Self)> {
        let x = *self.first()?;
        Some((x, self.get(1..)?))
    }

    fn split(self, n: usize) -> Option<(Self, Self)> {
        Some((self.get(0..n)?, self.get(n..)?))
    }

}

impl<Item, S: Stream<Item = Item>> Iterator for StreamIter<S> {
    type Item = Item;

    fn next(&mut self) -> Option<Self::Item> {
        let (x,xs) = self.0.uncons()?;
        *self = StreamIter(xs);
        Some(x)
    }
}
