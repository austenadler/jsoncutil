const ADDITIONAL_INDENT: usize = 4;

#[derive(Debug)]
pub struct Indentor {
    indent_str: Vec<u8>,
    buf: Vec<u8>,
}

impl Indentor {
    /// Constructs an indentor
    ///
    /// The length of `indent_str` must be `> 0`
    pub fn new(indent_str: impl AsRef<[u8]>) -> Self {
        assert!(!indent_str.as_ref().is_empty());

        Self {
            indent_str: indent_str.as_ref().to_vec(),
            buf: indent_str.as_ref().repeat(ADDITIONAL_INDENT),
        }
    }

    /// Return a `&[u8]` of the `indent_str` repeated `level` times
    ///
    /// Will update internal buffer if required
    pub fn get_indent(&mut self, level: usize) -> &[u8] {
        let levels_cached = self.levels_cached();

        if level > levels_cached {
            self.buf.append(
                &mut self
                    .indent_str
                    .repeat(level - levels_cached + ADDITIONAL_INDENT),
            );
        }

        &self.buf[0..(level * self.indent_str.len())]
    }

    /// Gets the number of indent levels cached
    fn levels_cached(&self) -> usize {
        self.buf.len() / self.indent_str.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_indentor() {
        let mut i = Indentor::new("\t");

        assert_eq!(i.get_indent(1), b"\t");

        assert_eq!(i.buf.len(), ADDITIONAL_INDENT);

        assert_eq!(i.get_indent(100), b"\t".repeat(100));

        assert!(i.buf.len() >= 100);
        assert!(i.buf.len() <= 100 + ADDITIONAL_INDENT);
    }
}
