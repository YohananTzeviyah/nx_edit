use nx;
use std::iter::Iterator;

/// Iterator that traverses all children of an `nx::Node` in a depth-first
/// manner. Does not traverse the root node, only its children.
pub struct NxDepthIter<'a> {
    stack: Vec<nx::node::Nodes<'a>>,
}

impl<'a> NxDepthIter<'a> {
    pub fn new(root: &nx::Node<'a>) -> Self {
        Self {
            stack: vec![root.iter()],
        }
    }
}

impl<'a> Iterator for NxDepthIter<'a> {
    type Item = nx::Node<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(d) = self.stack.last_mut() {
            if let Some(n) = d.next() {
                self.stack.push(n.iter());

                return Some(n);
            }

            self.stack.pop();
        }

        None
    }
}
