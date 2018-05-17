use nx;
use std::{collections::VecDeque, iter::Iterator};

/// Iterator that traverses all children of an `nx::Node` in a breadth-first
/// manner. Does not traverse the root node, only its children.
pub struct NxBreadthIter<'a> {
    queue: VecDeque<nx::node::Nodes<'a>>,
}

impl<'a> NxBreadthIter<'a> {
    pub fn new(root: &nx::Node<'a>) -> Self {
        let mut queue = VecDeque::with_capacity(12);
        queue.push_back(root.iter());

        Self { queue }
    }
}

impl<'a> Iterator for NxBreadthIter<'a> {
    type Item = nx::Node<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(f) = self.queue.front_mut() {
            if let Some(n) = f.next() {
                self.queue.push_back(n.iter());

                return Some(n);
            }

            self.queue.pop_front();
        }

        None
    }
}

/// Iterator that traverses all children of an `nx::Node` in a depth-first
/// manner. Does not traverse the root node, only its children.
pub struct NxDepthIter<'a> {
    stack: Vec<nx::node::Nodes<'a>>,
}

impl<'a> NxDepthIter<'a> {
    pub fn new(root: &nx::Node<'a>) -> Self {
        let mut stack = Vec::with_capacity(12);
        stack.push(root.iter());

        Self { stack }
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
