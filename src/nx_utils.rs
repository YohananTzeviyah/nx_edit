use nx;
use std::{collections::VecDeque, iter::Iterator};

/// Iterator that traverses all children of an `nx::Node` in a breadth-first
/// manner. Does not traverse the root node, only its children.
pub struct NxBreadthIter<'a> {
    queue: VecDeque<nx::node::Nodes<'a>>,
}

impl<'a> NxBreadthIter<'a> {
    #[inline]
    pub fn new(root: &nx::Node<'a>) -> Self {
        let mut queue = VecDeque::with_capacity(12);
        queue.push_back(root.iter());

        Self { queue }
    }
}

impl<'a> Iterator for NxBreadthIter<'a> {
    type Item = nx::Node<'a>;

    #[inline]
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

/// Iterator that traverses all children of an `nx::Node` in a breadth-first
/// manner and also supplies the paths of each of the nodes. Does not traverse
/// the root node, only its children.
pub struct NxBreadthPathIter<'a> {
    queue:    VecDeque<(nx::node::Nodes<'a>, Vec<i32>)>,
    curr_lvl: i32,
}

impl<'a> NxBreadthPathIter<'a> {
    #[inline]
    pub fn new(root: &nx::Node<'a>) -> Self {
        let mut queue = VecDeque::with_capacity(12);
        let mut p = Vec::with_capacity(12);
        p.push(0);
        queue.push_back((root.iter(), p));

        Self { queue, curr_lvl: 0 }
    }
}

impl<'a> Iterator for NxBreadthPathIter<'a> {
    type Item = (nx::Node<'a>, Vec<i32>);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((iter, path)) = self.queue.front_mut() {
            if let Some(n) = iter.next() {
                let mut n_path = path.clone();
                n_path.push(self.curr_lvl);
                self.curr_lvl += 1;

                if n.child_count() > 0 {
                    self.queue.push_back((n.iter(), n_path.clone()));
                }

                return Some((n, n_path));
            }

            self.queue.pop_front();
            self.curr_lvl = 0;
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
    #[inline]
    pub fn new(root: &nx::Node<'a>) -> Self {
        let mut stack = Vec::with_capacity(12);
        stack.push(root.iter());

        Self { stack }
    }
}

impl<'a> Iterator for NxDepthIter<'a> {
    type Item = nx::Node<'a>;

    #[inline]
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
