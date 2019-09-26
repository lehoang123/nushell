pub(crate) mod debug;

use crate::errors::ShellError;
use crate::parser::TokenNode;
use crate::traits::ToDebug;
use crate::{Tag, Tagged, TaggedItem};
use derive_new::new;

#[derive(Debug, new)]
pub struct TokensIterator<'a> {
    tokens: &'a [TokenNode],
    tag: Tag,
    skip_ws: bool,
    #[new(default)]
    index: usize,
    #[new(default)]
    seen: indexmap::IndexSet<usize>,
}

#[derive(Debug)]
pub struct Checkpoint<'content, 'me> {
    pub(crate) iterator: &'me mut TokensIterator<'content>,
    index: usize,
    seen: indexmap::IndexSet<usize>,
    committed: bool,
}

impl<'content, 'me> Checkpoint<'content, 'me> {
    pub(crate) fn commit(mut self) {
        self.committed = true;
    }
}

impl<'content, 'me> std::ops::Drop for Checkpoint<'content, 'me> {
    fn drop(&mut self) {
        if !self.committed {
            self.iterator.index = self.index;
            self.iterator.seen = self.seen.clone();
        }
    }
}

#[derive(Debug)]
pub struct Peeked<'content, 'me> {
    pub(crate) node: Option<&'content TokenNode>,
    iterator: &'me mut TokensIterator<'content>,
    from: usize,
    to: usize,
}

impl<'content, 'me> Peeked<'content, 'me> {
    pub fn commit(&mut self) -> Option<&'content TokenNode> {
        let Peeked {
            node,
            iterator,
            from,
            to,
        } = self;

        let node = (*node)?;
        iterator.commit(*from, *to);
        Some(node)
    }

    pub fn origin(&self) -> uuid::Uuid {
        self.iterator.tag.origin
    }

    pub fn not_eof(
        self,
        expected: impl Into<String>,
    ) -> Result<PeekedNode<'content, 'me>, ShellError> {
        match self.node {
            None => Err(ShellError::unexpected_eof(
                expected,
                self.iterator.eof_tag(),
            )),
            Some(node) => Ok(PeekedNode {
                node,
                iterator: self.iterator,
                from: self.from,
                to: self.to,
            }),
        }
    }

    pub fn type_error(&self, expected: impl Into<String>) -> ShellError {
        peek_error(&self.node, self.iterator.eof_tag(), expected)
    }
}

#[derive(Debug)]
pub struct PeekedNode<'content, 'me> {
    pub(crate) node: &'content TokenNode,
    iterator: &'me mut TokensIterator<'content>,
    from: usize,
    to: usize,
}

impl<'content, 'me> PeekedNode<'content, 'me> {
    pub fn commit(self) -> &'content TokenNode {
        let PeekedNode {
            node,
            iterator,
            from,
            to,
        } = self;

        iterator.commit(from, to);
        node
    }

    pub fn rollback(self) {}
}

pub fn peek_error(
    node: &Option<&TokenNode>,
    eof_tag: Tag,
    expected: impl Into<String>,
) -> ShellError {
    match node {
        None => ShellError::unexpected_eof(expected, eof_tag),
        Some(node) => ShellError::type_error(expected, node.tagged_type_name()),
    }
}

impl<'content> TokensIterator<'content> {
    #[cfg(test)]
    pub fn all(tokens: &'content [TokenNode], tag: Tag) -> TokensIterator<'content> {
        TokensIterator::new(tokens, tag, false)
    }

    /// Use a checkpoint when you need to peek more than one token ahead, but can't be sure
    /// that you'll succeed.
    pub fn checkpoint<'me>(&'me mut self) -> Checkpoint<'content, 'me> {
        let index = self.index;
        let seen = self.seen.clone();

        Checkpoint {
            iterator: self,
            index,
            seen,
            committed: false,
        }
    }

    pub fn debug(&self, source: &str) -> String {
        let mut out = vec![];

        for token in self.tokens {
            out.push(format!("[[ {} ]]", token.debug(source)));
        }

        out.join(" ")
    }

    pub fn origin(&self) -> uuid::Uuid {
        self.tag.origin
    }

    fn eof_tag(&self) -> Tag {
        Tag::from((self.tag.span.end(), self.tag.span.end(), self.tag.origin))
    }

    pub fn cursor_tag(&mut self) -> Tag {
        let next = self.peek_any();

        match next.node {
            None => self.eof_tag(),
            Some(node) => node.tag(),
        }
    }

    pub fn typed_tag_at_cursor(&mut self) -> Tagged<&'static str> {
        let next = self.peek_any();

        match next.node {
            None => "end".tagged(self.eof_tag()),
            Some(node) => node.tagged_type_name(),
        }
    }

    pub fn remove(&mut self, position: usize) {
        self.seen.insert(position);
    }

    pub fn at_end(&self) -> bool {
        peek(self, self.skip_ws).is_none()
    }

    pub fn at_end_possible_ws(&self) -> bool {
        peek(self, true).is_none()
    }

    pub fn advance(&mut self) {
        self.seen.insert(self.index);
        self.index += 1;
    }

    pub fn extract<T>(&mut self, f: impl Fn(&TokenNode) -> Option<T>) -> Option<(usize, T)> {
        for (i, item) in self.tokens.iter().enumerate() {
            if self.seen.contains(&i) {
                continue;
            }

            match f(item) {
                None => {
                    continue;
                }
                Some(value) => {
                    self.seen.insert(i);
                    return Some((i, value));
                }
            }
        }

        None
    }

    pub fn move_to(&mut self, pos: usize) {
        self.index = pos;
    }

    pub fn restart(&mut self) {
        self.index = 0;
    }

    pub fn clone(&self) -> TokensIterator<'content> {
        TokensIterator {
            tokens: self.tokens,
            tag: self.tag,
            index: self.index,
            seen: self.seen.clone(),
            skip_ws: self.skip_ws,
        }
    }

    // Get the next token, not including whitespace
    pub fn next_non_ws(&mut self) -> Option<&TokenNode> {
        let mut peeked = start_next(self, true);
        peeked.commit()
    }

    // Peek the next token, not including whitespace
    pub fn peek_non_ws<'me>(&'me mut self) -> Peeked<'content, 'me> {
        start_next(self, true)
    }

    // Peek the next token, including whitespace
    pub fn peek_any<'me>(&'me mut self) -> Peeked<'content, 'me> {
        start_next(self, false)
    }

    fn commit(&mut self, from: usize, to: usize) {
        for index in from..to {
            self.seen.insert(index);
        }

        self.index = to;
    }

    pub fn debug_remaining(&self) -> Vec<TokenNode> {
        let mut tokens = self.clone();
        tokens.restart();
        tokens.cloned().collect()
    }
}

impl<'a> Iterator for TokensIterator<'a> {
    type Item = &'a TokenNode;

    fn next(&mut self) -> Option<&'a TokenNode> {
        next(self, self.skip_ws)
    }
}

fn peek<'content, 'me>(
    iterator: &TokensIterator<'content>,
    skip_ws: bool,
) -> Option<&'content TokenNode> {
    let mut to = iterator.index;

    loop {
        if to >= iterator.tokens.len() {
            return None;
        }

        if iterator.seen.contains(&to) {
            to += 1;
            continue;
        }

        if to >= iterator.tokens.len() {
            return None;
        }

        let node = &iterator.tokens[to];

        match node {
            TokenNode::Whitespace(_) if skip_ws => {
                to += 1;
            }
            _ => {
                return Some(node);
            }
        }
    }
}

fn start_next<'content, 'me>(
    iterator: &'me mut TokensIterator<'content>,
    skip_ws: bool,
) -> Peeked<'content, 'me> {
    let from = iterator.index;
    let mut to = iterator.index;

    loop {
        if to >= iterator.tokens.len() {
            return Peeked {
                node: None,
                iterator,
                from,
                to,
            };
        }

        if iterator.seen.contains(&to) {
            to += 1;
            continue;
        }

        if to >= iterator.tokens.len() {
            return Peeked {
                node: None,
                iterator,
                from,
                to,
            };
        }

        let node = &iterator.tokens[to];

        match node {
            TokenNode::Whitespace(_) if skip_ws => {
                to += 1;
            }
            _ => {
                to += 1;
                return Peeked {
                    node: Some(node),
                    iterator,
                    from,
                    to,
                };
            }
        }
    }
}

fn next<'a>(iterator: &mut TokensIterator<'a>, skip_ws: bool) -> Option<&'a TokenNode> {
    loop {
        if iterator.index >= iterator.tokens.len() {
            return None;
        }

        if iterator.seen.contains(&iterator.index) {
            iterator.advance();
            continue;
        }

        if iterator.index >= iterator.tokens.len() {
            return None;
        }

        match &iterator.tokens[iterator.index] {
            TokenNode::Whitespace(_) if skip_ws => {
                iterator.advance();
            }
            other => {
                iterator.advance();
                return Some(other);
            }
        }
    }
}