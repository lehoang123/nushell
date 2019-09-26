use crate::context::{SourceMap, SpanSource};
use crate::parser::parse::parser::TracableContext;
use crate::prelude::*;
use crate::Text;
use derive_new::new;
use getset::Getters;
use serde::Deserialize;
use serde::Serialize;
use std::path::{Path, PathBuf};
use uuid::Uuid;

#[derive(new, Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize, Hash)]
pub struct Tagged<T> {
    pub tag: Tag,
    pub item: T,
}

impl<T> HasTag for Tagged<T> {
    fn tag(&self) -> Tag {
        self.tag
    }
}

impl AsRef<Path> for Tagged<PathBuf> {
    fn as_ref(&self) -> &Path {
        self.item.as_ref()
    }
}

pub trait TaggedItem: Sized {
    fn tagged(self, tag: impl Into<Tag>) -> Tagged<Self> {
        Tagged::from_item(self, tag.into())
    }

    // For now, this is a temporary facility. In many cases, there are other useful spans that we
    // could be using, such as the original source spans of JSON or Toml files, but we don't yet
    // have the infrastructure to make that work.
    fn tagged_unknown(self) -> Tagged<Self> {
        Tagged::from_item(
            self,
            Tag {
                span: Span::unknown(),
                origin: uuid::Uuid::nil(),
            },
        )
    }
}

impl<T> TaggedItem for T {}

impl<T> std::ops::Deref for Tagged<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.item
    }
}

impl<T> Tagged<T> {
    pub fn with_tag(self, tag: impl Into<Tag>) -> Tagged<T> {
        Tagged::from_item(self.item, tag)
    }

    pub fn from_item(item: T, tag: impl Into<Tag>) -> Tagged<T> {
        Tagged {
            item,
            tag: tag.into(),
        }
    }

    pub fn map<U>(self, input: impl FnOnce(T) -> U) -> Tagged<U> {
        let tag = self.tag();

        let mapped = input(self.item);
        Tagged::from_item(mapped, tag)
    }

    pub(crate) fn copy_tag<U>(&self, output: U) -> Tagged<U> {
        Tagged::from_item(output, self.tag())
    }

    pub fn source(&self, source: &Text) -> Text {
        Text::from(self.tag().slice(source))
    }

    pub fn tag(&self) -> Tag {
        self.tag
    }

    pub fn origin(&self) -> uuid::Uuid {
        self.tag.origin
    }

    pub fn origin_name(&self, source_map: &SourceMap) -> Option<String> {
        match source_map.get(&self.tag.origin) {
            Some(SpanSource::File(file)) => Some(file.clone()),
            Some(SpanSource::Url(url)) => Some(url.clone()),
            _ => None,
        }
    }

    pub fn item(&self) -> &T {
        &self.item
    }

    pub fn into_parts(self) -> (T, Tag) {
        (self.item, self.tag)
    }
}

impl From<&Tag> for Tag {
    fn from(input: &Tag) -> Tag {
        *input
    }
}

impl From<nom_locate::LocatedSpanEx<&str, Uuid>> for Span {
    fn from(input: nom_locate::LocatedSpanEx<&str, Uuid>) -> Span {
        Span::new(input.offset, input.offset + input.fragment.len())
    }
}

impl<T>
    From<(
        nom_locate::LocatedSpanEx<T, Uuid>,
        nom_locate::LocatedSpanEx<T, Uuid>,
    )> for Span
{
    fn from(
        input: (
            nom_locate::LocatedSpanEx<T, Uuid>,
            nom_locate::LocatedSpanEx<T, Uuid>,
        ),
    ) -> Span {
        Span {
            start: input.0.offset,
            end: input.1.offset,
        }
    }
}

impl From<(usize, usize)> for Span {
    fn from(input: (usize, usize)) -> Span {
        Span::new(input.0, input.1)
    }
}

impl From<&std::ops::Range<usize>> for Span {
    fn from(input: &std::ops::Range<usize>) -> Span {
        Span {
            start: input.start,
            end: input.end,
        }
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Serialize, Deserialize, Hash, Getters, new,
)]
pub struct Tag {
    pub origin: Uuid,
    pub span: Span,
}

impl From<Span> for Tag {
    fn from(span: Span) -> Self {
        Tag {
            origin: uuid::Uuid::nil(),
            span,
        }
    }
}

impl From<&Span> for Tag {
    fn from(span: &Span) -> Self {
        Tag {
            origin: uuid::Uuid::nil(),
            span: *span,
        }
    }
}

impl From<(usize, usize, TracableContext)> for Tag {
    fn from((start, end, context): (usize, usize, TracableContext)) -> Self {
        Tag {
            origin: context.origin,
            span: Span::new(start, end),
        }
    }
}

impl From<(usize, usize, Uuid)> for Tag {
    fn from((start, end, origin): (usize, usize, Uuid)) -> Self {
        Tag {
            origin,
            span: Span::new(start, end),
        }
    }
}

impl From<(usize, usize, Option<Uuid>)> for Tag {
    fn from((start, end, origin): (usize, usize, Option<Uuid>)) -> Self {
        Tag {
            origin: origin.unwrap_or(uuid::Uuid::nil()),
            span: Span::new(start, end),
        }
    }
}

impl From<nom_locate::LocatedSpanEx<&str, TracableContext>> for Tag {
    fn from(input: nom_locate::LocatedSpanEx<&str, TracableContext>) -> Tag {
        Tag {
            origin: input.extra.origin,
            span: Span::new(input.offset, input.offset + input.fragment.len()),
        }
    }
}

impl From<Tag> for Span {
    fn from(tag: Tag) -> Self {
        tag.span
    }
}

impl From<&Tag> for Span {
    fn from(tag: &Tag) -> Self {
        tag.span
    }
}

impl Tag {
    pub fn unknown_origin(span: Span) -> Tag {
        Tag {
            origin: uuid::Uuid::nil(),
            span,
        }
    }

    pub fn unknown_span(origin: Uuid) -> Tag {
        Tag {
            origin,
            span: Span::unknown(),
        }
    }

    pub fn unknown() -> Tag {
        Tag {
            origin: uuid::Uuid::nil(),
            span: Span::unknown(),
        }
    }

    pub fn until(&self, other: impl Into<Tag>) -> Tag {
        let other = other.into();
        debug_assert!(
            self.origin == other.origin,
            "Can only merge two tags with the same origin"
        );

        Tag {
            span: Span::new(self.span.start, other.span.end),
            origin: self.origin,
        }
    }

    pub fn slice<'a>(&self, source: &'a str) -> &'a str {
        self.span.slice(source)
    }

    pub fn string<'a>(&self, source: &'a str) -> String {
        self.span.slice(source).to_string()
    }

    pub fn tagged_slice<'a>(&self, source: &'a str) -> Tagged<&'a str> {
        self.span.slice(source).tagged(self)
    }

    pub fn tagged_string<'a>(&self, source: &'a str) -> Tagged<String> {
        self.span.slice(source).to_string().tagged(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Serialize, Deserialize, Hash)]
pub struct Span {
    start: usize,
    end: usize,
}

impl From<Option<Span>> for Span {
    fn from(input: Option<Span>) -> Span {
        match input {
            None => Span::new(0, 0),
            Some(span) => span,
        }
    }
}

impl Span {
    pub fn unknown() -> Span {
        Span::new(0, 0)
    }

    pub fn new(start: usize, end: usize) -> Span {
        assert!(
            end >= start,
            "Can't create a Span whose end < start, start={}, end={}",
            start,
            end
        );

        Span { start, end }
    }

    /*
    pub fn unknown_with_uuid(uuid: Uuid) -> Span {
        Span {
            start: 0,
            end: 0,
            source: Some(uuid),
        }
    }
    */

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn is_unknown(&self) -> bool {
        self.start == 0 && self.end == 0
    }

    pub fn slice<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start..self.end]
    }
}

impl language_reporting::ReportingSpan for Span {
    fn with_start(&self, start: usize) -> Self {
        Span::new(start, self.end)
    }

    fn with_end(&self, end: usize) -> Self {
        Span::new(self.start, end)
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

impl language_reporting::ReportingSpan for Tag {
    fn with_start(&self, start: usize) -> Self {
        Tag {
            span: Span::new(start, self.span.end),
            origin: self.origin,
        }
    }

    fn with_end(&self, end: usize) -> Self {
        Tag {
            span: Span::new(self.span.start, end),
            origin: self.origin,
        }
    }

    fn start(&self) -> usize {
        self.span.start
    }

    fn end(&self) -> usize {
        self.span.end
    }
}
