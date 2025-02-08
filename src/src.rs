use colored::{Color, Colorize};
use line_col::LineColLookup;
use std::{
    cmp::max,
    ffi::OsStr,
    fmt::{Debug, Display},
    fs,
    hash::Hash,
    ops::Range,
    path::{Path, PathBuf},
};
use crate::parse::{node::{Node, Parse}, token::{Token, TokenIterator, TokenKind, Tokenizer}};

pub enum Underline {
    /// Error squiggle
    Squiggle,
    /// Highlight
    Highlight,
    /// Gray underline
    Normal,
}

impl Underline {
    fn line(&self, range: Range<usize>) -> String {
        let (symbol, color) = match self {
            Self::Squiggle => ("~", Color::Red),
            Self::Highlight => ("^", Color::Cyan),
            Self::Normal => ("-", Color::Black),
        };
        format!(
            "{}{}",
            " ".repeat(range.start),
            symbol.repeat(max(1, range.end - range.start)).color(color)
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SrcID(usize);

impl SrcID {
    pub fn span(&self, range: Range<usize>) -> Span {
        Span(*self, range)
    }
}

#[derive(Debug, Clone)]
/// Represents a specific span of source code in a `Src`
pub struct Span(SrcID, Range<usize>);

impl Span {
    pub fn src(&self) -> SrcID {
        self.0
    }
    pub fn start(&self) -> usize {
        self.1.start
    }
    pub fn end(&self) -> usize {
        self.1.end
    }

    pub fn fetch_data<'s>(&self, codebase: &'s Codebase) -> &'s str {
        &codebase.get_src(self.0).data()[self.1.clone()]
    }
    pub fn name(&self, codebase: &Codebase) -> String {
        let src = codebase.get_src(self.0);
        let lookup = LineColLookup::new(src.data());
        let start = lookup.get(self.1.start);
        if self.1.is_empty() {
            format!("{}:{}:{}", src.name(), start.0, start.1)
        }
        else {
            let end = lookup.get(self.1.end);
            format!(
                "{}:{}:{}-{}:{}",
                src.name(),
                start.0,
                start.1,
                end.0,
                end.1
            )
        }
    }
    pub fn underlined(&self, codebase: &Codebase, style: Underline) -> String {
        let data = self.fetch_data(codebase);
        // Get the starting and ending linecols as 0-based indices
        let sub_tuple = |a: (usize, usize)| (a.0 - 1, a.1 - 1);
        let lookup = LineColLookup::new(data);
        let start = sub_tuple(lookup.get(self.1.start));
        let end = sub_tuple(lookup.get(self.1.end));

        let mut lines = data
            .lines()
            .skip(start.0)
            .take(end.0 - start.0 + 1);

        let padding = (end.0 + 1).to_string().len();
        let output_line = |line: usize, content, range| {
            format!(
                "{:pad1$}{}{}\n{:pad2$}{}\n",
                line.to_string().yellow(),
                " | ".black(),
                content,
                "",
                style.line(range),
                pad1 = padding - line.to_string().len(),
                pad2 = padding + 3
            )
        };

        let underlined = if end.0 == start.0 {
            output_line(start.0 + 1, lines.next().unwrap(), start.1..end.1)
        }
        else {
            let mut res = String::new();
            let mut i = 1;
            let len = end.0 - start.0;
            for line in lines {
                res.push_str(&output_line(
                    start.0 + i,
                    line,
                    match i {
                        _ if i == len => 0..end.1,
                        1 => start.1..line.len(),
                        _ => 0..line.len(),
                    },
                ));
                i += 1;
            }
            res
        };
        format!(
            "{}{}{}\n{}",
            " ".repeat(padding),
            "--> ".black(),
            data.black(),
            underlined
        )
    }
}

pub fn overall_span<S: IntoIterator<Item = Span>>(spans: S) -> Option<Span> {
    let mut items = spans.into_iter();
    let mut span = items.next()?;
    for Span(_, range) in items {
        if range.start < span.1.start {
            span.1.start = range.start;
        }
        if range.end > span.1.end {
            span.1.end = range.end;
        }
    }
    Some(span)
}

/// A source file of code. Not necessarily a file, can also be an in-memory
/// stream
pub enum Src {
    Memory { id: SrcID, name: String, data: String },
    File { id: SrcID, path: PathBuf, data: String },
}

impl Src {
    pub fn id(&self) -> SrcID {
        match self {
            Self::Memory { id, name: _, data: _ } => *id,
            Self::File { id, path: _, data: _ } => *id,
        }
    }
    pub fn name(&self) -> String {
        match self {
            Src::Memory { id: _, name, data: _ } => name.clone(),
            Src::File { id: _, path, data: _ } => path.to_string_lossy().to_string(),
        }
    }
    pub fn data(&self) -> &str {
        match self {
            Src::Memory { id: _, name: _, data } => data.as_str(),
            Src::File { id: _, path: _, data } => data.as_str(),
        }
    }
    pub fn cursor(&self) -> SrcCursor<'_> {
        SrcCursor(self, 0)
    }
    pub fn span(&self, range: Range<usize>) -> Span {
        Span(self.id(), range)
    }

    /// Tokenize this source file according to the Language's token type
    pub fn tokenize<T: TokenKind>(&self) -> Vec<Token<T>> {
        let mut tokenizer = Tokenizer::new(self);
        let mut res = Vec::new();
        loop {
            let token = tokenizer.next();
            if token.is_eof() {
                break;
            }
            res.push(token);
        }
        res
    }

    /// Parse this source file into a type
    pub fn parse<T: TokenKind, N: Parse<T>>(&self) -> Node<N> {
        let mut tokenizer = Tokenizer::new(self);
        N::parse(&mut tokenizer)
    }
}

impl Debug for Src {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Memory { id: _, name, data: _ } => f.write_fmt(format_args!("Memory({name:?})")),
            Self::File { id: _, path, data: _ } => f.write_fmt(format_args!("File({path:?})")),
        }
    }
}
impl Display for Src {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name())
    }
}

impl PartialEq for Src {
    fn eq(&self, other: &Self) -> bool {
        // SAFETY: Srcs in unrelated codebases should never be compared
        self.id() == other.id()
    }
}
impl Eq for Src {}
impl Hash for Src {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id().hash(state)
    }
}

pub struct SrcCursor<'s>(&'s Src, usize);
impl<'s> SrcCursor<'s> {
    pub fn peek(&self) -> Option<char> {
        self.0.data()[self.1..].chars().next()
    }
    pub fn peek_n(&self, n: usize) -> Option<char> {
        self.0.data()[self.1..].chars().nth(n)
    }
    pub fn prev(&mut self) -> Option<char> {
        self.0.data()[..self.1]
            .chars()
            .next_back()
            .inspect(|c| self.1 -= c.len_utf8())
    }
    pub fn peek_prev(&self) -> Option<char> {
        self.0.data()[..self.1].chars().next_back()
    }
    pub fn peek_prev_n(&self, n: usize) -> Option<char> {
        self.0.data()[..self.1].chars().nth_back(n)
    }
    /// Advances the cursor if there is a character coming up and the predicate matches
    pub fn next_if<P>(&mut self, predicate: P) -> Option<char>
    where
        P: Fn(char) -> bool,
    {
        if self.peek().is_some_and(predicate) {
            self.next()
        }
        else {
            None
        }
    }
    pub fn src(&self) -> &'s Src {
        self.0
    }
    pub fn pos(&self) -> usize {
        self.1
    }
    pub fn span_from(&self, start: usize) -> Span {
        Span(self.0.id(), start..self.1)
    }
    pub fn data_from(&self, start: usize) -> &'s str {
        &self.0.data()[start..self.1]
    }
}
impl Iterator for SrcCursor<'_> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.data()[self.1..]
            .chars()
            .next()
            .inspect(|c| self.1 += c.len_utf8())
    }
}

#[derive(Debug)]
/// Represents a codebase; i.e. all the source files that result in a single
/// output
pub struct Codebase {
    root_path: PathBuf,
    srcs: Vec<Src>,
}

impl Codebase {
    fn next_id(&self) -> SrcID {
        SrcID(self.srcs.len())
    }

    /// WARNING: Does not actually add any `Src`s to the `Codebase`!
    /// Call `Codebase::add_src` or `Codebase::add_src_recursive` afterwards!
    pub fn new(root_path: &Path) -> Self {
        Self {
            root_path: root_path.to_path_buf(),
            srcs: vec![],
        }
    }
    pub fn add_src_from_memory(&mut self, name: &str, data: &str) -> &Src {
        self.srcs.push(Src::Memory {
            id: self.next_id(),
            name: name.into(),
            data: data.into()
        });
        self.srcs.last().unwrap()
    }
    pub fn add_src(&mut self, path: &Path) -> Result<&Src, String> {
        self.srcs.push(Src::File {
            id: self.next_id(),
            data: fs::read_to_string(path).map_err(|e| format!("Can't read file: {}", e))?,
            path: path.to_path_buf(),
        });
        Ok(self.srcs.last().unwrap())
    }
    pub fn add_src_recursive(&mut self, path: &Path, file_extensions: &[&str]) -> Result<usize, String> {
        if !path.exists() {
            Err("directory does not exist")?;
        }
        if !path.is_dir() {
            Err("path is not a directory")?;
        }
        let files = std::fs::read_dir(path).map_err(|e| e.to_string())?;
        let mut count = 0;
        for file in files {
            let file = file.unwrap();
            let Ok(ty) = file.file_type()
            else {
                continue;
            };
            if ty.is_dir() {
                count += self.add_src_recursive(&file.path(), file_extensions)?;
            }
            else if let Some(ext) = file.path().extension() {
                if file_extensions.iter().any(|e| OsStr::new(e) == ext) {
                    self.add_src(&file.path())?;
                    count += 1;
                }
            }
        }
        Ok(count)
    }
    pub fn get_src(&self, id: SrcID) -> &Src {
        self.srcs.get(id.0).unwrap()
    }

    pub fn root_path(&self) -> &Path {
        &self.root_path
    }
    pub fn iter(&self) -> impl Iterator<Item = &Src> {
        self.into_iter()
    }
}

impl<'a> IntoIterator for &'a Codebase {
    type IntoIter = <&'a Vec<Src> as IntoIterator>::IntoIter;
    type Item = &'a Src;
    fn into_iter(self) -> Self::IntoIter {
        self.srcs.iter()
    }
}
