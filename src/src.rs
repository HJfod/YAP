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
use crate::parse::token::{Token, TokenIterator, TokenKind, Tokenizer};

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

#[derive(Debug)]
/// Represents a specific span of source code in a `Src`
pub struct Span<'s>(pub &'s Src, pub Range<usize>);

impl<'s> Span<'s> {
    pub fn builtin() -> Self {
        Self(Src::builtin(), 0..0)
    }
    pub fn data(&self) -> &'s str {
        &self.0.data()[self.1.clone()]
    }
    pub fn underlined(&self, style: Underline) -> String {
        // Get the starting and ending linecols as 0-based indices
        let sub_tuple = |a: (usize, usize)| (a.0 - 1, a.1 - 1);
        let lookup = LineColLookup::new(self.0.data());
        let start = sub_tuple(lookup.get(self.1.start));
        let end = sub_tuple(lookup.get(self.1.end));

        let mut lines = self
            .0
            .data()
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
            self.to_string().black(),
            underlined
        )
    }
}

impl Clone for Span<'_> {
    fn clone(&self) -> Self {
        Self(self.0, self.1.clone())
    }
}
impl Display for Span<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lookup = LineColLookup::new(self.0.data());
        let start = lookup.get(self.1.start);
        if self.1.is_empty() {
            write!(f, "{}:{}:{}", self.0.name(), start.0, start.1)
        }
        else {
            let end = lookup.get(self.1.end);
            write!(
                f,
                "{}:{}:{}-{}:{}",
                self.0.name(),
                start.0,
                start.1,
                end.0,
                end.1
            )
        }
    }
}

pub fn overall_span<'s, S: IntoIterator<Item = Span<'s>>>(spans: S) -> Option<Span<'s>> {
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
    Builtin,
    Memory { name: String, data: String },
    File { path: PathBuf, data: String },
}

impl Src {
    pub fn builtin() -> &'static Self {
        &Src::Builtin
    }
    pub fn from_memory<S: Into<String>, D: Into<String>>(name: S, data: D) -> Self {
        Src::Memory {
            name: name.into(),
            data: data.into(),
        }
    }
    pub fn from_file(path: &Path) -> Result<Self, String> {
        Ok(Src::File {
            data: fs::read_to_string(path).map_err(|e| format!("Can't read file: {}", e))?,
            path: path.to_path_buf(),
        })
    }
    pub fn name(&self) -> String {
        match self {
            Src::Builtin => String::from("<compiler built-in>"),
            Src::Memory { name, data: _ } => name.clone(),
            Src::File { path, data: _ } => path.to_string_lossy().to_string(),
        }
    }
    pub fn data(&self) -> &str {
        match self {
            Src::Builtin => "",
            Src::Memory { name: _, data } => data.as_str(),
            Src::File { path: _, data } => data.as_str(),
        }
    }
    pub fn cursor(&self) -> SrcCursor<'_> {
        SrcCursor(self, 0)
    }

    /// Tokenize this source file according to the Language's token type
    pub fn tokenize<'s, T: TokenKind<'s>>(&'s self) -> Vec<Token<'s, T>> {
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
}

impl Debug for Src {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Builtin => f.write_str("Builtin"),
            Self::Memory { name, data: _ } => f.write_fmt(format_args!("Memory({name:?})")),
            Self::File { path, data: _ } => f.write_fmt(format_args!("File({path:?})")),
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
        match (self, other) {
            (Src::Builtin, Src::Builtin) => true,
            (Src::Memory { name: a, data: ad }, Src::Memory { name: b, data: bd }) => {
                a == b && ad == bd
            }
            (Src::File { path: a, data: _ }, Src::File { path: b, data: _ }) => a == b,
            (_, _) => false,
        }
    }
}
impl Eq for Src {}
impl Hash for Src {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Builtin => 0.hash(state),
            Self::Memory { name, data } => {
                name.hash(state);
                data.hash(state);
            }
            Self::File { path, data: _ } => path.hash(state),
        }
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
    pub fn span_from(&self, start: usize) -> Span<'s> {
        Span(self.0, start..self.1)
    }
}
impl<'s> Iterator for SrcCursor<'s> {
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
    path: PathBuf,
    srcs: Vec<Src>,
}

impl Codebase {
    /// Loads a codebase from a path. Can be a source file directory for a
    /// single-file codebase or a directory with deeply nested source files
    pub fn new_from_path(path: &Path, file_extensions: &[&str]) -> Result<Self, String> {
        if path.is_file() {
            return Ok(Codebase {
                path: path.to_path_buf(),
                srcs: vec![Src::from_file(path)?],
            });
        }
        if !path.exists() {
            Err("Directory does not exist".to_string())?;
        }
        let srcs = Self::find_src_files(path, file_extensions)?;
        if srcs.is_empty() {
            Err("No source files found in directory or its subdirectories".to_string())
        }
        else {
            Ok(Codebase {
                path: path.to_path_buf(),
                srcs,
            })
        }
    }
    fn find_src_files(dir: &Path, file_extensions: &[&str]) -> Result<Vec<Src>, String> {
        let Ok(files) = std::fs::read_dir(dir)
        else {
            return Ok(vec![]);
        };

        let mut res = vec![];
        for file in files {
            let file = file.unwrap();
            let Ok(ty) = file.file_type()
            else {
                continue;
            };
            if ty.is_dir() {
                res.extend(Self::find_src_files(&file.path(), file_extensions)?);
            }
            else if let Some(ext) = file.path().extension() {
                if file_extensions.iter().any(|e| OsStr::new(e) == ext) {
                    res.push(Src::from_file(&file.path())?);
                }
            }
        }
        Ok(res)
    }
    pub fn path(&self) -> &Path {
        &self.path
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
