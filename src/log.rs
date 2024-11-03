use crate::src::Span;
use colored::Colorize;
use std::fmt::{Display, Write};

use super::src::Underline;

#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq)]
/// Represents the importance of a diagnostic message
pub enum Level {
    /// This is just some normal compiler information that is probably of
    /// interest but not critical
    Info,
    /// Something might go wrong at runtime, but compilation won't stop
    Warning,
    /// Compilation is impossible
    Error,
}

impl Level {
    fn underline_style(&self) -> Underline {
        match self {
            Self::Error => Underline::Squiggle,
            Self::Warning => Underline::Highlight,
            Self::Info => Underline::Normal,
        }
    }
}

impl Display for Level {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Level::Info => "Info".bold(),
                Level::Warning => "Warning".bold().yellow(),
                Level::Error => "Error".bold().red(),
            }
        )
    }
}

#[derive(Debug)]
enum NoteKind {
    Note,
    Hint,
}

impl NoteKind {
    fn underline_style(&self) -> Underline {
        match self {
            Self::Note => Underline::Normal,
            Self::Hint => Underline::Highlight,
        }
    }
}

impl Display for NoteKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Hint => f.write_str("Hint"),
            Self::Note => f.write_str("Note"),
        }
    }
}

#[derive(Debug)]
pub struct Note<'s> {
    info: String,
    at: Option<Span<'s>>,
    kind: NoteKind,
}

impl<'s> Note<'s> {
    pub fn new<S: Into<String>>(info: S, hint: bool) -> Self {
        Self {
            info: info.into(),
            at: None,
            kind: if hint { NoteKind::Hint } else { NoteKind::Note },
        }
    }
    pub fn new_at<S: Into<String>>(info: S, span: Span<'s>) -> Self {
        Self {
            info: info.into(),
            at: Some(span),
            kind: NoteKind::Note,
        }
    }
    pub fn hint<S: Into<String>>(info: S, span: Span<'s>) -> Self {
        Self {
            info: info.into(),
            at: Some(span),
            kind: NoteKind::Hint,
        }
    }
}

impl<'s> Display for Note<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref span) = self.at {
            write!(
                f,
                "{}:\n{}{}",
                self.kind.to_string().bold(),
                span.underlined(self.kind.underline_style()),
                self.info
            )
        }
        else {
            write!(f, "{}: {}", self.kind.to_string().bold(), self.info)
        }
    }
}

#[derive(Debug)]
pub struct Message<'s> {
    pub(crate) level: Level,
    info: String,
    notes: Vec<Note<'s>>,
    span: Span<'s>,
}

impl<'s> Message<'s> {
    pub fn new<S: Display>(level: Level, info: S, span: Span<'s>) -> Self {
        Self {
            level,
            info: info.to_string(),
            notes: vec![],
            span,
        }
    }
    pub fn note(mut self, note: Note<'s>) -> Self {
        self.notes.push(note);
        self
    }
}

impl<'s> Display for Message<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // todo: migrate to https://crates.io/crates/lyneate mayhaps

        fn indent(msg: &str) -> String {
            let mut lines = msg.lines();
            let first = lines.next().unwrap_or_default();
            lines.fold(first.to_string(), |mut acc, line| {
                write!(&mut acc, "\n{:>3}{}", "", line).unwrap();
                acc
            })
        }

        f.write_fmt(format_args!(
            "{}:\n{}{}\n{}",
            self.level,
            self.span.underlined(self.level.underline_style()),
            self.info,
            self.notes.iter().fold(String::new(), |mut acc, note| {
                write!(&mut acc, "\n + {}\n", indent(&note.to_string())).unwrap();
                acc
            })
        ))
    }
}

pub struct Logger<'s> {
    #[allow(clippy::type_complexity)]
    logger: Box<dyn FnMut(Message<'s>)>,
    error_count: usize,
    warn_count: usize,
}

impl<'s> std::fmt::Debug for Logger<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Logger")
    }
}

impl<'s> Logger<'s> {
    pub fn new<F: FnMut(Message<'s>) + 'static>(logger: F) -> Self {
        Self {
            logger: Box::from(logger),
            error_count: 0,
            warn_count: 0,
        }
    }
    #[allow(clippy::should_implement_trait)]
    pub fn default() -> Self {
        Self::new(default_console_logger)
    }
    pub fn log(&mut self, msg: Message<'s>) {
        match msg.level {
            Level::Info => {}
            Level::Warning => self.warn_count += 1,
            Level::Error => self.error_count += 1,
        }
        (self.logger)(msg);
    }
    pub fn error_count(&self) -> usize {
        self.error_count
    }
    pub fn warn_count(&self) -> usize {
        self.warn_count
    }
}

pub fn default_console_logger(msg: Message<'_>) {
    println!("{msg}");
}
