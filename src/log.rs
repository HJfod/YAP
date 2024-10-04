use crate::{lang::Language, src::Span};
use colored::Colorize;
use std::{
    fmt::{Display, Write},
    sync::{Arc, Mutex},
};

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
pub struct Note<'s, L: Language> {
    info: String,
    at: Option<Span<'s, L>>,
    kind: NoteKind,
}

impl<'s, L: Language> Note<'s, L> {
    pub fn new<S: Into<String>>(info: S, hint: bool) -> Self {
        Self {
            info: info.into(),
            at: None,
            kind: if hint { NoteKind::Hint } else { NoteKind::Note },
        }
    }
    pub fn new_at<S: Into<String>>(info: S, span: Span<'s, L>) -> Self {
        Self {
            info: info.into(),
            at: Some(span),
            kind: NoteKind::Note,
        }
    }
    pub fn hint<S: Into<String>>(info: S, span: Span<'s, L>) -> Self {
        Self {
            info: info.into(),
            at: Some(span),
            kind: NoteKind::Hint,
        }
    }
}

impl<'s, L: Language> Display for Note<'s, L> {
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
pub struct Message<'s, L: Language> {
    pub(crate) level: Level,
    info: String,
    notes: Vec<Note<'s, L>>,
    span: Span<'s, L>,
}

impl<'s, L: Language> Message<'s, L> {
    pub fn new<S: Display>(level: Level, info: S, span: Span<'s, L>) -> Self {
        Self {
            level,
            info: info.to_string(),
            notes: vec![],
            span,
        }
    }
    pub fn note(mut self, note: Note<'s, L>) -> Self {
        self.notes.push(note);
        self
    }
}

impl<'s, L: Language> Display for Message<'s, L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // todo: migrate to https://crates.io/crates/lyneate mayhaps

        fn indent(msg: &str) -> String {
            let mut lines = msg.lines();
            let first = lines.next().unwrap_or_default();
            lines.fold(first.to_string(), |mut acc, l| {
                write!(&mut acc, "\n{:>3}{}", "", l).unwrap();
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

pub struct Logger<L: Language> {
    #[allow(clippy::type_complexity)]
    logger: Box<dyn FnMut(Message<'_, L>)>,
    error_count: usize,
    warn_count: usize,
}

impl<L: Language> std::fmt::Debug for Logger<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Logger")
    }
}

impl<L: Language> Logger<L> {
    pub fn new<F: FnMut(Message<'_, L>) + 'static>(logger: F) -> LoggerRef<L> {
        Arc::from(Mutex::from(Self {
            logger: Box::from(logger),
            error_count: 0,
            warn_count: 0,
        }))
    }
    #[allow(clippy::should_implement_trait)]
    pub fn default() -> LoggerRef<L> {
        Self::new(default_console_logger)
    }
    pub fn log(&mut self, msg: Message<'_, L>) {
        match msg.level {
            Level::Info => {}
            Level::Warning => self.warn_count += 1,
            Level::Error => self.error_count += 1,
        }
        (self.logger)(msg);
    }
    pub fn errors(&self) -> usize {
        self.error_count
    }
    pub fn warnings(&self) -> usize {
        self.warn_count
    }
}

pub(crate) type LoggerRef<L> = Arc<Mutex<Logger<L>>>;

pub fn default_console_logger<L: Language>(msg: Message<'_, L>) {
    println!("{msg}");
}
