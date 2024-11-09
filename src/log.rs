// use crate::src::{Codebase, Span};
// use colored::Colorize;
// use std::fmt::{Display, Write};

// use super::src::Underline;

// #[allow(unused)]
// #[derive(Debug, Clone, Copy, PartialEq)]
// /// Represents the importance of a diagnostic message
// pub enum Level {
//     /// This is just some normal compiler information that is probably of
//     /// interest but not critical
//     Info,
//     /// Something might go wrong at runtime, but compilation won't stop
//     Warning,
//     /// Compilation is impossible
//     Error,
// }

// impl Level {
//     fn underline_style(&self) -> Underline {
//         match self {
//             Self::Error => Underline::Squiggle,
//             Self::Warning => Underline::Highlight,
//             Self::Info => Underline::Normal,
//         }
//     }
// }

// impl Display for Level {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "{}",
//             match self {
//                 Level::Info => "Info".bold(),
//                 Level::Warning => "Warning".bold().yellow(),
//                 Level::Error => "Error".bold().red(),
//             }
//         )
//     }
// }

// #[derive(Debug)]
// enum NoteKind {
//     Note,
//     Hint,
// }

// impl NoteKind {
//     fn underline_style(&self) -> Underline {
//         match self {
//             Self::Note => Underline::Normal,
//             Self::Hint => Underline::Highlight,
//         }
//     }
// }

// impl Display for NoteKind {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::Hint => f.write_str("Hint"),
//             Self::Note => f.write_str("Note"),
//         }
//     }
// }

// #[derive(Debug)]
// pub struct Note {
//     info: String,
//     at: Option<Span>,
//     kind: NoteKind,
// }

// impl Note {
//     pub fn new<S: Into<String>>(info: S, hint: bool) -> Self {
//         Self {
//             info: info.into(),
//             at: None,
//             kind: if hint { NoteKind::Hint } else { NoteKind::Note },
//         }
//     }
//     pub fn new_at<S: Into<String>>(info: S, span: Span) -> Self {
//         Self {
//             info: info.into(),
//             at: Some(span),
//             kind: NoteKind::Note,
//         }
//     }
//     pub fn hint<S: Into<String>>(info: S, span: Span) -> Self {
//         Self {
//             info: info.into(),
//             at: Some(span),
//             kind: NoteKind::Hint,
//         }
//     }

//     pub fn display(&self, codebase: &Codebase) -> String {
//         if let Some(ref span) = self.at {
//             format!(
//                 "{}:\n{}{}",
//                 self.kind.to_string().bold(),
//                 span.underlined(codebase, self.kind.underline_style()),
//                 self.info
//             )
//         }
//         else {
//             format!("{}: {}", self.kind.to_string().bold(), self.info)
//         }
//     }
// }

// #[derive(Debug)]
// pub struct Message {
//     pub(crate) level: Level,
//     info: String,
//     notes: Vec<Note>,
//     span: Span,
// }

// impl Message {
//     pub fn new<S: Display>(level: Level, info: S, span: Span) -> Self {
//         Self {
//             level,
//             info: info.to_string(),
//             notes: vec![],
//             span,
//         }
//     }
//     pub fn note(mut self, note: Note) -> Self {
//         self.notes.push(note);
//         self
//     }

//     pub fn display(&self, codebase: &Codebase) -> String {
//         // todo: migrate to https://crates.io/crates/lyneate mayhaps

//         fn indent(msg: &str) -> String {
//             let mut lines = msg.lines();
//             let first = lines.next().unwrap_or_default();
//             lines.fold(first.to_string(), |mut acc, line| {
//                 write!(&mut acc, "\n{:>3}{}", "", line).unwrap();
//                 acc
//             })
//         }

//         format!(
//             "{}:\n{}{}\n{}",
//             self.level,
//             self.span.underlined(codebase, self.level.underline_style()),
//             self.info,
//             self.notes.iter().fold(String::new(), |mut acc, note| {
//                 write!(&mut acc, "\n + {}\n", indent(&note.display(codebase))).unwrap();
//                 acc
//             })
//         )
//     }
// }

// #[derive(Default)]
// pub struct Logger {
//     messages: Vec<Message>,
// }

// impl std::fmt::Debug for Logger {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         f.write_str("Logger")
//     }
// }

// impl Logger {
//     pub fn new() -> Self {
//         Self {
//             messages: vec![],
//         }
//     }
//     pub fn release_logs<F: FnMut(Message, &Codebase)>(&mut self, codebase: &Codebase, mut logger: F) {
//         for msg in std::mem::take(&mut self.messages) {
//             logger(msg, codebase);
//         }
//     }
//     pub fn log(&mut self, msg: Message) {
//         self.messages.push(msg);
//     }
//     pub fn error_count(&self) -> usize {
//         self.messages.iter().filter(|m| m.level == Level::Error).count()
//     }
//     pub fn warn_count(&self) -> usize {
//         self.messages.iter().filter(|m| m.level == Level::Warning).count()
//     }
// }

// pub fn default_console_logger(msg: Message, codebase: &Codebase) {
//     println!("{}", msg.display(codebase));
// }
