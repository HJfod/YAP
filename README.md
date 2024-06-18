# Yet Another Parser

Because I keep yapping about parsers :speaking_head:

This is an opinionated library that provides tokenizing, parsing, and type-checking tools for domain-specific programming languages. It is an extraction of the parser from [Dash](https://github.com/HJfod/Dash) into a reusable parser for all my projects so I don't have to rewrite another parser from scratch ever again.

The goals of YAP are to make it easy and simple to create programming languages with:
 - C-like syntax
 - Ergonomic yet strong type systems
 - Powerful macros & compile-time code execution

Progress:
 - [x] Tokenizing into tokens
 - [ ] Parsing into AST
 - [ ] Typechecking
