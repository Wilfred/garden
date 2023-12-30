use std::path::Path;

use lazy_static::lazy_static;
use line_numbers::LinePositions;
use regex::Regex;

use crate::diagnostics::ErrorMessage;
use crate::{ast::Position, ParseError};

lazy_static! {
    pub(crate) static ref INTEGER_RE: Regex = Regex::new(r"^-?[0-9]+").unwrap();
    pub(crate) static ref STRING_RE: Regex = Regex::new(r#"^"(\\"|[^"])*""#).unwrap();
    pub(crate) static ref SYMBOL_RE: Regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Token<'a> {
    pub(crate) position: Position,
    pub(crate) text: &'a str,
    pub(crate) preceding_comments: Vec<(Position, &'a str)>,
}

#[derive(Debug, Clone)]
pub(crate) struct TokenStream<'a> {
    tokens: Vec<Token<'a>>,
    pub(crate) idx: usize,
}

impl<'a> TokenStream<'a> {
    pub(crate) fn is_empty(&self) -> bool {
        self.tokens.get(self.idx).is_none()
    }

    pub(crate) fn pop(&mut self) -> Option<Token<'a>> {
        match self.tokens.get(self.idx) {
            Some(token) => {
                self.idx += 1;
                Some(token.clone())
            }
            None => None,
        }
    }

    pub(crate) fn peek(&self) -> Option<Token<'a>> {
        self.tokens.get(self.idx).cloned()
    }

    pub(crate) fn peek_two(&self) -> Option<(Token<'a>, Token<'a>)> {
        match (self.tokens.get(self.idx), self.tokens.get(self.idx + 1)) {
            (Some(token1), Some(token2)) => Some((token1.clone(), token2.clone())),
            _ => None,
        }
    }

    pub(crate) fn prev(&self) -> Option<Token<'a>> {
        if self.idx == 0 {
            return None;
        }

        self.tokens.get(self.idx - 1).cloned()
    }
}

pub(crate) fn lex_between<'a>(
    path: &Path,
    s: &'a str,
    offset: usize,
    end_offset: usize,
) -> Result<TokenStream<'a>, ParseError> {
    assert!(end_offset <= s.len());

    let lp = LinePositions::from(s);
    let mut tokens: Vec<Token<'a>> = vec![];

    let mut preceding_comments = vec![];
    let mut offset = offset;

    // Skip shebang if present at the beginning of the file.
    if offset == 0 && s.starts_with('#') {
        offset = s.find('\n').unwrap_or(s.len());
    }

    'outer: while offset < end_offset {
        let s = &s[offset..];

        // Skip over comments.
        if s.starts_with("//") {
            if let Some(i) = s.find('\n') {
                preceding_comments.push((
                    Position {
                        start_offset: offset,
                        end_offset: offset + i,
                        line_number: lp.from_offset(offset).as_usize(),
                        path: path.to_path_buf(),
                    },
                    &s["//".len()..i + 1],
                ));
                offset += i + 1;
                continue;
            } else {
                offset += s.len();
                break;
            }
        }

        // If we see a blank line, discard any comments. We're only
        // interested in comments immediately before definitions.
        if s.starts_with('\n') {
            offset += 1;
            preceding_comments = vec![];
            continue;
        }

        // Skip over other whitespace.
        if let Some(first_char) = s.chars().next() {
            if first_char.is_whitespace() {
                offset += 1;
                continue;
            }
        } else {
            break;
        }

        for token_str in ["==", "!=", ">=", "<=", "&&", "||", "=>"] {
            if s.starts_with(token_str) {
                tokens.push(Token {
                    position: Position {
                        start_offset: offset,
                        end_offset: offset + token_str.len(),
                        line_number: lp.from_offset(offset).as_usize(),
                        path: path.to_path_buf(),
                    },
                    text: &s[0..token_str.len()],
                    preceding_comments,
                });
                preceding_comments = vec![];

                offset += token_str.len();
                continue 'outer;
            }
        }

        // Match integers before binary operators, so -1 is treated as
        // a single integer literal, not the tokens - followed by 1.
        if let Some(integer_match) = INTEGER_RE.find(s) {
            tokens.push(Token {
                position: Position {
                    start_offset: offset,
                    end_offset: offset + integer_match.end(),
                    line_number: lp.from_offset(offset).as_usize(),
                    path: path.to_path_buf(),
                },
                text: integer_match.as_str(),
                preceding_comments,
            });
            preceding_comments = vec![];

            offset += integer_match.end();
            continue;
        }

        for token_char in [
            '+', '-', '*', '/', '(', ')', '{', '}', ';', '=', ',', '<', '>', '[', ']', '.', ':',
        ] {
            if s.starts_with(token_char) {
                tokens.push(Token {
                    position: Position {
                        start_offset: offset,
                        end_offset: offset + 1,
                        line_number: lp.from_offset(offset).as_usize(),
                        path: path.to_path_buf(),
                    },
                    text: &s[0..1],
                    preceding_comments,
                });
                preceding_comments = vec![];

                offset += 1;
                continue 'outer;
            }
        }
        if let Some(string_match) = STRING_RE.find(s) {
            tokens.push(Token {
                position: Position {
                    start_offset: offset,
                    end_offset: offset + string_match.end(),
                    line_number: lp.from_offset(offset).as_usize(),
                    path: path.to_path_buf(),
                },
                text: string_match.as_str(),
                preceding_comments,
            });
            preceding_comments = vec![];

            offset += string_match.end();
        } else if let Some(variable_match) = SYMBOL_RE.find(s) {
            tokens.push(Token {
                position: Position {
                    start_offset: offset,
                    end_offset: offset + variable_match.end(),
                    line_number: lp.from_offset(offset).as_usize(),
                    path: path.to_path_buf(),
                },
                text: variable_match.as_str(),
                preceding_comments,
            });
            preceding_comments = vec![];

            offset += variable_match.end();
        } else {
            break;
        }
    }

    if offset != end_offset {
        return Err(ParseError::Invalid {
            position: Position {
                start_offset: offset,
                end_offset: s.len(),
                line_number: lp.from_offset(offset).as_usize(),
                path: path.to_path_buf(),
            },
            message: ErrorMessage(format!("Unrecognized syntax: '{}'", &s[offset..])),
            additional: vec![],
        });
    }

    Ok(TokenStream { tokens, idx: 0 })
}

pub(crate) fn lex<'a>(path: &Path, s: &'a str) -> Result<TokenStream<'a>, ParseError> {
    lex_between(path, s, 0, s.len())
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    #[test]
    fn test_lex_no_offset() {
        let tokens = lex(&PathBuf::from("__test.gdn"), "1").unwrap();
        assert_eq!(
            tokens.peek(),
            Some(Token {
                position: Position {
                    start_offset: 0,
                    end_offset: 1,
                    line_number: 0,
                    path: PathBuf::from("__test.gdn")
                },
                text: "1",
                preceding_comments: vec![],
            })
        );
    }

    #[test]
    fn test_lex_with_offset() {
        let tokens = lex(&PathBuf::from("__test.gdn"), " a").unwrap();
        assert_eq!(
            tokens.peek(),
            Some(Token {
                position: Position {
                    start_offset: 1,
                    end_offset: 2,
                    line_number: 0,
                    path: PathBuf::from("__test.gdn")
                },
                text: "a",
                preceding_comments: vec![],
            })
        );
    }

    #[test]
    fn test_lex_spaces() {
        assert_eq!(
            lex(&PathBuf::from("__test.gdn"), "1 + 2")
                .unwrap()
                .tokens
                .iter()
                .map(|token| token.text)
                .collect::<Vec<_>>(),
            vec!["1", "+", "2"]
        );
    }

    #[test]
    fn test_lex_no_spaces() {
        assert_eq!(
            lex(&PathBuf::from("__test.gdn"), "1+2")
                .unwrap()
                .tokens
                .iter()
                .map(|token| token.text)
                .collect::<Vec<_>>(),
            vec!["1", "+", "2"]
        );
    }

    #[test]
    fn test_lex_comment() {
        let tokens = lex(&PathBuf::from("__test.gdn"), "// 2\n1").unwrap();
        assert_eq!(
            tokens.peek(),
            Some(Token {
                position: Position {
                    start_offset: 5,
                    end_offset: 6,
                    line_number: 1,
                    path: PathBuf::from("__test.gdn")
                },
                text: "1",
                preceding_comments: vec![(
                    Position {
                        start_offset: 0,
                        end_offset: 4,
                        line_number: 0,
                        path: PathBuf::from("__test.gdn")
                    },
                    " 2\n"
                )],
            })
        );
    }

    #[test]
    fn test_lex_comment_not_touching() {
        let tokens = lex(&PathBuf::from("__test.gdn"), "// 2\n\n1").unwrap();
        assert_eq!(
            tokens.peek(),
            Some(Token {
                position: Position {
                    start_offset: 6,
                    end_offset: 7,
                    line_number: 2,
                    path: PathBuf::from("__test.gdn")
                },
                text: "1",
                preceding_comments: vec![],
            })
        );
    }

    #[test]
    fn test_lex_comment_leading_newline() {
        assert!(lex(&PathBuf::from("__test.gdn"), "\n// 2")
            .unwrap()
            .is_empty());
    }

    #[test]
    fn test_lex_standalone_comment() {
        assert!(lex(&PathBuf::from("__test.gdn"), "// foo")
            .unwrap()
            .is_empty());
    }
}
