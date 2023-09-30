//! Syntax tree definitions for Garden.

use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::eval::ErrorKind;

/// A position is an offset into source code.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Position {
    pub start_offset: usize,
    pub end_offset: usize,
    // TODO: Use LineNumber instead, finding a way to serialize it.
    pub line_number: usize,
    // TODO: consider storing a &Path to reduce memory usage.
    pub path: PathBuf,
}

impl Position {
    pub fn todo() -> Self {
        Self {
            start_offset: 0,
            end_offset: 0,
            line_number: 0,
            path: PathBuf::from("/position/todo"),
        }
    }

    /// Return the merged position of `first` and `second`. Assumes
    /// that `second` occurs after `first`.
    pub fn merge(first: Self, second: Self) -> Self {
        Self {
            start_offset: first.start_offset,
            end_offset: second.end_offset,
            line_number: first.line_number,
            path: first.path,
        }
    }
}

/// An owned string of the source text associated with a definition.
#[derive(Clone, Debug, PartialEq)]
pub struct SourceString {
    /// The offset of this string into the defining file, at the time
    /// of evaluation.
    pub offset: usize,
    /// The string containing this definition.
    pub src: String,
}

// TODO: store positions of type hints too.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeName(pub String);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SymbolName(pub String);

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol(pub Position, pub SymbolName);

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolWithType(pub Symbol, pub Option<TypeName>);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperatorKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression_ {
    If(Box<Expression>, Block, Option<Block>),
    While(Box<Expression>, Block),
    Assign(Symbol, Box<Expression>),
    Let(Symbol, Box<Expression>),
    Return(Box<Expression>),
    IntLiteral(i64),
    StringLiteral(String),
    BoolLiteral(bool),
    ListLiteral(Vec<Expression>),
    BinaryOperator(Box<Expression>, BinaryOperatorKind, Box<Expression>),
    Variable(Symbol),
    Call(Box<Expression>, Vec<Expression>),
    /// ```
    /// foo.bar(x, y)
    /// ```
    MethodCall(Box<Expression>, Symbol, Vec<Expression>),
    FunLiteral(FunInfo),
    Stop(Option<ErrorKind>),
    Block(Block),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression(pub Position, pub Expression_);

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub open_brace: Position,
    pub exprs: Vec<Expression>,
    pub close_brace: Position,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ToplevelExpression(pub String, pub Expression);

#[derive(Debug, Clone, PartialEq)]
pub struct FunInfo {
    pub src_string: SourceString,
    pub doc_comment: Option<String>,
    /// The name of the function. This is `None` for closures.
    pub name: Option<Symbol>,
    pub params: Vec<SymbolWithType>,
    pub return_type: Option<TypeName>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TestInfo {
    pub src_string: SourceString,
    pub doc_comment: Option<String>,
    /// The name of the test. This is optional in test definitions.
    pub name: Option<Symbol>,
    pub body: Block,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BuiltinMethodKind {
    ListGet,
    ListLen,
    StringConcat,
    StringLen,
    StringSubstring,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MethodKind {
    BuiltinMethod(BuiltinMethodKind),
    UserDefinedMethod(FunInfo),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodInfo {
    /// The type that has this method.
    pub receiver_type: TypeName,
    /// The name of the receiver in the method definition. This is
    /// typically `self`.
    ///
    /// TODO: this only exists for user-defined methods, so it's
    /// clunky to have it for all methods.
    pub receiver_name: SymbolName,
    /// The name of the method itself, e.g. `len` in
    /// `some_string.len()`.
    pub name: Symbol,
    /// User-defined or built-in.
    pub kind: MethodKind,
}

impl MethodInfo {
    pub fn doc_comment(&self) -> Option<String> {
        match &self.kind {
            MethodKind::BuiltinMethod(kind) => match kind {
                BuiltinMethodKind::ListGet => Some(
                    "Get the item in the list at the index specified.
Errors if the index is less than 0 or greater than length - 1.

```
[4, 5, 6].get(1); // 5
```"
                    .to_owned(),
                ),
                BuiltinMethodKind::ListLen => Some(
                    "Return the length of the list.

```
[10, 11, 12].len(); // 3
```"
                    .to_owned(),
                ),
                BuiltinMethodKind::StringConcat => Some(
                    "Return a new string with the two string arguments concatenated.

```
\"foo\".concat(\"bar\"); // \"foobar\"
```"
                    .to_owned(),
                ),
                BuiltinMethodKind::StringLen => Some(
                    "Return the number of characters (codepoints) in the string.

```
\"abc\".len(); // 3
```"
                    .to_owned(),
                ),
                BuiltinMethodKind::StringSubstring => Some(
                    "Return the substring of the string between the indexes specified.

```
\"abcdef\".substring(1, 3); // \"bc\"
```"
                    .to_owned(),
                ),
            },
            MethodKind::UserDefinedMethod(fun_info) => fun_info.doc_comment.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition_ {
    /// ```garden
    /// fun foo() {}
    /// ```
    FunDefinition(Symbol, FunInfo),
    /// ```garden
    /// fun (self: List) foo() {}
    /// ```
    MethodDefinition(MethodInfo),
    TestDefinition(TestInfo),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Definition(pub SourceString, pub Position, pub Definition_);

#[derive(Debug, Clone, PartialEq)]
pub enum ToplevelItem {
    Def(Definition),
    Expr(ToplevelExpression),
}
