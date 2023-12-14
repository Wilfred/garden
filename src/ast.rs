//! Syntax tree definitions for Garden.

use std::{fmt::Display, path::PathBuf};

use serde::{Deserialize, Serialize};

/// A position is an offset into source code.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub(crate) struct Position {
    /// The start of this position, relative to the start of the file.
    pub(crate) start_offset: usize,
    /// The end of this position, relative to the start of the file.
    pub(crate) end_offset: usize,
    // TODO: Use LineNumber instead, finding a way to serialize it.
    pub(crate) line_number: usize,
    // TODO: consider storing a &Path to reduce memory usage.
    pub(crate) path: PathBuf,
}

impl Position {
    pub(crate) fn todo() -> Self {
        Self {
            start_offset: 0,
            end_offset: 0,
            line_number: 0,
            path: PathBuf::from("/position/todo"),
        }
    }

    /// Return the merged position of `first` and `second`. Assumes
    /// that `second` occurs after `first`.
    pub(crate) fn merge(first: Self, second: Self) -> Self {
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
pub(crate) struct SourceString {
    /// The offset of this string into the defining file, at the time
    /// of evaluation.
    pub(crate) offset: usize,
    /// The string containing this definition.
    pub(crate) src: String,
}

// TODO: store positions of type hints too.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct TypeName(pub(crate) String);

impl Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct SymbolName(pub(crate) String);

impl Display for SymbolName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl SymbolName {
    pub(crate) fn is_underscore(&self) -> bool {
        self.0 == "_"
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Symbol {
    pub(crate) pos: Position,
    pub(crate) name: SymbolName,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct SymbolWithType {
    pub(crate) symbol: Symbol,
    pub(crate) type_: Option<TypeName>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum BinaryOperatorKind {
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

/// The left hand side of a case in a `match` expression.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Pattern {
    pub(crate) symbol: Symbol,
    pub(crate) argument: Option<Symbol>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expression_ {
    /// ```
    /// match (x) { Some(y) => { z; } _ => { zz; }}
    /// ```
    // TODO: this needs some kind of terminator in the expression
    // case, or it's hard to read. Support something like this:
    // ```
    // match (x) { Some (y) => y + 1; _ => 0 }
    // ```
    Match(Box<Expression>, Vec<(Pattern, Box<Expression>)>),
    /// ```
    /// if (x) { y; }
    /// if (x) { y; } else { z; }
    /// ```
    If(Box<Expression>, Block, Option<Block>),
    /// ```
    /// while (x) { y; z; }
    /// ```
    While(Box<Expression>, Block),
    /// ```
    /// x = y;
    /// ```
    Assign(Symbol, Box<Expression>),
    /// ```
    /// let x = y;
    /// ```
    Let(Symbol, Box<Expression>),
    /// ```
    /// return x;
    /// ```
    Return(Box<Expression>),
    /// ```
    /// 123;
    /// ```
    IntLiteral(i64),
    /// ```
    /// "foo";
    /// ```
    StringLiteral(String),
    /// ```
    /// [x, y];
    /// ```
    ListLiteral(Vec<Expression>),
    /// ```
    /// x + y;
    /// x < y;
    /// x && y;
    /// ```
    BinaryOperator(Box<Expression>, BinaryOperatorKind, Box<Expression>),
    /// ```
    /// x;
    /// ```
    Variable(Symbol),
    /// ```
    /// x();
    /// ```
    Call(Box<Expression>, Vec<Expression>),
    /// ```
    /// foo.bar(x, y)
    /// ```
    MethodCall(Box<Expression>, Symbol, Vec<Expression>),
    /// ```
    /// fun(x, y) { x + y; }
    /// ```
    FunLiteral(FunInfo),
    /// ```
    /// { x; y; }
    /// ```
    Block(Block),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Expression(pub(crate) Position, pub(crate) Expression_);

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Block {
    pub(crate) open_brace: Position,
    pub(crate) exprs: Vec<Expression>,
    pub(crate) close_brace: Position,
    /// If we are entering a block with extra bindings that are only
    /// defined for the duration of the block, pass them here.
    ///
    /// For example:
    /// ```
    /// match (x) { Some(y) => { y + 1; } _ => {}}
    /// ```
    ///
    /// We want `y` to be bound, but only in the block.
    pub(crate) bindings: Vec<(Symbol, Expression)>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ToplevelExpression(pub(crate) String, pub(crate) Expression);

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunInfo {
    pub(crate) src_string: SourceString,
    pub(crate) doc_comment: Option<String>,
    /// The name of the function. This is `None` for closures.
    pub(crate) name: Option<Symbol>,
    pub(crate) params: Vec<SymbolWithType>,
    pub(crate) return_type: Option<TypeName>,
    pub(crate) body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TestInfo {
    pub(crate) src_string: SourceString,
    pub(crate) doc_comment: Option<String>,
    /// The name of the test. This is optional in test definitions.
    pub(crate) name: Option<Symbol>,
    pub(crate) body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct VariantInfo {
    pub(crate) name_sym: Symbol,
    /// Does this variant wrap a value? For example, the Some variant
    /// in Option.
    pub(crate) has_payload: bool,
}

impl Display for VariantInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name_sym.name.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct EnumInfo {
    pub(crate) src_string: SourceString,
    pub(crate) doc_comment: Option<String>,
    pub(crate) name: TypeName,
    pub(crate) variants: Vec<VariantInfo>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum BuiltinMethodKind {
    ListAppend,
    ListGet,
    ListLen,
    StringConcat,
    StringLen,
    StringSubstring,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum MethodKind {
    BuiltinMethod(BuiltinMethodKind),
    UserDefinedMethod(FunInfo),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct MethodInfo {
    /// The type that has this method.
    pub(crate) receiver_type: TypeName,
    /// The name of the receiver in the method definition. This is
    /// typically `self`.
    ///
    /// TODO: this only exists for user-defined methods, so it's
    /// clunky to have it for all methods.
    pub(crate) receiver_name: SymbolName,
    /// The name of the method itself, e.g. `len` in
    /// `some_string.len()`.
    pub(crate) name_sym: Symbol,
    /// User-defined or built-in.
    pub(crate) kind: MethodKind,
}

impl MethodInfo {
    pub(crate) fn doc_comment(&self) -> Option<String> {
        match &self.kind {
            MethodKind::BuiltinMethod(kind) => match kind {
                BuiltinMethodKind::ListAppend => Some(
                    "Return a new list with the value added to the end.

```
[10].append(11); // [10, 11]
```"
                    .to_owned(),
                ),
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
pub(crate) enum Definition_ {
    /// ```garden
    /// fun foo() {}
    /// ```
    Fun(Symbol, FunInfo),
    /// ```garden
    /// fun (self: List) foo() {}
    /// ```
    Method(MethodInfo),
    Test(TestInfo),
    Enum(EnumInfo),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Definition(
    pub(crate) SourceString,
    pub(crate) Position,
    pub(crate) Definition_,
);

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ToplevelItem {
    Def(Definition),
    Expr(ToplevelExpression),
}
