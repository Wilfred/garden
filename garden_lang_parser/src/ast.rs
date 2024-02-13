//! Syntax tree definitions for Garden.

use std::{fmt::Display, path::PathBuf};

use serde::{Deserialize, Serialize};

/// A position is an offset into source code.
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Position {
    /// The start of this position, relative to the start of the file.
    pub start_offset: usize,
    /// The end of this position, relative to the start of the file.
    pub end_offset: usize,
    // TODO: Use LineNumber instead, finding a way to serialize it.
    pub line_number: usize,
    // TODO: consider storing a &Path to reduce memory usage.
    pub path: PathBuf,
}

impl std::fmt::Debug for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if std::env::var("VERBOSE").is_ok() {
            f.debug_struct("Position")
                .field("start_offset", &self.start_offset)
                .field("end_offset", &self.end_offset)
                .field("line_number", &self.line_number)
                .finish()
        } else {
            if self.path == PathBuf::from("/position/todo") {
                f.write_str("Position { TODO }")
            } else {
                f.write_str("Position { ... }")
            }
        }
    }
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
#[derive(Clone, PartialEq)]
pub struct SourceString {
    /// The offset of this string into the defining file, at the time
    /// of evaluation.
    pub offset: usize,
    /// The string containing this definition.
    pub src: String,
}

impl std::fmt::Debug for SourceString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if std::env::var("VERBOSE").is_ok() {
            f.debug_struct("SourceString")
                .field("offset", &self.offset)
                .field("src", &self.src)
                .finish()
        } else {
            f.write_str("SourceString")
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TypeName {
    pub name: String,
}

impl std::fmt::Debug for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
impl Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, PartialEq)]
pub struct TypeSymbol {
    pub name: TypeName,
    pub position: Position,
}

impl std::fmt::Debug for TypeSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if std::env::var("VERBOSE").is_ok() {
            f.debug_struct("TypeSymbol")
                .field("name", &self.name)
                .field("position", &self.position)
                .finish()
        } else {
            write!(f, "TypeSymbol\"{}\"", self.name.name)
        }
    }
}

impl Display for TypeSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeHint {
    pub sym: TypeSymbol,
    pub args: Vec<TypeHint>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SymbolName(pub String);

impl Display for SymbolName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl SymbolName {
    pub fn is_underscore(&self) -> bool {
        self.0 == "_"
    }
}

#[derive(Clone, PartialEq)]
pub struct Symbol {
    pub position: Position,
    pub name: SymbolName,
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if std::env::var("VERBOSE").is_ok() {
            f.debug_struct("Symbol")
                .field("name", &self.name)
                .field("position", &self.position)
                .finish()
        } else {
            write!(f, "Symbol\"{}\"", self.name.0)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolWithType {
    pub symbol: Symbol,
    pub type_: Option<TypeHint>,
}

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

/// The left hand side of a case in a `match` expression.
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub symbol: Symbol,
    pub argument: Option<Symbol>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression_ {
    /// ```garden
    /// match (x) {
    ///     None => { get_value(); }
    ///     Some(y) => y + 1,
    ///     _ => error("yikes"),
    /// }
    /// ```
    Match(Box<Expression>, Vec<(Pattern, Box<Expression>)>),
    /// ```garden
    /// if (x) { y; }
    /// if (x) { y; } else { z; }
    /// ```
    If(Box<Expression>, Block, Option<Block>),
    /// ```garden
    /// while (x) { y; z; }
    /// ```
    While(Box<Expression>, Block),
    /// ```garden
    /// x = y;
    /// ```
    Assign(Symbol, Box<Expression>),
    /// ```garden
    /// let x = y;
    /// ```
    Let(Symbol, Box<Expression>),
    /// ```garden
    /// return x;
    /// ```
    Return(Box<Expression>),
    /// ```garden
    /// 123;
    /// ```
    IntLiteral(i64),
    /// ```garden
    /// "foo";
    /// ```
    StringLiteral(String),
    /// ```garden
    /// [x, y];
    /// ```
    ListLiteral(Vec<Expression>),
    /// ```garden
    /// x + y;
    /// x < y;
    /// x && y;
    /// ```
    BinaryOperator(Box<Expression>, BinaryOperatorKind, Box<Expression>),
    /// ```garden
    /// x;
    /// ```
    Variable(Symbol),
    /// ```garden
    /// x();
    /// ```
    Call(Box<Expression>, Vec<Expression>),
    /// ```garden
    /// foo.bar(x, y)
    /// ```
    MethodCall(Box<Expression>, Symbol, Vec<Expression>),
    /// ```garden
    /// fun(x, y) { x + y; }
    /// ```
    FunLiteral(FunInfo),
    /// ```garden
    /// { x; y; }
    /// ```
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
pub struct ToplevelExpression(pub Expression);

#[derive(Debug, Clone, PartialEq)]
pub struct FunInfo {
    pub src_string: SourceString,
    pub doc_comment: Option<String>,
    /// The name of the function. This is `None` for closures.
    pub name: Option<Symbol>,
    pub type_params: Vec<TypeSymbol>,
    pub params: Vec<SymbolWithType>,
    pub return_type: Option<TypeHint>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct VariantInfo {
    pub name_sym: Symbol,
    /// Does this variant wrap a value? For example, the Some variant
    /// in Option.
    pub has_payload: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldInfo {
    pub sym: Symbol,
    pub hint: Option<TypeHint>,
    pub doc_comment: Option<String>,
}

impl Display for VariantInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name_sym.name.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumInfo {
    pub src_string: SourceString,
    pub doc_comment: Option<String>,
    pub name_sym: TypeSymbol,
    pub type_params: Vec<TypeSymbol>,
    pub variants: Vec<VariantInfo>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInfo {
    pub src_string: SourceString,
    pub doc_comment: Option<String>,
    pub name_sym: TypeSymbol,
    pub type_params: Vec<TypeSymbol>,
    pub fields: Vec<FieldInfo>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BuiltinMethodKind {
    ListAppend,
    ListGet,
    ListLen,
    StringConcat,
    StringLen,
    StringSubstring,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MethodKind {
    BuiltinMethod(BuiltinMethodKind, Option<FunInfo>),
    UserDefinedMethod(FunInfo),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodInfo {
    /// The type that has this method.
    pub receiver_type: TypeHint,
    /// The name of the receiver in the method definition. This is
    /// typically `self`.
    ///
    /// TODO: this only exists for user-defined methods, so it's
    /// clunky to have it for all methods.
    pub receiver_name: SymbolName,
    /// The name of the method itself, e.g. `len` in
    /// `some_string.len()`.
    pub name_sym: Symbol,
    /// User-defined or built-in.
    pub kind: MethodKind,
}

impl MethodInfo {
    pub fn doc_comment(&self) -> Option<String> {
        match &self.kind {
            MethodKind::BuiltinMethod(_, fun_info) => {
                fun_info.as_ref().and_then(|fi| fi.doc_comment.clone())
            }
            MethodKind::UserDefinedMethod(fun_info) => fun_info.doc_comment.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition_ {
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
    Struct(StructInfo),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Definition(pub SourceString, pub Position, pub Definition_);

#[derive(Debug, Clone, PartialEq)]
pub enum ToplevelItem {
    Def(Definition),
    Expr(ToplevelExpression),
}
