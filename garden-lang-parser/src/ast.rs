//! Syntax tree definitions for Garden.

use std::{cell::OnceCell, fmt::Display};

use crate::position::Position;

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

impl From<&str> for TypeName {
    fn from(s: &str) -> Self {
        TypeName { name: s.to_owned() }
    }
}

#[derive(Clone)]
pub struct TypeSymbol {
    pub name: TypeName,
    pub position: Position,
    pub id: OnceCell<SyntaxId>,
}

/// Only consider the name when comparing type symbols. This is
/// important when the runtime checks values have the same type.
impl PartialEq for TypeSymbol {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
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

/// Represents a type name in source code. This might be a concrete
/// type, such as `List<Int>`, or may refer to generics
/// e.g. `List<T>`.
#[derive(Clone, Debug, PartialEq)]
pub struct TypeHint {
    pub sym: TypeSymbol,
    pub args: Vec<TypeHint>,
    pub position: Position,
}

impl TypeHint {
    /// The source code equivalent of this type hint.
    pub fn as_src(&self) -> String {
        if self.args.is_empty() {
            format!("{}", self.sym.name)
        } else {
            let formatted_args = self
                .args
                .iter()
                .map(|a| a.as_src())
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}<{}>", self.sym.name, formatted_args)
        }
    }
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

impl From<&str> for SymbolName {
    fn from(s: &str) -> Self {
        SymbolName(s.to_owned())
    }
}

/// A symbol representing a value, such as a local variable, a
/// function name or a method name.
///
/// See also [`TypeSymbol`].
#[derive(Clone, PartialEq)]
pub struct Symbol {
    pub position: Position,
    pub name: SymbolName,
    pub id: OnceCell<SyntaxId>,
}

impl Symbol {
    pub fn new<S: AsRef<str>>(position: Position, name: S) -> Self {
        Self {
            position,
            name: SymbolName(name.as_ref().to_owned()),
            id: OnceCell::new(),
        }
    }
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
pub struct SymbolWithHint {
    pub symbol: Symbol,
    pub hint: Option<TypeHint>,
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
pub struct ParenthesizedArguments {
    pub open_paren: Position,
    pub arguments: Vec<Expression>,
    pub close_paren: Position,
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
    /// break;
    /// ```
    Break,
    /// ```garden
    /// x = y;
    /// ```
    Assign(Symbol, Box<Expression>),
    /// ```garden
    /// let x = y;
    /// let x: T = y;
    /// ```
    Let(Symbol, Option<TypeHint>, Box<Expression>),
    /// ```garden
    /// return x;
    /// return;
    /// ```
    Return(Option<Box<Expression>>),
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
    /// Foo { x: 1, y: bar() };
    /// ```
    ///
    /// Field values are executed in the order they occur in source
    /// code, so we want an ordered data type here.
    StructLiteral(TypeSymbol, Vec<(Symbol, Expression)>),
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
    Call(Box<Expression>, ParenthesizedArguments),
    /// ```garden
    /// foo.bar(x, y)
    /// ```
    MethodCall(Box<Expression>, Symbol, ParenthesizedArguments),
    /// ```garden
    /// foo.bar
    /// ```
    DotAccess(Box<Expression>, Symbol),
    /// ```garden
    /// fun(x, y) { x + y; }
    /// ```
    FunLiteral(FunInfo),
    /// ```garden
    /// { x; y; }
    /// ```
    Block(Block),
    /// We had a parse error in this position, so there's no
    /// expression.
    Invalid,
}

/// A syntactic item that the IDE can interact with, such as an
/// expression or a variable name.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct SyntaxId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub pos: Position,
    pub expr_: Expression_,
    pub id: OnceCell<SyntaxId>,
}

impl Expression {
    pub fn new(position: Position, expr_: Expression_) -> Self {
        Self {
            pos: position,
            expr_,
            id: OnceCell::new(),
        }
    }

    pub fn new_with_id(position: Position, expr_: Expression_, id: &OnceCell<SyntaxId>) -> Self {
        Self {
            pos: position,
            expr_,
            id: id.clone(),
        }
    }

    /// Helper for creating Invalid expressions.
    pub fn invalid() -> Self {
        Self {
            pos: Position::todo(),
            expr_: Expression_::Invalid,
            id: OnceCell::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub is_loop_body: bool,
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
    pub params: Vec<SymbolWithHint>,
    pub return_hint: Option<TypeHint>,
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
    /// If this variant is of the form `Foo(T)`, the type hint inside
    /// the parentheses.
    pub payload_hint: Option<TypeHint>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldInfo {
    pub sym: Symbol,
    pub hint: TypeHint,
    pub doc_comment: Option<String>,
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
    /// The fields of this struct.
    ///
    /// We deliberately want an ordered data type here, so we can
    /// display field information in the same order as the user
    /// defined the fields.
    pub fields: Vec<FieldInfo>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BuiltinMethodKind {
    ListAppend,
    ListGet,
    ListLen,
    StringAppend,
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
    pub receiver_hint: TypeHint,
    /// The name of the receiver in the method definition. This is
    /// typically `self`.
    ///
    /// TODO: this only exists for user-defined methods, so it's
    /// clunky to have it for all methods.
    pub receiver_sym: Symbol,
    /// The name of the method itself, e.g. `len` in
    /// `some_string.len()`.
    pub name_sym: Symbol,
    /// User-defined or built-in.
    pub kind: MethodKind,
}

impl MethodInfo {
    pub fn fun_info(&self) -> Option<&FunInfo> {
        match &self.kind {
            MethodKind::BuiltinMethod(_, fun_info) => fun_info.as_ref(),
            MethodKind::UserDefinedMethod(fun_info) => Some(fun_info),
        }
    }

    pub fn full_name(&self) -> String {
        format!("{}::{}", self.receiver_hint.sym, self.name_sym.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition_ {
    /// ```garden
    /// fun foo() {}
    /// ```
    Fun(Symbol, FunInfo),
    /// ```garden
    /// fun (self: MyType) foo() {}
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
