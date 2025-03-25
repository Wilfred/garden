//! Syntax tree definitions for Garden.

use rustc_hash::FxHashMap;

use std::{fmt::Display, path::PathBuf, rc::Rc};

use crate::position::Position;

/// An owned string of the source text associated with a definition.
#[derive(Clone, PartialEq, Eq)]
pub struct SourceString {
    /// The offset of this string into the defining file, at the time
    /// of evaluation.
    pub offset: usize,
    /// The string containing this definition. This is a substring of
    /// the defining file.
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

impl TypeName {
    pub fn is_no_value(&self) -> bool {
        self.name == "NoValue"
    }
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

impl From<&String> for TypeName {
    fn from(s: &String) -> Self {
        TypeName { name: s.to_owned() }
    }
}

#[derive(Clone, Eq)]
pub struct TypeSymbol {
    pub name: TypeName,
    pub position: Position,
    pub id: SyntaxId,
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
                .field("id", &self.id)
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

impl TypeSymbol {
    pub fn is_placeholder(&self) -> bool {
        // TODO: Prevent users from writing this symbol in userland code.
        self.name.name == "__placeholder" || self.name.name == "__reserved_word_placeholder"
    }
}

/// Represents a type name in source code. This might be a concrete
/// type, such as `List<Int>`, or may refer to generics
/// e.g. `List<T>`.
#[derive(Clone, Debug, PartialEq, Eq)]
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
        } else if self.sym.name.name == "Tuple" {
            let formatted_args = self
                .args
                .iter()
                .map(|a| a.as_src())
                .collect::<Vec<_>>()
                .join(", ");

            format!("({})", formatted_args)
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
pub struct SymbolName {
    pub name: String,
}

impl Display for SymbolName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl SymbolName {
    pub fn is_underscore(&self) -> bool {
        self.name == "_"
    }

    pub fn is_placeholder(&self) -> bool {
        // TODO: Prevent users from writing this symbol in userland code.
        self.name == "__placeholder" || self.name == "__reserved_word_placeholder"
    }
}

impl From<&str> for SymbolName {
    fn from(s: &str) -> Self {
        SymbolName { name: s.to_owned() }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct InternedSymbolId(pub usize);

/// A symbol representing a value, such as a local variable, a
/// function name or a method name.
///
/// See also [`TypeSymbol`].
#[derive(Clone, PartialEq, Eq)]
pub struct Symbol {
    pub position: Position,
    pub name: SymbolName,
    pub id: SyntaxId,
    pub interned_id: InternedSymbolId,
}

impl Symbol {
    pub fn new<S: AsRef<str>>(position: Position, name: S, id_gen: &mut IdGenerator) -> Self {
        let name = SymbolName {
            name: name.as_ref().to_owned(),
        };

        Self {
            interned_id: id_gen.intern_symbol(&name),
            position,
            name,
            id: id_gen.next(),
        }
    }

    pub fn is_placeholder(&self) -> bool {
        self.name.is_placeholder()
    }
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if std::env::var("VERBOSE").is_ok() {
            f.debug_struct("Symbol")
                .field("name", &self.name)
                .field("position", &self.position)
                .field("id", &self.id)
                .field("interned_id", &self.interned_id)
                .finish()
        } else {
            write!(f, "Symbol\"{}\"", self.name.name)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymbolWithHint {
    pub symbol: Symbol,
    pub hint: Option<TypeHint>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperatorKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Exponent,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
    StringConcat,
}

impl Display for BinaryOperatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BinaryOperatorKind::Add => "+",
            BinaryOperatorKind::Subtract => "-",
            BinaryOperatorKind::Multiply => "*",
            BinaryOperatorKind::Divide => "/",
            BinaryOperatorKind::Modulo => "%",
            BinaryOperatorKind::Exponent => "^",
            BinaryOperatorKind::Equal => "==",
            BinaryOperatorKind::NotEqual => "!=",
            BinaryOperatorKind::LessThan => "<",
            BinaryOperatorKind::LessThanOrEqual => "<=",
            BinaryOperatorKind::GreaterThan => ">",
            BinaryOperatorKind::GreaterThanOrEqual => ">=",
            BinaryOperatorKind::And => "&&",
            BinaryOperatorKind::Or => "||",
            BinaryOperatorKind::StringConcat => "^",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignUpdateKind {
    Add,
    Subtract,
}

impl AssignUpdateKind {
    pub fn as_src(&self) -> &'static str {
        match self {
            AssignUpdateKind::Add => "+=",
            AssignUpdateKind::Subtract => "-=",
        }
    }
}

/// The left hand side of a case in a `match`
/// expression. E.g. `Foo(bar)` in the following code.
///
/// ```garden
/// match x {
///   Foo(bar) => {}
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pattern {
    /// E.g. `Some` or `None`.
    pub variant_sym: Symbol,
    /// E.g. `foo` in `Some(foo) => `.
    pub payload: Option<LetDestination>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionWithComma {
    pub expr: Rc<Expression>,
    pub comma: Option<Position>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParenthesizedExpression {
    pub open_paren: Position,
    pub expr: Rc<Expression>,
    pub close_paren: Position,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParenthesizedArguments {
    pub open_paren: Position,
    pub arguments: Vec<ExpressionWithComma>,
    pub close_paren: Position,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LetDestination {
    /// ```garden
    /// let x = y
    /// ```
    Symbol(Symbol),
    /// ```garden
    /// let (x, y) = z
    /// ```
    Destructure(Vec<Symbol>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression_ {
    /// ```garden
    /// match x {
    ///     None => { get_value() }
    ///     Some(y) => y + 1,
    ///     _ => error("yikes"),
    /// }
    /// ```
    Match(Rc<Expression>, Vec<(Pattern, Block)>),
    /// ```garden
    /// if x { y }
    /// if x { y } else { z }
    /// ```
    If(Rc<Expression>, Block, Option<Block>),
    /// ```garden
    /// while x { y }
    /// ```
    While(Rc<Expression>, Block),
    /// ```garden
    /// for x in y { z }
    /// for (foo, bar) in y { z }
    /// ```
    ForIn(LetDestination, Rc<Expression>, Block),
    /// ```garden
    /// break
    /// ```
    Break,
    /// ```garden
    /// continue
    /// ```
    Continue,
    /// ```garden
    /// x = y
    /// ```
    Assign(Symbol, Rc<Expression>),
    /// ```garden
    /// x += y
    /// x -= y
    /// ```
    AssignUpdate(Symbol, AssignUpdateKind, Rc<Expression>),
    /// ```garden
    /// let x = y
    /// let x: T = y
    /// let (x, y) = z
    /// ```
    Let(LetDestination, Option<TypeHint>, Rc<Expression>),
    /// ```garden
    /// return x
    /// return // equivalent to `return Unit`
    /// ```
    Return(Option<Rc<Expression>>),
    /// ```garden
    /// 123
    /// ```
    IntLiteral(i64),
    /// ```garden
    /// "foo"
    /// ```
    StringLiteral(String),
    /// ```garden
    /// [x, y]
    /// ```
    ListLiteral(Vec<ExpressionWithComma>),
    /// ```garden
    /// ()
    /// (x,)
    /// (x, y)
    /// ```
    TupleLiteral(Vec<Rc<Expression>>),
    /// ```garden
    /// Foo { x: 1, y: bar() }
    /// ```
    ///
    /// Field values are executed in the order they occur in source
    /// code, so we want an ordered data type here.
    StructLiteral(TypeSymbol, Vec<(Symbol, Rc<Expression>)>),
    /// ```garden
    /// x + y
    /// x < y
    /// x && y
    /// ```
    BinaryOperator(Rc<Expression>, BinaryOperatorKind, Rc<Expression>),
    /// ```garden
    /// x
    /// ```
    Variable(Symbol),
    /// ```garden
    /// x()
    /// ```
    Call(Rc<Expression>, ParenthesizedArguments),
    /// ```garden
    /// foo.bar(x, y)
    /// ```
    MethodCall(Rc<Expression>, Symbol, ParenthesizedArguments),
    /// ```garden
    /// foo.bar
    /// ```
    DotAccess(Rc<Expression>, Symbol),
    /// ```garden
    /// fun(x, y) { x + y }
    /// ```
    FunLiteral(FunInfo),
    /// ```garden
    /// assert(x)
    /// ```
    Assert(Rc<Expression>),
    /// Parentheses used for grouping, particularly in nested binary operators.
    ///
    /// ```garden
    /// (x)
    /// x * (y * z)
    /// ```
    Parentheses(Position, Rc<Expression>, Position),
    /// We had a parse error in this position, so there's no
    /// expression.
    Invalid,
}

impl Expression_ {
    pub(crate) fn is_invalid_or_placeholder(&self) -> bool {
        match self {
            Expression_::Variable(sym) => sym.is_placeholder(),
            Expression_::Invalid => true,
            _ => false,
        }
    }
}

/// A syntactic item that the IDE can interact with, such as an
/// expression or a variable name.
#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct SyntaxId(pub usize);

impl std::fmt::Debug for SyntaxId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Avoid deriving Debug because otherwise we get:
        //
        // SyntaxId(
        //   123
        // )
        //
        // which is too verbose.
        write!(f, "SyntaxId({})", self.0)
    }
}

/// An ID that represents a piece of syntax that we can perform
/// operations on, such as go-to-def.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstId {
    /// Syntax ID of an expression.
    Expr(SyntaxId),
    /// Syntax ID of a symbol. This is not necessarily part of an
    /// expression: it could be a function parameter or type name.
    ///
    /// ```
    /// fun f(foo: Int) {}
    /// ```
    Sym(SyntaxId),
    /// Syntax ID of a type symbol.
    ///
    /// ```
    /// fun f(_: Foo) {}
    /// ```
    TypeSym(SyntaxId),
    /// An import item.
    ///
    /// ```
    /// import "./foo.gdn"
    /// ```
    Import(SyntaxId),
}

impl AstId {
    pub fn id(&self) -> SyntaxId {
        match self {
            AstId::Expr(syntax_id) => *syntax_id,
            AstId::Sym(syntax_id) => *syntax_id,
            AstId::TypeSym(syntax_id) => *syntax_id,
            AstId::Import(syntax_id) => *syntax_id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IdGenerator {
    pub next_id: SyntaxId,
    pub interned: FxHashMap<SymbolName, InternedSymbolId>,
    pub intern_id_to_name: FxHashMap<InternedSymbolId, SymbolName>,
}

impl Default for IdGenerator {
    fn default() -> Self {
        Self {
            next_id: SyntaxId(0),
            interned: FxHashMap::default(),
            intern_id_to_name: FxHashMap::default(),
        }
    }
}

impl IdGenerator {
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> SyntaxId {
        let next_id = self.next_id;
        self.next_id = SyntaxId(next_id.0 + 1);
        next_id
    }

    pub fn intern_symbol(&mut self, name: &SymbolName) -> InternedSymbolId {
        match self.interned.get(name) {
            Some(id) => *id,
            None => {
                let id = InternedSymbolId(self.interned.len());
                self.interned.insert(name.clone(), id);
                self.intern_id_to_name.insert(id, name.clone());
                id
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
    pub position: Position,
    pub expr_: Expression_,
    /// Is this expression in a position where its value is used?
    /// This is only false in blocks, e.g. in `{ foo() bar() }`,
    /// `foo()` is ignored.
    pub value_is_used: bool,
    pub id: SyntaxId,
}

impl Expression {
    pub(crate) fn new(position: Position, expr_: Expression_, id: SyntaxId) -> Self {
        Self {
            position,
            expr_,
            value_is_used: true,
            id,
        }
    }

    /// Helper for creating Invalid expressions.
    pub fn invalid(position: Position, id: SyntaxId) -> Self {
        Self {
            position,
            expr_: Expression_::Invalid,
            value_is_used: true,
            id,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub open_brace: Position,
    pub exprs: Vec<Rc<Expression>>,
    pub close_brace: Position,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Visibility {
    External(Position),
    CurrentFile,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToplevelExpression(pub Expression);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunInfo {
    pub src_string: SourceString,
    pub doc_comment: Option<String>,
    /// The name of the function. This is `None` for closures.
    pub name_sym: Option<Symbol>,
    /// If this is a toplevel function, the ID of the definition.
    pub item_id: Option<ToplevelItemId>,
    pub type_params: Vec<TypeSymbol>,
    pub params: ParenthesizedParameters,
    pub return_hint: Option<TypeHint>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParenthesizedParameters {
    pub open_paren: Position,
    pub params: Vec<SymbolWithHint>,
    pub close_paren: Position,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestInfo {
    pub src_string: SourceString,
    pub doc_comment: Option<String>,
    pub name_sym: Symbol,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantInfo {
    pub name_sym: Symbol,
    /// If this variant is of the form `Foo(T)`, the type hint inside
    /// the parentheses.
    pub payload_hint: Option<TypeHint>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldInfo {
    pub sym: Symbol,
    pub hint: TypeHint,
    pub doc_comment: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumInfo {
    pub visibility: Visibility,
    pub src_string: SourceString,
    pub doc_comment: Option<String>,
    pub name_sym: TypeSymbol,
    pub type_params: Vec<TypeSymbol>,
    pub variants: Vec<VariantInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructInfo {
    pub visibility: Visibility,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportInfo {
    pub path_pos: Position,
    pub path: PathBuf,
    pub id: SyntaxId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinMethodKind {
    ListAppend,
    ListContains,
    ListGet,
    ListLen,
    PathExists,
    PathRead,
    StringIndexOf,
    StringLen,
    StringLines,
    StringSubstring,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MethodKind {
    BuiltinMethod(BuiltinMethodKind, Option<FunInfo>),
    UserDefinedMethod(FunInfo),
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct ToplevelItemId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ToplevelItem_ {
    /// ```garden
    /// fun foo() {}
    /// external fun bar() {}
    /// ```
    Fun(Symbol, FunInfo, Visibility),
    /// ```garden
    /// fun (this: MyType) foo() {}
    /// external fun (this: MyType) baz() {}
    /// ```
    Method(MethodInfo, Visibility),
    /// ```garden
    /// test foo { assert(1 == 2) }
    /// ```
    Test(TestInfo),
    /// ```garden
    /// enum Foo { Red, Green, Custom(String) }
    /// ```
    Enum(EnumInfo),
    /// ```garden
    /// struct Foo { x: Int, y: Int }
    /// ```
    Struct(StructInfo),
    /// ```garden
    /// import "./foo.gdn"
    /// ```
    Import(ImportInfo),
    /// ```garden
    /// println("hello world")
    /// ```
    ///
    /// Should only be used in the REPL.
    Expr(ToplevelExpression),
    /// ```garden
    /// { println("hello world") }
    /// ```
    Block(Block),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToplevelItem(pub SourceString, pub Position, pub ToplevelItem_);

impl ToplevelItem_ {
    pub(crate) fn is_invalid_or_placeholder(&self) -> bool {
        match self {
            ToplevelItem_::Fun(symbol, _, _) => symbol.is_placeholder(),
            ToplevelItem_::Method(method_info, _) => method_info.name_sym.is_placeholder(),
            ToplevelItem_::Test(test_info) => test_info.name_sym.is_placeholder(),
            ToplevelItem_::Enum(enum_info) => enum_info.name_sym.is_placeholder(),
            ToplevelItem_::Struct(struct_info) => struct_info.name_sym.is_placeholder(),
            ToplevelItem_::Expr(e) => e.0.expr_.is_invalid_or_placeholder(),
            ToplevelItem_::Block(_) => false,
            ToplevelItem_::Import(_) => false,
        }
    }
}
