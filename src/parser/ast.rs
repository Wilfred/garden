//! Syntax tree definitions for Garden.

use rustc_hash::FxHashMap;

use std::fmt::Display;
use std::path::PathBuf;
use std::rc::Rc;

use crate::parser::position::Position;

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct TypeName {
    pub(crate) text: String,
}

impl TypeName {
    pub(crate) fn is_no_value(&self) -> bool {
        self.text == "NoValue"
    }
}

impl std::fmt::Debug for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text)
    }
}
impl Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text)
    }
}

impl From<&str> for TypeName {
    fn from(s: &str) -> Self {
        TypeName { text: s.to_owned() }
    }
}

impl From<&String> for TypeName {
    fn from(s: &String) -> Self {
        TypeName { text: s.to_owned() }
    }
}

#[derive(Clone, Eq)]
pub(crate) struct TypeSymbol {
    pub(crate) name: TypeName,
    pub(crate) position: Position,
    pub(crate) id: SyntaxId,
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
            write!(f, "TypeSymbol\"{}\"", self.name.text)
        }
    }
}

impl Display for TypeSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl TypeSymbol {
    pub(crate) fn is_placeholder(&self) -> bool {
        // TODO: Prevent users from writing this symbol in userland code.
        self.name.text == "__placeholder" || self.name.text == "__keyword_placeholder"
    }
}

/// Represents a type name in source code. This might be a concrete
/// type, such as `List<Int>`, or may refer to generics
/// e.g. `List<T>`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct TypeHint {
    pub(crate) sym: TypeSymbol,
    pub(crate) args: Vec<TypeHint>,
    pub(crate) position: Position,
}

impl TypeHint {
    /// The source code equivalent of this type hint.
    pub(crate) fn as_src(&self) -> String {
        if self.args.is_empty() {
            format!("{}", self.sym.name)
        } else if self.sym.name.text == "Tuple" {
            let formatted_args = self
                .args
                .iter()
                .map(|a| a.as_src())
                .collect::<Vec<_>>()
                .join(", ");

            format!("({formatted_args})")
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
pub(crate) struct SymbolName {
    pub(crate) text: String,
}

impl Display for SymbolName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text)
    }
}

impl SymbolName {
    pub(crate) fn is_underscore(&self) -> bool {
        self.text == "_"
    }

    pub(crate) fn is_placeholder(&self) -> bool {
        // TODO: Prevent users from writing these symbols in userland code.
        self.text == "__placeholder" || self.text == "__keyword_placeholder"
    }
}

impl From<&str> for SymbolName {
    fn from(s: &str) -> Self {
        SymbolName { text: s.to_owned() }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub(crate) struct InternedSymbolId(pub(crate) usize);

/// A symbol representing a value, such as a local variable, a
/// function name or a method name.
///
/// See also [`TypeSymbol`].
#[derive(Clone, PartialEq, Eq)]
pub(crate) struct Symbol {
    pub(crate) position: Position,
    pub(crate) name: SymbolName,
    pub(crate) id: SyntaxId,
    pub(crate) interned_id: InternedSymbolId,
}

impl Symbol {
    pub(crate) fn new<S: AsRef<str>>(
        position: Position,
        name: S,
        id_gen: &mut IdGenerator,
    ) -> Self {
        let name = SymbolName {
            text: name.as_ref().to_owned(),
        };

        Self {
            interned_id: id_gen.intern_symbol(&name),
            position,
            name,
            id: id_gen.next(),
        }
    }

    pub(crate) fn is_placeholder(&self) -> bool {
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
            write!(f, "Symbol\"{}\"", self.name.text)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct SymbolWithHint {
    pub(crate) symbol: Symbol,
    pub(crate) hint: Option<TypeHint>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BinaryOperatorKind {
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
        write!(f, "{s}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AssignUpdateKind {
    Add,
    Subtract,
}

impl AssignUpdateKind {
    pub(crate) fn as_src(&self) -> &'static str {
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
pub(crate) struct Pattern {
    /// E.g. `Some` or `None`.
    pub(crate) variant_sym: Symbol,
    /// E.g. `foo` in `Some(foo) => `.
    pub(crate) payload: Option<LetDestination>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExpressionWithComma {
    pub(crate) expr: Rc<Expression>,
    pub(crate) comma: Option<Position>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ParenthesizedArguments {
    pub(crate) open_paren: Position,
    pub(crate) arguments: Vec<ExpressionWithComma>,
    pub(crate) close_paren: Position,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum LetDestination {
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
pub(crate) enum Expression_ {
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
    /// foo::bar
    /// ```
    NamespaceAccess(Rc<Expression>, Symbol),
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
pub(crate) struct SyntaxId(pub(crate) usize);

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
pub(crate) enum AstId {
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
    pub(crate) fn id(&self) -> SyntaxId {
        match self {
            AstId::Expr(syntax_id) => *syntax_id,
            AstId::Sym(syntax_id) => *syntax_id,
            AstId::TypeSym(syntax_id) => *syntax_id,
            AstId::Import(syntax_id) => *syntax_id,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct IdGenerator {
    pub(crate) next_id: SyntaxId,
    pub(crate) interned: FxHashMap<SymbolName, InternedSymbolId>,
    pub(crate) intern_id_to_name: FxHashMap<InternedSymbolId, SymbolName>,
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
    pub(crate) fn next(&mut self) -> SyntaxId {
        let next_id = self.next_id;
        self.next_id = SyntaxId(next_id.0 + 1);
        next_id
    }

    pub(crate) fn intern_symbol(&mut self, name: &SymbolName) -> InternedSymbolId {
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
pub(crate) struct Expression {
    pub(crate) position: Position,
    pub(crate) expr_: Expression_,
    /// Is this expression in a position where its value is used?
    /// This is only false in blocks, e.g. in `{ foo() bar() }`,
    /// `foo()` is ignored.
    pub(crate) value_is_used: bool,
    pub(crate) id: SyntaxId,
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
    pub(crate) fn invalid(position: Position, id: SyntaxId) -> Self {
        Self {
            position,
            expr_: Expression_::Invalid,
            value_is_used: true,
            id,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Block {
    pub(crate) open_brace: Position,
    pub(crate) exprs: Vec<Rc<Expression>>,
    pub(crate) close_brace: Position,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Visibility {
    External(Position),
    CurrentFile,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ToplevelExpression(pub(crate) Expression);

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FunInfo {
    pub(crate) pos: Position,
    pub(crate) doc_comment: Option<String>,
    /// The name of the function. This is `None` for closures.
    pub(crate) name_sym: Option<Symbol>,
    /// If this is a toplevel function, the ID of the definition.
    pub(crate) item_id: Option<ToplevelItemId>,
    pub(crate) type_params: Vec<TypeSymbol>,
    pub(crate) params: ParenthesizedParameters,
    pub(crate) return_hint: Option<TypeHint>,
    pub(crate) body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ParenthesizedParameters {
    pub(crate) open_paren: Position,
    pub(crate) params: Vec<SymbolWithHint>,
    pub(crate) close_paren: Position,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TestInfo {
    pub(crate) pos: Position,
    pub(crate) doc_comment: Option<String>,
    pub(crate) name_sym: Symbol,
    pub(crate) body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct VariantInfo {
    pub(crate) name_sym: Symbol,
    /// If this variant is of the form `Foo(T)`, the type hint inside
    /// the parentheses.
    pub(crate) payload_hint: Option<TypeHint>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FieldInfo {
    pub(crate) sym: Symbol,
    pub(crate) hint: TypeHint,
    pub(crate) doc_comment: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct EnumInfo {
    pub(crate) pos: Position,
    pub(crate) visibility: Visibility,
    pub(crate) doc_comment: Option<String>,
    pub(crate) name_sym: TypeSymbol,
    pub(crate) type_params: Vec<TypeSymbol>,
    pub(crate) variants: Vec<VariantInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StructInfo {
    pub(crate) pos: Position,
    pub(crate) visibility: Visibility,
    pub(crate) doc_comment: Option<String>,
    pub(crate) name_sym: TypeSymbol,
    pub(crate) type_params: Vec<TypeSymbol>,
    /// The fields of this struct.
    ///
    /// We deliberately want an ordered data type here, so we can
    /// display field information in the same order as the user
    /// defined the fields.
    pub(crate) fields: Vec<FieldInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ImportInfo {
    pub(crate) pos: Position,
    /// The actual path being imported. For example, if the user
    /// wrote `import "./foo.gdn"`, then ./foo.gdn is the path here.
    pub(crate) path: PathBuf,
    /// The position of the string literal within
    /// `import "./foo.gdn"`.
    pub(crate) path_pos: Position,
    /// The `bar` in `import "./foo.gdn" as bar`.
    pub(crate) namespace_sym: Option<Symbol>,

    pub(crate) id: SyntaxId,
}

/// All the methods implemented as primitives rather than Garden code.
///
/// See also `crate::values::BuiltinFunctionKind`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BuiltinMethodKind {
    ListAppend,
    ListContains,
    ListGet,
    ListLen,
    PathExists,
    PathRead,
    StringAsInt,
    StringChars,
    StringIndexOf,
    StringJoin,
    StringLen,
    StringLines,
    StringStartsWith,
    StringEndsWith,
    StringSubstring,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum MethodKind {
    BuiltinMethod(BuiltinMethodKind, Option<FunInfo>),
    UserDefinedMethod(FunInfo),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MethodInfo {
    pub(crate) pos: Position,
    /// The type that has this method.
    pub(crate) receiver_hint: TypeHint,
    /// The name of the receiver in the method definition. This is
    /// typically `this`.
    ///
    /// TODO: this only exists for user-defined methods, so it's
    /// clunky to have it for all methods.
    pub(crate) receiver_sym: Symbol,
    /// The name of the method itself, e.g. `len` in
    /// `method len(this: String): Int {}`.
    pub(crate) name_sym: Symbol,
    /// User-defined or built-in.
    pub(crate) kind: MethodKind,
}

impl MethodInfo {
    pub(crate) fn fun_info(&self) -> Option<&FunInfo> {
        match &self.kind {
            MethodKind::BuiltinMethod(_, fun_info) => fun_info.as_ref(),
            MethodKind::UserDefinedMethod(fun_info) => Some(fun_info),
        }
    }

    pub(crate) fn full_name(&self) -> String {
        format!("{}::{}", self.receiver_hint.sym, self.name_sym.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub(crate) struct ToplevelItemId(pub(crate) usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ToplevelItem {
    /// ```garden
    /// fun foo() {}
    /// external fun bar() {}
    /// ```
    Fun(Symbol, FunInfo, Visibility),
    /// ```garden
    /// method foo(this: MyType) {}
    /// external method bar(this: MyType) {}
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
    /// import "./foo.gdn" as bar
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

impl ToplevelItem {
    pub(crate) fn position(&self) -> Position {
        match self {
            ToplevelItem::Fun(_, fun_info, _) => fun_info.pos.clone(),
            ToplevelItem::Method(method_info, _) => method_info.pos.clone(),
            ToplevelItem::Test(test_info) => test_info.pos.clone(),
            ToplevelItem::Enum(enum_info) => enum_info.pos.clone(),
            ToplevelItem::Struct(struct_info) => struct_info.pos.clone(),
            ToplevelItem::Import(import_info) => import_info.pos.clone(),
            ToplevelItem::Expr(toplevel_expression) => toplevel_expression.0.position.clone(),
            ToplevelItem::Block(block) => Position::merge(&block.open_brace, &block.close_brace),
        }
    }

    pub(crate) fn is_invalid_or_placeholder(&self) -> bool {
        match self {
            ToplevelItem::Fun(symbol, _, _) => symbol.is_placeholder(),
            ToplevelItem::Method(method_info, _) => method_info.name_sym.is_placeholder(),
            ToplevelItem::Test(test_info) => test_info.name_sym.is_placeholder(),
            ToplevelItem::Enum(enum_info) => enum_info.name_sym.is_placeholder(),
            ToplevelItem::Struct(struct_info) => struct_info.name_sym.is_placeholder(),
            ToplevelItem::Expr(e) => e.0.expr_.is_invalid_or_placeholder(),
            ToplevelItem::Block(_) => false,
            ToplevelItem::Import(_) => false,
        }
    }
}
