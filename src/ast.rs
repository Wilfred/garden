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

/// An owned string of the source text associated with a definition.
#[derive(Clone, Debug, PartialEq)]
pub struct SourceString {
    /// The offset of this string into the defining file, at the time
    /// of evaluation.
    pub offset: usize,
    /// The string containing this definition.
    pub src: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct VariableName(pub String);

#[derive(Clone, Debug, PartialEq)]
pub struct Variable(pub Position, pub VariableName);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperatorKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression_ {
    If(Box<Expression>, Block, Option<Block>),
    While(Box<Expression>, Block),
    Assign(Variable, Box<Expression>),
    Let(Variable, Box<Expression>),
    Return(Box<Expression>),
    IntLiteral(i64),
    StringLiteral(String),
    BoolLiteral(bool),
    ListLiteral(Vec<Expression>),
    BinaryOperator(Box<Expression>, BinaryOperatorKind, Box<Expression>),
    Variable(Variable),
    Call(Box<Expression>, Vec<Expression>),
    MethodCall(Box<Expression>, Variable, Vec<Expression>),
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
    pub name: Option<Variable>,
    pub params: Vec<Variable>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition_ {
    Fun(Variable, FunInfo),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Definition(pub SourceString, pub Position, pub Definition_);

#[derive(Debug, Clone, PartialEq)]
pub enum DefinitionsOrExpression {
    Defs(Vec<Definition>),
    Expr(ToplevelExpression),
}
