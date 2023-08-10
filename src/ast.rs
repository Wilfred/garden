//! Syntax tree definitions for Garden.

use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::eval::ErrorKind;

/// A position is an offset into source code.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Position {
    pub start_offset: usize,
    pub end_offset: usize,
    // TODO: consider storing a &Path to reduce memory usage.
    pub path: PathBuf,
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
    Lambda(Vec<Variable>, Block),
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
    pub doc_comment: Option<String>,
    pub name: Variable,
    pub params: Vec<Variable>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition_ {
    Fun(FunInfo),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Definition(pub String, pub Position, pub Definition_);

#[derive(Debug, Clone, PartialEq)]
pub enum DefinitionsOrExpression {
    Defs(Vec<Definition>),
    Expr(ToplevelExpression),
}
