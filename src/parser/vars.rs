//! Resolving local variables to the lexical binding they refer to.
//!
//! After parsing, [`assign_var_ids`] walks the AST and gives every
//! local variable binding a unique [`LocalVariableId`]. Each reference
//! is resolved to the ID of the binding in scope, so two variables of
//! the same name in different scopes are distinguished, and every
//! reference to one binding shares its ID.
//!
//! Variables bound at the toplevel are deliberately left unresolved:
//! they behave as globals looked up by name. This keeps the REPL
//! correct, because a later snippet is parsed separately and can still
//! refer to a toplevel variable defined in an earlier one.

use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::parser::ast::{
    Block, Expression, Expression_, FunInfo, IdGenerator, LetDestination, LocalVariableId,
    MethodInfo, MethodKind, Symbol, SymbolName, ToplevelItem,
};
use crate::parser::visitor::Visitor;

/// Assign a [`LocalVariableId`] to every local variable binding and
/// reference in `items`.
pub(crate) fn assign_var_ids(items: &mut [ToplevelItem], id_gen: &mut IdGenerator) {
    let mut resolver = Resolver {
        id_gen,
        scopes: vec![],
    };
    for item in items {
        resolver.visit_toplevel_item(item);
    }
}

struct Resolver<'a> {
    id_gen: &'a mut IdGenerator,
    /// The lexical scopes enclosing the code we're currently visiting.
    /// This is empty at the toplevel, whose variables are treated as
    /// globals and looked up by name.
    scopes: Vec<FxHashMap<SymbolName, LocalVariableId>>,
}

impl Resolver<'_> {
    fn push_scope(&mut self) {
        self.scopes.push(FxHashMap::default());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    /// Record `symbol` as a fresh binding in the current scope.
    ///
    /// Toplevel bindings (with no enclosing scope) are left unresolved,
    /// so they're looked up by name and remain visible to later REPL
    /// snippets.
    fn bind(&mut self, symbol: &mut Symbol) {
        if symbol.name.is_underscore() || self.scopes.is_empty() {
            return;
        }

        let var_id = self.id_gen.next_var_id();
        self.scopes
            .last_mut()
            .expect("scopes is non-empty")
            .insert(symbol.name.clone(), var_id);
        symbol.var_id = Some(var_id);
    }

    /// Resolve `symbol`, a reference, to the binding currently in scope.
    fn resolve(&mut self, symbol: &mut Symbol) {
        for scope in self.scopes.iter().rev() {
            if let Some(var_id) = scope.get(&symbol.name) {
                symbol.var_id = Some(*var_id);
                return;
            }
        }
    }

    fn bind_dest(&mut self, dest: &mut LetDestination) {
        match dest {
            LetDestination::Symbol(symbol) => self.bind(symbol),
            LetDestination::Destructure(symbols) => {
                for symbol in symbols {
                    self.bind(symbol);
                }
            }
        }
    }

    fn visit_toplevel_item(&mut self, item: &mut ToplevelItem) {
        match item {
            ToplevelItem::Fun(_, fun_info, _) => self.visit_fun_info(None, fun_info),
            ToplevelItem::Method(method_info, _) => self.visit_method_info(method_info),
            ToplevelItem::Test(test_info) => self.visit_block(&mut test_info.body),
            ToplevelItem::Expr(toplevel_expr) => self.visit_expr(&mut toplevel_expr.0),
            ToplevelItem::Block(block) => {
                // A toplevel block's bindings live at the toplevel, so
                // don't open a new scope for them.
                for expr in &mut block.exprs {
                    self.visit_expr(Rc::make_mut(expr));
                }
            }
            ToplevelItem::Enum(_) | ToplevelItem::Struct(_) | ToplevelItem::Import(_) => {}
        }
    }

    fn visit_method_info(&mut self, method_info: &mut MethodInfo) {
        let MethodInfo {
            receiver_sym, kind, ..
        } = method_info;

        match kind {
            MethodKind::UserDefinedMethod(fun_info)
            | MethodKind::BuiltinMethod(_, Some(fun_info)) => {
                self.visit_fun_info(Some(receiver_sym), fun_info);
            }
            MethodKind::BuiltinMethod(_, None) => {}
        }
    }

    /// Resolve a function body, binding `receiver` (the method receiver,
    /// if any) and the parameters in a fresh scope.
    fn visit_fun_info(&mut self, receiver: Option<&mut Symbol>, fun_info: &mut FunInfo) {
        self.push_scope();

        if let Some(receiver) = receiver {
            self.bind(receiver);
        }
        for param in &mut fun_info.params.params {
            self.bind(&mut param.symbol);
        }

        self.visit_block(&mut fun_info.body);
        self.pop_scope();
    }

    fn visit_block(&mut self, block: &mut Block) {
        self.push_scope();
        for expr in &mut block.exprs {
            self.visit_expr(Rc::make_mut(expr));
        }
        self.pop_scope();
    }

    fn visit_expr(&mut self, expr: &mut Expression) {
        match &mut expr.expr_ {
            Expression_::Variable(symbol) => self.resolve(symbol),
            Expression_::Assign(symbol, value) => {
                self.visit_expr(Rc::make_mut(value));
                self.resolve(symbol);
            }
            Expression_::AssignUpdate(symbol, _, value) => {
                self.visit_expr(Rc::make_mut(value));
                self.resolve(symbol);
            }
            Expression_::Let(dest, _, value) => {
                // Visit the value before binding, so `let x = x` reads
                // an outer `x` on the right-hand side.
                self.visit_expr(Rc::make_mut(value));
                self.bind_dest(dest);
            }
            Expression_::ForIn(dest, iteree, body) => {
                self.visit_expr(Rc::make_mut(iteree));
                self.push_scope();
                self.bind_dest(dest);
                self.visit_block(body);
                self.pop_scope();
            }
            Expression_::Match(scrutinee, cases) => {
                self.visit_expr(Rc::make_mut(scrutinee));
                for (pattern, body) in cases {
                    self.push_scope();
                    if let Some(dest) = &mut pattern.payload {
                        self.bind_dest(dest);
                    }
                    self.visit_block(body);
                    self.pop_scope();
                }
            }
            Expression_::Try(try_body, catch_sym, catch_body) => {
                self.visit_block(try_body);
                self.push_scope();
                self.bind(catch_sym);
                self.visit_block(catch_body);
                self.pop_scope();
            }
            Expression_::If(cond, then_body, else_body) => {
                self.visit_expr(Rc::make_mut(cond));
                self.visit_block(then_body);
                if let Some(else_body) = else_body {
                    self.visit_block(else_body);
                }
            }
            Expression_::While(cond, body) => {
                self.visit_expr(Rc::make_mut(cond));
                self.visit_block(body);
            }
            Expression_::Return(value) => {
                if let Some(value) = value {
                    self.visit_expr(Rc::make_mut(value));
                }
            }
            Expression_::Assert(value) => self.visit_expr(Rc::make_mut(value)),
            Expression_::BinaryOperator(lhs, _, rhs) => {
                self.visit_expr(Rc::make_mut(lhs));
                self.visit_expr(Rc::make_mut(rhs));
            }
            Expression_::Call(recv, args) => {
                self.visit_expr(Rc::make_mut(recv));
                for arg in &mut args.arguments {
                    self.visit_expr(Rc::make_mut(&mut arg.expr));
                }
            }
            Expression_::MethodCall(recv, _, args) => {
                self.visit_expr(Rc::make_mut(recv));
                for arg in &mut args.arguments {
                    self.visit_expr(Rc::make_mut(&mut arg.expr));
                }
            }
            Expression_::DotAccess(recv, _) | Expression_::NamespaceAccess(recv, _) => {
                self.visit_expr(Rc::make_mut(recv));
            }
            Expression_::FunLiteral(fun_info) => self.visit_fun_info(None, fun_info),
            Expression_::ListLiteral(items) => {
                for item in items {
                    self.visit_expr(Rc::make_mut(&mut item.expr));
                }
            }
            Expression_::DictLiteral(items) => {
                for item in items {
                    self.visit_expr(Rc::make_mut(&mut item.key));
                    self.visit_expr(Rc::make_mut(&mut item.value));
                }
            }
            Expression_::TupleLiteral(items) => {
                for item in items {
                    self.visit_expr(Rc::make_mut(item));
                }
            }
            Expression_::StructLiteral(_, fields) => {
                for (_, value) in fields {
                    self.visit_expr(Rc::make_mut(value));
                }
            }
            Expression_::Parentheses(paren) => self.visit_expr(Rc::make_mut(&mut paren.expr)),
            Expression_::Break
            | Expression_::Continue
            | Expression_::IntLiteral(_)
            | Expression_::FloatLiteral(_)
            | Expression_::StringLiteral(_)
            | Expression_::Invalid => {}
        }
    }
}

/// A human-readable listing of every local variable in `items`, one per
/// line, showing whether it is a binding or a reference and the
/// `LocalVariableId` it resolved to (or `global` for a toplevel
/// variable). Used by the `reftest-var-ids` subcommand.
pub(crate) fn var_id_summary(items: &[ToplevelItem]) -> String {
    let mut summary = VarIdSummary { lines: vec![] };
    for item in items {
        summary.visit_toplevel_item(item);
    }
    summary.lines.join("\n")
}

struct VarIdSummary {
    lines: Vec<String>,
}

impl VarIdSummary {
    fn binding(&mut self, symbol: &Symbol) {
        self.record(symbol, "binding");
    }

    fn reference(&mut self, symbol: &Symbol) {
        self.record(symbol, "reference");
    }

    fn record(&mut self, symbol: &Symbol, kind: &str) {
        if symbol.name.is_underscore() {
            return;
        }

        let id = match symbol.var_id {
            Some(LocalVariableId(id)) => format!("#{id}"),
            None => "global".to_owned(),
        };
        self.lines.push(format!("{} {} {}", symbol.name, kind, id));
    }
}

impl Visitor for VarIdSummary {
    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        for param in &fun_info.params.params {
            self.binding(&param.symbol);
        }
        self.visit_block(&fun_info.body);
    }

    fn visit_method_info(&mut self, method_info: &MethodInfo) {
        self.binding(&method_info.receiver_sym);
        if let Some(fun_info) = method_info.fun_info() {
            self.visit_fun_info(fun_info);
        }
    }

    fn visit_expr_variable(&mut self, symbol: &Symbol) {
        self.reference(symbol);
    }

    fn visit_expr_assign(&mut self, symbol: &Symbol, expr: &Expression) {
        self.reference(symbol);
        self.visit_expr(expr);
    }

    fn visit_expr_assign_update(&mut self, symbol: &Symbol, expr: &Expression) {
        self.reference(symbol);
        self.visit_expr(expr);
    }

    fn visit_expr_for_in(&mut self, dest: &LetDestination, expr: &Expression, body: &Block) {
        self.visit_expr(expr);
        self.visit_dest(dest);
        self.visit_block(body);
    }

    fn visit_expr_try(&mut self, try_body: &Block, catch_sym: &Symbol, catch_body: &Block) {
        self.visit_block(try_body);
        self.binding(catch_sym);
        self.visit_block(catch_body);
    }

    fn visit_dest(&mut self, dest: &LetDestination) {
        match dest {
            LetDestination::Symbol(symbol) => self.binding(symbol),
            LetDestination::Destructure(symbols) => {
                for symbol in symbols {
                    self.binding(symbol);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use std::rc::Rc;

    use crate::parser::ast::IdGenerator;
    use crate::parser::parse_toplevel_items;
    use crate::parser::vfs::Vfs;

    use super::var_id_summary;

    /// Parse `src` and return the resolved variables, reusing `id_gen`
    /// so a sequence of calls mirrors evaluating REPL snippets.
    fn summary(src: &str, id_gen: &mut IdGenerator) -> String {
        let mut vfs = Vfs::default();
        let vfs_path = vfs.insert(Rc::new(PathBuf::from("__test.gdn")), src.to_owned());
        let (items, errors) = parse_toplevel_items(&vfs_path, src, id_gen);
        assert!(errors.is_empty(), "unexpected parse errors: {errors:?}");
        var_id_summary(&items)
    }

    #[test]
    fn test_reference_resolves_to_binding() {
        let mut id_gen = IdGenerator::default();
        let out = summary("fun f(x: Int): Int { x }", &mut id_gen);
        assert_eq!(out, "x binding #0\nx reference #0");
    }

    #[test]
    fn test_shadowing_gets_distinct_ids() {
        let mut id_gen = IdGenerator::default();
        let out = summary(
            "fun f(x: Int): Int { if x > 0 { let x = 1 x } else { x } }",
            &mut id_gen,
        );
        assert_eq!(
            out,
            "x binding #0\nx reference #0\nx binding #1\nx reference #1\nx reference #0"
        );
    }

    #[test]
    fn test_closure_captures_outer_binding() {
        let mut id_gen = IdGenerator::default();
        let out = summary("let f = fun(n: Int) { fun(m: Int) { n + m } }", &mut id_gen);
        assert_eq!(
            out,
            "n binding #0\nm binding #1\nn reference #0\nm reference #1\nf binding global"
        );
    }

    #[test]
    fn test_toplevel_variables_are_global() {
        let mut id_gen = IdGenerator::default();
        let out = summary("let a = 1\na", &mut id_gen);
        assert_eq!(out, "a binding global\na reference global");
    }

    #[test]
    fn test_var_ids_are_unique_across_repl_snippets() {
        // The REPL parses each snippet separately but reuses one
        // IdGenerator, so a local in a later snippet gets a fresh ID
        // rather than colliding with one from an earlier snippet.
        let mut id_gen = IdGenerator::default();

        let first = summary("fun f(x: Int): Int { x }", &mut id_gen);
        assert_eq!(first, "x binding #0\nx reference #0");

        let second = summary("fun g(y: Int): Int { y }", &mut id_gen);
        assert_eq!(second, "y binding #1\ny reference #1");
    }

    #[test]
    fn test_toplevel_binding_from_earlier_snippet_stays_global() {
        // A later snippet referring to a toplevel variable from an
        // earlier snippet leaves it unresolved, so evaluation falls
        // back to looking it up by name.
        let mut id_gen = IdGenerator::default();

        summary("let x = 1", &mut id_gen);

        let later = summary("fun f(): Int { x }", &mut id_gen);
        assert_eq!(later, "x reference global");
    }
}
