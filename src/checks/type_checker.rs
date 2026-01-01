use std::cell::RefCell;
use std::collections::HashSet;
use std::path::PathBuf;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::diagnostics::{Autofix, Diagnostic, Severity};
use crate::env::Env;
use crate::eval::most_similar;
use crate::garden_type::{is_subtype, Type, TypeDefKind, TypeVarEnv, UnwrapOrErrTy as _};
use crate::namespaces::NamespaceInfo;
use crate::parser::ast::{
    BinaryOperatorKind, Block, EnumInfo, Expression, Expression_, FunInfo, LetDestination,
    MethodInfo, ParenthesizedArguments, Pattern, StructInfo, Symbol, SymbolName, SyntaxId,
    TestInfo, ToplevelExpression, ToplevelItem, ToplevelItemId, TypeHint, TypeName, VariantInfo,
};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::diagnostics::MessagePart::*;
use crate::parser::position::Position;
use crate::parser::vfs::{VfsId, VfsPathBuf};
use crate::types::TypeDef;
use crate::values::{Value, Value_};
use crate::{msgcode, msgtext};

#[derive(Debug)]
pub(crate) struct TCSummary {
    /// Warnings and errors emitting during type checking.
    pub diagnostics: Vec<Diagnostic>,
    /// A mapping of syntax IDs to their inferred types, used for
    /// hover information.
    pub id_to_ty: FxHashMap<SyntaxId, Type>,
    /// A mapping of syntax IDs to the doc comment of their
    /// definitions.
    pub id_to_doc_comment: FxHashMap<SyntaxId, String>,
    /// A mapping of syntax IDs to their definition positions.
    ///
    /// E.g. if we see an occurrence of `None`, we map the ID of this
    /// occurrence to the definition position of `Option::None`.
    pub id_to_def_pos: FxHashMap<SyntaxId, Position>,
    /// A mapping from each toplevel definition to the other functions
    /// it calls. Does not include tests.
    pub callees: FxHashMap<Option<ToplevelItemId>, HashSet<ToplevelItemId>>,
    /// A mapping of expression IDs to the local bindings visible at
    /// that position. Used for code completion.
    pub id_to_bindings: FxHashMap<SyntaxId, Vec<(SymbolName, Type)>>,
}

pub(crate) fn check_types(
    path: &VfsPathBuf,
    items: &[ToplevelItem],
    env: &Env,
    _namespace: Rc<RefCell<NamespaceInfo>>,
) -> TCSummary {
    let mut visitor = TypeCheckVisitor {
        path: path.to_owned(),
        env,
        diagnostics: vec![],
        bindings: LocalBindings::new(env),
        id_to_ty: FxHashMap::default(),
        id_to_doc_comment: FxHashMap::default(),
        id_to_def_pos: FxHashMap::default(),
        callees: FxHashMap::default(),
        id_to_bindings: FxHashMap::default(),
        current_item: None,
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }

    TCSummary {
        diagnostics: visitor.diagnostics,
        id_to_ty: visitor.id_to_ty,
        id_to_doc_comment: visitor.id_to_doc_comment,
        id_to_def_pos: visitor.id_to_def_pos,
        callees: visitor.callees,
        id_to_bindings: visitor.id_to_bindings,
    }
}

#[derive(Debug)]
struct LocalBindings {
    blocks: Vec<FxHashMap<SymbolName, (Type, Position)>>,
}

impl LocalBindings {
    fn new(env: &Env) -> Self {
        let placeholder_path = Rc::new(PathBuf::from("__local_scope_placeholder__"));
        let placeholder_vfs_path = VfsPathBuf {
            path: placeholder_path,
            id: VfsId(u32::MAX),
        };

        let mut block_bindings = FxHashMap::default();
        if let Some(stack_top) = env.stack.0.last() {
            for (sym_id, value) in stack_top.bindings.all() {
                let Some(sym_name) = env.id_gen.intern_id_to_name.get(&sym_id) else {
                    continue;
                };
                let ty = Type::from_value(&value);

                block_bindings.insert(
                    sym_name.clone(),
                    (ty, Position::todo(&placeholder_vfs_path)),
                );
            }
        }

        Self {
            blocks: vec![block_bindings],
        }
    }
}

impl LocalBindings {
    fn enter_block(&mut self) {
        self.blocks.push(FxHashMap::default());
    }

    fn exit_block(&mut self) {
        self.blocks.pop();
    }

    fn get(&self, name: &SymbolName) -> Option<&(Type, Position)> {
        for block in self.blocks.iter().rev() {
            if let Some(ty_and_pos) = block.get(name) {
                return Some(ty_and_pos);
            }
        }

        None
    }

    fn set(&mut self, symbol: &Symbol, ty: Type) {
        let block = self.blocks.last_mut().expect("Should be non-empty");
        block.insert(symbol.name.clone(), (ty, symbol.position.clone()));
    }

    /// Return all bindings currently visible, with innermost scope
    /// taking precedence.
    fn all_bindings(&self) -> Vec<(SymbolName, Type)> {
        let mut seen: FxHashMap<SymbolName, Type> = FxHashMap::default();
        for block in self.blocks.iter().rev() {
            for (name, (ty, _)) in block {
                // Only insert if we haven't seen this name yet (inner scope shadows outer)
                seen.entry(name.clone()).or_insert_with(|| ty.clone());
            }
        }
        seen.into_iter().collect()
    }
}

#[derive(Debug)]
struct TypeCheckVisitor<'a> {
    path: VfsPathBuf,
    env: &'a Env,
    diagnostics: Vec<Diagnostic>,
    bindings: LocalBindings,
    id_to_ty: FxHashMap<SyntaxId, Type>,
    id_to_doc_comment: FxHashMap<SyntaxId, String>,
    id_to_def_pos: FxHashMap<SyntaxId, Position>,
    current_item: Option<ToplevelItemId>,
    callees: FxHashMap<Option<ToplevelItemId>, HashSet<ToplevelItemId>>,
    id_to_bindings: FxHashMap<SyntaxId, Vec<(SymbolName, Type)>>,
}

impl TypeCheckVisitor<'_> {
    fn visit_toplevel_item(&mut self, item: &ToplevelItem) {
        match &item {
            ToplevelItem::Fun(_, fun_info, _) => self.visit_fun_info(fun_info),
            ToplevelItem::Method(method_info, _) => self.visit_method_info(method_info),
            ToplevelItem::Test(test_info) => self.visit_test_info(test_info),
            ToplevelItem::Enum(enum_info) => self.visit_enum_info(enum_info),
            ToplevelItem::Struct(struct_info) => self.visit_struct_info(struct_info),
            ToplevelItem::Import(info) => {
                self.id_to_def_pos.insert(
                    info.id,
                    Position {
                        start_offset: 0,
                        end_offset: 0,
                        line_number: 0,
                        end_line_number: 0,
                        column: 0,
                        end_column: 0,
                        path: info.path.clone().into(),
                        vfs_path: VfsPathBuf {
                            path: Rc::new(info.path.clone()),
                            id: VfsId(0), // TODO
                        },
                    },
                );
            }
            ToplevelItem::Expr(toplevel_expr) => self.visit_toplevel_expr(toplevel_expr),
            ToplevelItem::Block(block) => self.visit_block(block),
        }
    }

    fn visit_test_info(&mut self, test_info: &TestInfo) {
        // Don't include tests call sites when computing callees, so
        // discard any additional callee found.
        let old_callees = self.callees.clone();
        self.visit_block(&test_info.body);
        self.callees = old_callees;
    }

    fn visit_method_info(&mut self, method_info: &MethodInfo) {
        self.bindings.enter_block();

        let mut type_bindings = FxHashMap::default();
        if let Some(fun_info) = method_info.fun_info() {
            for type_param in &fun_info.type_params {
                type_bindings.insert(type_param.name.clone(), None);
            }
        }

        let type_hint = &method_info.receiver_hint;
        if type_bindings.contains_key(&type_hint.sym.name) {
            self.diagnostics.push(Diagnostic {
                notes: vec![],
                fixes: vec![],
                severity: Severity::Error,
                message: ErrorMessage(vec![
                    msgtext!("Methods must be defined on specific types, such as "),
                    msgcode!("List"),
                    msgtext!(", not generic types."),
                ]),
                position: type_hint.position.clone(),
            });
        }

        let mut generic_args_seen: FxHashMap<TypeName, Position> = FxHashMap::default();
        for type_arg in &type_hint.args {
            if type_bindings.contains_key(&type_arg.sym.name) {
                match generic_args_seen.get(&type_arg.sym.name) {
                    Some(seen_pos) => {
                        self.diagnostics.push(Diagnostic {
                            notes: vec![(
                                ErrorMessage(vec![msgtext!("First occurrence here.")]),
                                seen_pos.clone(),
                            )],
                            fixes: vec![],
                            severity: Severity::Error,
                            message: ErrorMessage(vec![msgtext!(
                                "Methods cannot repeat generic type parameters."
                            )]),
                            position: type_arg.position.clone(),
                        });
                    }
                    None => {
                        generic_args_seen
                            .insert(type_arg.sym.name.clone(), type_arg.position.clone());
                    }
                }
            } else {
                self.diagnostics.push(Diagnostic {
                    notes: vec![],
                    fixes: vec![],
                    severity: Severity::Error,
                    message: ErrorMessage(vec![
                        msgtext!(
                            "Methods must be generic in all their type parameters, for example "
                        ),
                        msgcode!("List<T>"),
                        msgtext!(" rather than "),
                        msgcode!("List<Int>"),
                        msgtext!("."),
                    ]),
                    position: type_arg.position.clone(),
                });
            }
        }

        let self_ty = Type::from_hint(&method_info.receiver_hint, &self.env.types, &type_bindings)
            .unwrap_or_err_ty();
        self.bindings.set(&method_info.receiver_sym, self_ty);

        // TODO: generic variables are bound here.

        if let Some(fun_info) = method_info.fun_info() {
            self.visit_fun_info(fun_info);
        }

        self.bindings.exit_block();
    }

    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        if std::env::var_os("GDN_TRUST_PRELUDE").is_some() {
            // Skip typechecking built-ins and prelude to help print debugging.
            if let Some(n) = &fun_info.name_sym {
                let path = n.position.path.clone();
                if path.display().to_string().ends_with("__prelude.gdn") {
                    return;
                }
            }
        }

        let old_def_id = self.current_item;
        if let Some(def_id) = fun_info.item_id {
            self.current_item = Some(def_id);
        }

        let mut type_bindings = FxHashMap::default();
        for type_param in &fun_info.type_params {
            type_bindings.insert(type_param.name.clone(), None);
        }

        self.bindings.enter_block();

        for param in &fun_info.params.params {
            let param_ty = match &param.hint {
                Some(hint) => {
                    let hint_ty =
                        Type::from_hint(hint, &self.env.types, &type_bindings).unwrap_or_err_ty();
                    self.save_hint_ty_id(hint, &hint_ty);
                    hint_ty
                }
                None => Type::Any,
            };

            self.set_binding(&param.symbol, param_ty);
        }

        let return_ty = match &fun_info.return_hint {
            Some(hint) => {
                let hint_ty =
                    Type::from_hint(hint, &self.env.types, &type_bindings).unwrap_or_err_ty();
                self.save_hint_ty_id(hint, &hint_ty);

                hint_ty
            }
            None => Type::Any,
        };

        self.verify_block(&return_ty, &fun_info.body, &type_bindings, &return_ty);

        self.bindings.exit_block();

        self.current_item = old_def_id;
    }

    fn visit_struct_info(&mut self, struct_info: &StructInfo) {
        self.bindings.enter_block();

        let mut type_bindings = FxHashMap::default();
        for type_param in &struct_info.type_params {
            type_bindings.insert(type_param.name.clone(), None);
        }

        for field in &struct_info.fields {
            let field_ty =
                Type::from_hint(&field.hint, &self.env.types, &type_bindings).unwrap_or_err_ty();
            self.save_hint_ty_id(&field.hint, &field_ty);
        }

        self.bindings.exit_block();
    }

    fn visit_enum_info(&mut self, enum_info: &EnumInfo) {
        self.bindings.enter_block();

        let mut type_bindings = FxHashMap::default();
        for type_param in &enum_info.type_params {
            type_bindings.insert(type_param.name.clone(), None);
        }

        for variant in &enum_info.variants {
            let Some(payload_hint) = &variant.payload_hint else {
                continue;
            };
            let payload_ty =
                Type::from_hint(payload_hint, &self.env.types, &type_bindings).unwrap_or_err_ty();
            self.save_hint_ty_id(payload_hint, &payload_ty);
        }

        self.bindings.exit_block();
    }

    fn visit_block(&mut self, block: &Block) {
        // infer_block recurses, so don't recurse in the visitor.
        self.infer_block(block, &FxHashMap::default(), &Type::Any);
    }

    fn visit_toplevel_expr(&mut self, toplevel_expr: &ToplevelExpression) {
        self.infer_expr(&toplevel_expr.0, &FxHashMap::default(), &Type::Any);
    }

    /// Update `id_to_pos` for this occurrence of an enum variant,
    /// e.g. an instance of the literal `True`.
    fn save_enum_variant_id(&mut self, occurrence_sym: &Symbol, value: Value) {
        let type_name = match value.as_ref() {
            Value_::EnumVariant { type_name, .. } | Value_::EnumConstructor { type_name, .. } => {
                type_name
            }
            _ => return,
        };

        let Some(TypeDef::Enum(enum_info)) = self.env.get_type_def(type_name) else {
            return;
        };

        for variant in &enum_info.variants {
            if variant.name_sym.name == occurrence_sym.name {
                self.id_to_def_pos
                    .insert(occurrence_sym.id, variant.name_sym.position.clone());
                return;
            }
        }

        // We found the enum, but not this variant. Go to the start of
        // the enum definition so we at least give the user a hint.
        self.id_to_def_pos
            .insert(occurrence_sym.id, enum_info.name_sym.position.clone());
    }

    /// Update `id_to_pos` for this type hint.
    fn save_hint_ty_id(&mut self, hint: &TypeHint, hint_ty: &Type) {
        // Recurse on hint arguments.
        let hint_ty_args: Vec<Type> = match hint_ty {
            Type::Tuple(args) => args.clone(),
            Type::Fun {
                params, return_, ..
            } => vec![Type::Tuple(params.clone()), *return_.clone()],
            Type::UserDefined { args, .. } => args.clone(),
            _ => vec![],
        };
        for (hint_ty_arg, hint_arg) in hint_ty_args.iter().zip(&hint.args) {
            self.save_hint_ty_id(hint_arg, hint_ty_arg)
        }

        let ty_name: TypeName = match &hint_ty {
            Type::UserDefined { name: name_sym, .. } => name_sym.clone(),
            _ => {
                return;
            }
        };

        if let Some(type_def) = self.env.get_type_def(&ty_name) {
            let (def_name_sym, doc_comment) = match type_def {
                TypeDef::BuiltIn(_, Some(struct_info)) => {
                    (&struct_info.name_sym, struct_info.doc_comment.clone())
                }
                TypeDef::BuiltIn(_, None) => {
                    return;
                }
                TypeDef::Enum(enum_info) => (&enum_info.name_sym, enum_info.doc_comment.clone()),
                TypeDef::Struct(struct_info) => {
                    (&struct_info.name_sym, struct_info.doc_comment.clone())
                }
            };

            let hint_id = hint.sym.id;
            self.id_to_def_pos
                .insert(hint_id, def_name_sym.position.clone());

            if let Some(doc_comment) = doc_comment {
                self.id_to_doc_comment.insert(hint_id, doc_comment);
            }
        }
    }

    fn arity_diagnostics(
        &mut self,
        name: Option<&String>,
        expected_args: &[Type],
        // The type of the value, the position of the value, the comma position.
        actual_args: &[(Type, Position, Option<Position>)],
        paren_args: &ParenthesizedArguments,
    ) {
        debug_assert!(expected_args.len() != actual_args.len());

        let formatted_name = match name {
            Some(name) => msgcode!("{}", name),
            None => msgtext!("This function call"),
        };

        for (i, (arg_ty, arg_pos, comma_pos)) in actual_args.iter().enumerate() {
            let Some(expected_ty) = expected_args.get(i) else {
                break;
            };
            if is_subtype(arg_ty, expected_ty) {
                // This argument is fine.
                continue;
            }

            if let Some(next_expected_ty) = expected_args.get(i + 1) {
                // This argument looks like it matches the next parameter, presumably we
                // forgot an argument.
                if is_subtype(arg_ty, next_expected_ty) {
                    let message = ErrorMessage(vec![
                        formatted_name,
                        msgtext!(" requires an additional ",),
                        msgcode!("{}", expected_ty),
                        msgtext!(" argument here."),
                    ]);

                    let mut position = paren_args.open_paren.clone();
                    if i > 0 {
                        let (_, _, prev_comma) = &actual_args[i - 1];
                        if let Some(prev_comma) = prev_comma {
                            position = prev_comma.clone();
                        }
                    }
                    self.diagnostics.push(Diagnostic {
                        notes: vec![],
                        fixes: vec![],
                        severity: Severity::Error,
                        message,
                        position,
                    });
                    return;
                }
            };

            let Some((next_arg_ty, _, _)) = actual_args.get(i + 1) else {
                break;
            };

            if is_subtype(next_arg_ty, expected_ty) && actual_args.len() > expected_args.len() {
                let message = ErrorMessage(vec![
                    msgtext!("Unexpected extra argument. ",),
                    formatted_name,
                    msgtext!(
                        " requires {} argument{}.",
                        expected_args.len(),
                        if expected_args.len() == 1 { "" } else { "s" }
                    ),
                ]);

                let mut position = arg_pos.clone();
                if let Some(comma_pos) = comma_pos {
                    position = Position::merge(&position, comma_pos);
                }

                self.diagnostics.push(Diagnostic {
                    notes: vec![],
                    fixes: vec![],
                    severity: Severity::Error,
                    message,
                    position,
                });
                return;
            }
        }

        if let Some(last_param_ty) = expected_args.last() {
            if paren_args.arguments.len() + 1 == expected_args.len() {
                let message = ErrorMessage(vec![
                    formatted_name,
                    msgtext!(" requires an additional ",),
                    msgcode!("{}", last_param_ty),
                    msgtext!(" argument."),
                ]);
                self.diagnostics.push(Diagnostic {
                    notes: vec![],
                    fixes: vec![],
                    severity: Severity::Error,
                    message,
                    position: paren_args.close_paren.clone(),
                });
                return;
            }
        }

        let message = ErrorMessage(vec![
            formatted_name,
            msgtext!(
                " expects {} argument{}, but got {}.",
                expected_args.len(),
                if expected_args.len() == 1 { "" } else { "s" },
                paren_args.arguments.len()
            ),
        ]);

        if expected_args.len() < paren_args.arguments.len() {
            // Got too many arguments.
            let first_excess_arg = &paren_args.arguments[expected_args.len()];
            let last_arg = paren_args.arguments.last().unwrap();

            let position =
                Position::merge(&first_excess_arg.expr.position, &last_arg.expr.position);

            self.diagnostics.push(Diagnostic {
                notes: vec![],
                fixes: vec![],
                severity: Severity::Error,
                message,
                position,
            });
        } else if expected_args.len() > paren_args.arguments.len() {
            // Got too few arguments.

            let position = if paren_args.arguments.is_empty() {
                // Highlight both parens when we need arguments but got none.
                Position::merge(&paren_args.open_paren, &paren_args.close_paren)
            } else {
                // We had a non-zero number of arguments, but not
                // enough. Just highlight the closing paren.
                paren_args.close_paren.clone()
            };

            self.diagnostics.push(Diagnostic {
                notes: vec![],
                fixes: vec![],
                severity: Severity::Error,
                message,
                position,
            });
        }
    }

    /// Run type inference on this block, and return the type of the
    /// last expression.
    fn infer_block(
        &mut self,
        block: &Block,
        type_bindings: &TypeVarEnv,
        expected_return_ty: &Type,
    ) -> Type {
        self.bindings.enter_block();

        let mut ty = Type::unit();
        for expr in &block.exprs {
            ty = self.infer_expr(expr, type_bindings, expected_return_ty);
        }

        self.bindings.exit_block();
        ty
    }

    fn verify_block(
        &mut self,
        expected_ty: &Type,
        block: &Block,
        type_bindings: &TypeVarEnv,
        expected_return_ty: &Type,
    ) -> Type {
        self.bindings.enter_block();

        let mut ty = Type::unit();
        for (i, expr) in block.exprs.iter().enumerate() {
            if i == block.exprs.len() - 1 {
                ty = self.verify_expr(expected_ty, expr, type_bindings, expected_return_ty);
            } else {
                self.infer_expr(expr, type_bindings, expected_return_ty);
            }
        }

        if block.exprs.is_empty() && !is_subtype(&ty, expected_ty) {
            let block_pos = Position::merge(&block.open_brace, &block.close_brace);

            self.diagnostics.push(Diagnostic {
                notes: vec![],
                fixes: vec![],
                severity: Severity::Error,
                message: format_type_mismatch(expected_ty, &ty),
                position: block_pos,
            });
        }

        self.bindings.exit_block();
        ty
    }

    fn infer_expr(
        &mut self,
        expr: &Expression,
        type_bindings: &TypeVarEnv,
        expected_return_ty: &Type,
    ) -> Type {
        let ty = self.infer_expr_(
            &expr.expr_,
            &expr.position,
            expr.id,
            type_bindings,
            expected_return_ty,
        );
        self.id_to_ty.insert(expr.id, ty.clone());

        ty
    }

    fn verify_match(
        &mut self,
        expected_ty: &Type,
        type_bindings: &TypeVarEnv,
        expected_return_ty: &Type,
        expr_id: SyntaxId,
        scrutinee: &Expression,
        cases: &[(Pattern, Block)],
    ) -> Type {
        let scrutinee_ty = self.infer_expr(scrutinee, type_bindings, expected_return_ty);
        let scrutinee_ty_name = scrutinee_ty.type_name();

        if let Some(scrutinee_ty_name) = &scrutinee_ty_name {
            let patterns: Vec<_> = cases.iter().map(|(p, _)| p.clone()).collect();
            check_match_exhaustive(
                self.env,
                &scrutinee.position,
                scrutinee_ty_name,
                &patterns,
                &mut self.diagnostics,
            );
        }

        let mut case_tys = vec![];

        for (pattern, case_expr) in cases {
            self.bindings.enter_block();

            if let Some(payload_dest) = &pattern.payload {
                let payload_ty = enum_payload_type(self.env, &scrutinee_ty, &pattern.variant_sym);
                self.set_dest_binding(payload_dest, &pattern.variant_sym.position, payload_ty);
            }

            let case_value_pos = match case_expr.exprs.last() {
                Some(last_expr) => last_expr.position.clone(),
                None => Position::merge(&case_expr.open_brace, &case_expr.close_brace),
            };

            match expected_ty {
                Type::Any => {
                    case_tys.push((
                        self.infer_block(case_expr, type_bindings, expected_return_ty),
                        case_value_pos,
                    ));
                }
                _ => {
                    self.verify_block(expected_ty, case_expr, type_bindings, expected_return_ty);
                }
            }

            self.bindings.exit_block();

            // Matching `_` works for any type.
            if pattern.variant_sym.name.is_underscore() {
                continue;
            }

            let Some(value) = self.get_var(&pattern.variant_sym.name) else {
                self.diagnostics.push(Diagnostic {
                    notes: vec![],
                    fixes: vec![],
                    severity: Severity::Error,
                    message: ErrorMessage(vec![
                        msgtext!("No such type "),
                        msgcode!("{}", pattern.variant_sym.name),
                        msgtext!("."),
                    ]),
                    position: pattern.variant_sym.position.clone(),
                });
                continue;
            };

            let pattern_type_name = match value.as_ref() {
                Value_::EnumVariant { type_name, .. } => type_name,
                Value_::EnumConstructor { type_name, .. } => type_name,
                _ => {
                    self.diagnostics.push(Diagnostic {
                        notes: vec![],
                        fixes: vec![],
                        severity: Severity::Error,
                        message: ErrorMessage(vec![
                            msgtext!("Expected an enum variant here, but got "),
                            msgcode!("{}", value.display(self.env)),
                            msgtext!("."),
                        ]),
                        position: pattern.variant_sym.position.clone(),
                    });
                    continue;
                }
            };

            let Some(scrutinee_ty_name) = &scrutinee_ty_name else {
                continue;
            };
            if scrutinee_ty_name.is_no_value() {
                continue;
            }
            if pattern_type_name != scrutinee_ty_name {
                self.diagnostics.push(Diagnostic {
                    notes: vec![],
                    fixes: vec![],
                    severity: Severity::Error,
                    message: ErrorMessage(vec![
                        msgtext!("This match case is for "),
                        msgcode!("{}", pattern_type_name),
                        msgtext!(", but you're matching on a "),
                        msgcode!("{}", scrutinee_ty_name),
                        msgtext!("."),
                    ]),
                    position: pattern.variant_sym.position.clone(),
                });
            }
        }

        let ty = match expected_ty {
            Type::Any => match unify_all(&case_tys) {
                Ok(ty) => ty,
                Err(position) => {
                    self.diagnostics.push(Diagnostic {
                        notes: vec![],
                        fixes: vec![],
                        severity: Severity::Error,
                        message: ErrorMessage(vec![
                            msgcode!("match"),
                            msgtext!(" cases have different types."),
                        ]),
                        position,
                    });

                    Type::error("Cases had incompatible types.")
                }
            },
            _ => {
                debug_assert_eq!(case_tys.len(), 0);
                expected_ty.clone()
            }
        };

        self.id_to_ty.insert(expr_id, ty.clone());
        ty
    }

    fn infer_expr_(
        &mut self,
        expr_: &Expression_,
        pos: &Position,
        expr_id: SyntaxId,
        type_bindings: &TypeVarEnv,
        expected_return_ty: &Type,
    ) -> Type {
        match expr_ {
            Expression_::Match(scrutinee, cases) => self.verify_match(
                &Type::Any,
                type_bindings,
                expected_return_ty,
                expr_id,
                scrutinee,
                cases,
            ),
            Expression_::If(cond_expr, then_block, else_block) => self.infer_if(
                cond_expr,
                then_block,
                else_block,
                type_bindings,
                expected_return_ty,
            ),
            Expression_::While(cond_expr, block) => {
                self.verify_expr(&Type::bool(), cond_expr, type_bindings, expected_return_ty);
                self.infer_block(block, type_bindings, expected_return_ty);
                Type::unit()
            }
            Expression_::ForIn(dest, expr, body) => {
                let expr_ty = self.verify_expr(
                    &Type::list(Type::Any),
                    expr,
                    type_bindings,
                    expected_return_ty,
                );

                self.bindings.enter_block();

                let elem_ty = match expr_ty {
                    Type::UserDefined { name, args, .. } if name.text == "List" => {
                        if let Some(arg) = args.first() {
                            arg.clone()
                        } else {
                            Type::error("Bad list arity")
                        }
                    }
                    _ => Type::error("For loop expression that isn't a list"),
                };

                self.set_dest_binding(dest, &expr.position, elem_ty);

                self.infer_block(body, type_bindings, expected_return_ty);
                self.bindings.exit_block();

                Type::unit()
            }
            Expression_::Break | Expression_::Continue => {
                // As with `return`, these never produce a value, they
                // jump elsewhere. `let x = break` will never assign
                // a value to x.
                Type::no_value()
            }
            Expression_::Assign(sym, expr) => {
                // TODO: also enforce the type of an assignment at runtime.
                let expected_ty = self.get_var_for_assignment(sym);

                self.verify_expr(&expected_ty, expr, type_bindings, expected_return_ty);
                Type::unit()
            }
            Expression_::AssignUpdate(sym, op, expr) => {
                // TODO: also enforce the type of an assignment at runtime.

                let sym_ty = self.get_var_for_assignment(sym);
                if !is_subtype(&sym_ty, &Type::int()) {
                    self.diagnostics.push(Diagnostic {
                        notes: vec![],
                        fixes: vec![],
                        severity: Severity::Error,
                        message: ErrorMessage(vec![
                            msgcode!("{}", op.as_src()),
                            msgtext!(" can only be used with "),
                            msgcode!("Int"),
                            msgtext!(" variables, but got "),
                            msgcode!("{}", sym_ty),
                            msgtext!("."),
                        ]),
                        position: sym.position.clone(),
                    });
                }

                self.verify_expr(&Type::int(), expr, type_bindings, expected_return_ty);
                Type::unit()
            }
            Expression_::Let(dest, hint, expr) => {
                let ty = match hint {
                    Some(hint) => {
                        let hint_ty = Type::from_hint(hint, &self.env.types, type_bindings)
                            .unwrap_or_err_ty();
                        self.save_hint_ty_id(hint, &hint_ty);

                        self.verify_expr(&hint_ty, expr, type_bindings, expected_return_ty);

                        hint_ty
                    }
                    None => self.infer_expr(expr, type_bindings, expected_return_ty),
                };

                self.set_dest_binding(dest, &expr.position, ty);
                Type::unit()
            }
            Expression_::Return(inner_expr) => {
                let expr_ty = expected_return_ty;
                match inner_expr {
                    Some(expr) => {
                        self.verify_expr(expr_ty, expr, type_bindings, expected_return_ty);
                    }
                    None => {
                        if !is_subtype(&Type::unit(), expr_ty) {
                            self.diagnostics.push(Diagnostic {
                                notes: vec![],
                                fixes: vec![],
                                severity: Severity::Error,
                                message: ErrorMessage(vec![
                                    msgtext!("Expected this function to return "),
                                    msgcode!("{}", expr_ty),
                                    msgtext!(", but got "),
                                    msgcode!("Unit"),
                                    msgtext!("."),
                                ]),
                                position: pos.clone(),
                            });
                        }
                    }
                }
                // `return` terminates the current function, so we can't
                // use this expression. Infer as bottom.
                Type::no_value()
            }
            Expression_::IntLiteral(_) => Type::int(),
            Expression_::StringLiteral(_) => Type::string(),
            Expression_::ListLiteral(items) => {
                let item_tys = items
                    .iter()
                    .map(|item| {
                        (
                            self.infer_expr(&item.expr, type_bindings, expected_return_ty),
                            item.expr.position.clone(),
                        )
                    })
                    .collect::<Vec<_>>();

                let elem_ty = match unify_all(&item_tys) {
                    Ok(ty) => ty,
                    Err(position) => {
                        self.diagnostics.push(Diagnostic {
                            notes: vec![],
                            fixes: vec![],
                            severity: Severity::Error,
                            message: ErrorMessage(vec![Text(
                                "List elements have different types.".to_owned(),
                            )]),
                            position,
                        });
                        Type::Any
                    }
                };

                Type::list(elem_ty)
            }
            Expression_::DictLiteral(items) => {
                let mut value_tys = vec![];
                for (key_expr, _, value_expr) in items {
                    self.verify_expr(&Type::string(), key_expr, type_bindings, expected_return_ty);

                    let inferred_value_ty =
                        self.infer_expr(value_expr, type_bindings, expected_return_ty);
                    value_tys.push((inferred_value_ty, value_expr.position.clone()));
                }

                let value_ty = match unify_all(&value_tys) {
                    Ok(ty) => ty,
                    Err(position) => {
                        self.diagnostics.push(Diagnostic {
                            notes: vec![],
                            fixes: vec![],
                            severity: Severity::Error,
                            message: ErrorMessage(vec![Text(
                                "Dict values have different types.".to_owned(),
                            )]),
                            position,
                        });
                        Type::Any
                    }
                };

                Type::dict(value_ty)
            }
            Expression_::TupleLiteral(items) => {
                let item_tys: Vec<_> = items
                    .iter()
                    .map(|item| self.infer_expr(item, type_bindings, expected_return_ty))
                    .collect();
                Type::Tuple(item_tys)
            }
            Expression_::StructLiteral(name_sym, fields) => {
                self.infer_struct_literal(name_sym, fields, type_bindings, expected_return_ty)
            }
            Expression_::BinaryOperator(lhs, op, rhs) => {
                self.infer_binary_op(pos, lhs, op, rhs, type_bindings, expected_return_ty)
            }
            Expression_::Variable(sym) => self.infer_var(expr_id, sym),
            Expression_::Call(recv, paren_args) => {
                self.infer_call(pos, recv, paren_args, type_bindings, expected_return_ty)
            }
            Expression_::MethodCall(recv, sym, paren_args) => {
                self.infer_method_call(recv, sym, paren_args, type_bindings, expected_return_ty)
            }
            Expression_::DotAccess(recv, field_sym) => {
                self.infer_dot_access(recv, field_sym, type_bindings, expected_return_ty)
            }
            Expression_::NamespaceAccess(recv, sym) => {
                self.infer_namespace_access(recv, sym, type_bindings, expected_return_ty)
            }
            Expression_::FunLiteral(fun_info) => {
                let param_tys = fun_info
                    .params
                    .params
                    .iter()
                    .map(|param| match &param.hint {
                        Some(hint) => {
                            Type::from_hint(hint, &self.env.types, type_bindings).unwrap_or_err_ty()
                        }
                        None => Type::Any,
                    })
                    .collect::<Vec<_>>();

                let return_ty = match &fun_info.return_hint {
                    Some(hint) => {
                        Type::from_hint(hint, &self.env.types, type_bindings).unwrap_or_err_ty()
                    }
                    None => Type::Any,
                };

                let expected_ty = Type::Fun {
                    type_params: vec![],
                    params: param_tys,
                    return_: Box::new(return_ty),
                    name_sym: None,
                };
                self.verify_expr_(
                    &expected_ty,
                    expr_,
                    pos,
                    expr_id,
                    type_bindings,
                    expected_return_ty,
                )
            }
            Expression_::Assert(expr) => {
                self.verify_expr(&Type::bool(), expr, type_bindings, expected_return_ty);
                Type::unit()
            }
            Expression_::Parentheses(_, expr, _) => {
                self.infer_expr(expr, type_bindings, expected_return_ty)
            }
            Expression_::Invalid => {
                // We've already emitted a parse error, so use the
                // bottom type to prevent later type errors.
                Type::no_value()
            }
        }
    }

    fn infer_dot_access(
        &mut self,
        recv: &Rc<Expression>,
        field_sym: &Symbol,
        type_bindings: &TypeVarEnv,
        expected_return_ty: &Type,
    ) -> Type {
        let recv_ty = self.infer_expr(recv, type_bindings, expected_return_ty);
        let Some(recv_ty_name) = recv_ty.type_name() else {
            return Type::error("No type name found for this receiver");
        };

        match recv_ty {
            Type::UserDefined {
                kind: TypeDefKind::Struct,
                ref name,
                ..
            } => {
                if let Some(TypeDef::Struct(struct_info)) = self.env.get_type_def(name) {
                    for field in &struct_info.fields {
                        if field.sym.name == field_sym.name {
                            self.id_to_def_pos
                                .insert(field_sym.id, field.sym.position.clone());

                            let field_ty =
                                Type::from_hint(&field.hint, &self.env.types, type_bindings)
                                    .unwrap_or_err_ty();
                            return field_ty;
                        }
                    }

                    let notes = match recv_ty.def_sym_pos(self.env) {
                        Some(pos) => vec![(
                            ErrorMessage(vec![
                                msgcode!("{}", recv_ty_name),
                                msgtext!(" is defined here."),
                            ]),
                            pos,
                        )],
                        None => vec![],
                    };

                    self.diagnostics.push(Diagnostic {
                        notes,
                        fixes: vec![],
                        severity: Severity::Error,
                        message: ErrorMessage(vec![
                            msgtext!("Struct "),
                            msgcode!("{}", recv_ty_name),
                            msgtext!(" has no field "),
                            msgcode!("{}", field_sym.name),
                            msgtext!("."),
                        ]),
                        position: field_sym.position.clone(),
                    });

                    Type::error("No struct field with this name")
                } else {
                    self.diagnostics.push(Diagnostic {
                        notes: vec![],
                        fixes: vec![],
                        severity: Severity::Error,
                        message: ErrorMessage(vec![
                            msgcode!("{}", recv_ty_name),
                            msgtext!(" is not a struct."),
                        ]),
                        position: field_sym.position.clone(),
                    });

                    Type::error("No type with this name or type is not a struct")
                }
            }
            _ => {
                self.diagnostics.push(Diagnostic {
                    notes: vec![],
                    fixes: vec![],
                    severity: Severity::Error,
                    message: ErrorMessage(vec![
                        msgcode!("{}", recv_ty_name),
                        msgtext!(" is not a struct."),
                    ]),
                    position: field_sym.position.clone(),
                });

                Type::error("This type is not a user-defined type")
            }
        }
    }

    fn infer_method_call(
        &mut self,
        recv: &Rc<Expression>,
        sym: &Symbol,
        paren_args: &ParenthesizedArguments,
        type_bindings: &TypeVarEnv,
        expected_return_ty: &Type,
    ) -> Type {
        let receiver_ty = self.infer_expr(recv, type_bindings, expected_return_ty);
        if matches!(receiver_ty, Type::Error { .. }) {
            for arg in &paren_args.arguments {
                self.infer_expr(&arg.expr, type_bindings, expected_return_ty);
            }

            // Allow calling methods on error types, to avoid cascading errors.
            return Type::error("Called method on an error type");
        }

        let Some(receiver_ty_name) = receiver_ty.type_name() else {
            for arg in &paren_args.arguments {
                self.infer_expr(&arg.expr, type_bindings, expected_return_ty);
            }

            self.diagnostics.push(Diagnostic {
                notes: vec![],
                fixes: vec![],
                severity: Severity::Error,
                message: ErrorMessage(vec![
                    msgtext!("Expected a type with a "),
                    msgcode!("{}", sym.name),
                    msgtext!(" method, but got "),
                    msgcode!("{}", receiver_ty),
                    msgtext!("."),
                ]),
                position: recv.position.clone(),
            });
            return Type::error("No type name for this method receiver");
        };

        let methods = self
            .env
            .types
            .get(&receiver_ty_name)
            .cloned()
            .map(|tdm| tdm.methods)
            .unwrap_or_default();

        match methods.get(&sym.name) {
            Some(method_info) => {
                self.id_to_def_pos
                    .insert(sym.id, method_info.name_sym.position.clone());

                let Some(fun_info) = method_info.fun_info() else {
                    self.diagnostics.push(Diagnostic {
                        notes: vec![],
                        fixes: vec![],
                        severity: Severity::Error,
                        message: ErrorMessage(vec![msgtext!("Tried to call a method that has no fun_info. This means there's a bug in prelude loading.")]),
                        position: sym.position.clone(),
                    });
                    return Type::error("This method has no fun_info");
                };

                if let Some(def_id) = fun_info.item_id {
                    self.callees
                        .entry(self.current_item)
                        .or_default()
                        .insert(def_id);
                }

                let mut ty_var_env = TypeVarEnv::default();
                for type_param in &fun_info.type_params {
                    ty_var_env.insert(type_param.name.clone(), None);
                }

                let param_decl_tys: Vec<Type> = fun_info
                    .params
                    .params
                    .iter()
                    .map(|sym_with_hint| match &sym_with_hint.hint {
                        Some(hint) => {
                            Type::from_hint(hint, &self.env.types, &ty_var_env).unwrap_or_err_ty()
                        }
                        None => Type::Any,
                    })
                    .collect();

                let recv_decl_ty =
                    Type::from_hint(&method_info.receiver_hint, &self.env.types, &ty_var_env)
                        .unwrap_or_err_ty();
                unify_and_solve_ty(&recv_decl_ty, &receiver_ty, &mut ty_var_env);

                let mut arg_tys = vec![];
                for arg in &paren_args.arguments {
                    let arg_ty = self.infer_expr(&arg.expr, type_bindings, expected_return_ty);
                    arg_tys.push((arg_ty, arg.expr.position.clone(), arg.comma.clone()));
                }

                for (param_ty, (arg_ty, _, _)) in param_decl_tys.iter().zip(arg_tys.iter()) {
                    unify_and_solve_ty(param_ty, arg_ty, &mut ty_var_env);
                }

                let params = param_decl_tys
                    .iter()
                    .map(|param_decl_ty| substitute_ty_vars(param_decl_ty, &ty_var_env))
                    .collect::<Vec<_>>();

                for (param_ty, (arg_ty, arg_pos, _)) in params.iter().zip(&arg_tys) {
                    if !is_subtype(arg_ty, param_ty) {
                        self.diagnostics.push(Diagnostic {
                            notes: vec![],
                            fixes: vec![],
                            severity: Severity::Error,
                            message: format_type_mismatch(param_ty, arg_ty),
                            position: arg_pos.clone(),
                        });
                    }
                }

                if fun_info.params.params.len() != paren_args.arguments.len() {
                    let name = format!("{}::{}", receiver_ty_name, sym.name);
                    self.arity_diagnostics(Some(&name), &params, &arg_tys, paren_args);
                }

                let (more_diagnostics, ret_ty) = subst_type_vars_in_meth_return_ty(
                    self.env,
                    method_info,
                    &recv.position,
                    &receiver_ty,
                    &arg_tys,
                    &mut ty_var_env,
                );
                self.diagnostics.extend(more_diagnostics);

                ret_ty
            }
            None => {
                // No method exists with that name on this type.

                for arg in &paren_args.arguments {
                    self.infer_expr(&arg.expr, type_bindings, expected_return_ty);
                }

                let mut fixes = vec![];

                let available_methods = methods.keys().collect::<Vec<_>>();
                let suggest = if let Some(similar) = most_similar(&available_methods, &sym.name) {
                    let fix = Autofix {
                        description: format!("Use `{}` here.", similar),
                        position: sym.position.clone(),
                        new_text: similar.text.clone(),
                    };
                    fixes.push(fix);

                    // TODO: consider arity and types when
                    // trying to suggest the best
                    // alternative.
                    format!(" Did you mean `{similar}`?")
                } else {
                    "".to_owned()
                };

                self.diagnostics.push(Diagnostic {
                    severity: Severity::Error,
                    message: ErrorMessage(vec![
                        msgcode!("{}", receiver_ty_name),
                        msgtext!(" has no method "),
                        msgcode!("{}", sym.name),
                        msgtext!(".{}", suggest),
                    ]),
                    position: sym.position.clone(),
                    notes: vec![],
                    fixes,
                });
                Type::error("No such method on this type")
            }
        }
    }

    fn infer_call(
        &mut self,
        pos: &Position,
        recv: &Expression,
        paren_args: &ParenthesizedArguments,
        type_bindings: &TypeVarEnv,
        expected_return_ty: &Type,
    ) -> Type {
        if let Expression_::Variable(s) = &recv.expr_ {
            if s.name.text == "todo" && self.bindings.get(&SymbolName::from("todo")).is_none() {
                self.diagnostics.push(Diagnostic {
                    message: ErrorMessage(vec![Text("Unfinished code.".to_owned())]),
                    position: pos.clone(),
                    notes: vec![],
                    fixes: vec![],
                    severity: Severity::Warning,
                });
            }
        }

        let recv_ty = self.infer_expr(recv, type_bindings, expected_return_ty);

        match recv_ty {
            Type::Fun {
                type_params,
                params,
                return_,
                name_sym,
            } => {
                let mut ty_var_env = TypeVarEnv::default();
                for type_param in type_params {
                    ty_var_env.insert(type_param.clone(), None);
                }

                let mut arg_tys = vec![];
                for arg in &paren_args.arguments {
                    let arg_ty = self.infer_expr(&arg.expr, type_bindings, expected_return_ty);

                    arg_tys.push((arg_ty, arg.expr.position.clone(), arg.comma.clone()));
                }

                for (param_ty, (arg_ty, _, _)) in params.iter().zip(arg_tys.iter()) {
                    unify_and_solve_ty(param_ty, arg_ty, &mut ty_var_env);
                }

                let params = params
                    .iter()
                    .map(|p| substitute_ty_vars(p, &ty_var_env))
                    .collect::<Vec<_>>();

                if params.len() == arg_tys.len() {
                    // Only check argument types if we have the right number of
                    // arguments. Otherwise, it's likely that the types are valid,
                    // but there were missing previous arguments.
                    for (param_ty, (arg_ty, arg_pos, _)) in params.iter().zip(&arg_tys) {
                        if !is_subtype(arg_ty, param_ty) {
                            self.diagnostics.push(Diagnostic {
                                notes: vec![],
                                fixes: vec![],
                                severity: Severity::Error,
                                message: format_type_mismatch(param_ty, arg_ty),
                                position: arg_pos.clone(),
                            });
                        }
                    }
                } else {
                    let name = name_sym.map(|sym| sym.name.text.clone());
                    self.arity_diagnostics(name.as_ref(), &params, &arg_tys, paren_args);
                }

                substitute_ty_vars(&return_, &ty_var_env)
            }
            Type::Error {
                internal_reason,
                inferred_type: _,
            } => {
                for arg in &paren_args.arguments {
                    // We still want to check arguments as far as possible.
                    self.infer_expr(&arg.expr, type_bindings, expected_return_ty);
                }

                // If the receiver is an error, propagate the
                // reason to aid debugging, but not the
                // inferred type (which isn't relevant).
                Type::Error {
                    internal_reason,
                    inferred_type: None,
                }
            }
            _ => {
                for arg in &paren_args.arguments {
                    // We still want to check arguments as far as possible.
                    self.infer_expr(&arg.expr, type_bindings, expected_return_ty);
                }

                self.diagnostics.push(Diagnostic {
                    notes: vec![],
                    fixes: vec![],
                    severity: Severity::Error,
                    message: ErrorMessage(vec![
                        msgtext!("Expected a function, but got "),
                        msgcode!("{}", recv_ty),
                        msgtext!("."),
                    ]),
                    position: recv.position.clone(),
                });

                Type::Error {
                    internal_reason: "Calling something that isn't a function".to_owned(),
                    inferred_type: None,
                }
            }
        }
    }

    fn infer_var(&mut self, expr_id: SyntaxId, sym: &Symbol) -> Type {
        // Store visible bindings at this expression for code completion.
        self.id_to_bindings
            .insert(expr_id, self.bindings.all_bindings());

        if let Some((value_ty, position)) = self.bindings.get(&sym.name) {
            self.id_to_def_pos.insert(sym.id, position.clone());
            self.id_to_ty.insert(sym.id, value_ty.clone());
            self.id_to_ty.insert(expr_id, value_ty.clone());
            return value_ty.clone();
        }

        let Some(value) = self.get_var(&sym.name) else {
            let ty = Type::Error {
                internal_reason: "Unbound variable".to_owned(),
                inferred_type: None,
            };
            // Treat this variable as locally bound in the
            // rest of the scope, to prevent cascading
            // errors.
            self.set_binding(sym, ty.clone());

            if sym.name.text != "__BUILT_IN_IMPLEMENTATION" {
                self.diagnostics.push(Diagnostic {
                    notes: vec![],
                    fixes: vec![],
                    severity: Severity::Error,
                    message: ErrorMessage(vec![
                        msgtext!("Unbound symbol: "),
                        msgcode!("{}", sym.name),
                    ]),
                    position: sym.position.clone(),
                });
            }

            return ty;
        };

        let fun_info = match value.as_ref() {
            Value_::Fun { fun_info, .. } => Some(fun_info),
            Value_::Closure(_, fun_info, _) => Some(fun_info),
            Value_::BuiltInFunction(_, fun_info, _) => fun_info.as_ref(),
            _ => None,
        };
        if let Some(fun_info) = fun_info {
            if let Some(def_id) = fun_info.item_id {
                self.callees
                    .entry(self.current_item)
                    .or_default()
                    .insert(def_id);
            }

            if let Some(fun_sym) = &fun_info.name_sym {
                self.id_to_def_pos.insert(sym.id, fun_sym.position.clone());
            }

            if let Some(doc_comment) = &fun_info.doc_comment {
                self.id_to_doc_comment.insert(sym.id, doc_comment.clone());
            }
        }

        if matches!(
            value.as_ref(),
            Value_::EnumVariant { .. } | Value_::EnumConstructor { .. }
        ) {
            self.save_enum_variant_id(sym, value.clone());
        }

        Type::from_value(&value)
    }

    fn infer_binary_op(
        &mut self,
        pos: &Position,
        lhs: &Expression,
        op: &BinaryOperatorKind,
        rhs: &Expression,
        type_bindings: &TypeVarEnv,
        expected_return_ty: &Type,
    ) -> Type {
        match op {
            BinaryOperatorKind::Add
            | BinaryOperatorKind::Subtract
            | BinaryOperatorKind::Multiply
            | BinaryOperatorKind::Divide
            | BinaryOperatorKind::Modulo
            | BinaryOperatorKind::Exponent => {
                self.verify_expr(&Type::int(), lhs, type_bindings, expected_return_ty);
                self.verify_expr(&Type::int(), rhs, type_bindings, expected_return_ty);

                Type::int()
            }
            BinaryOperatorKind::LessThan
            | BinaryOperatorKind::LessThanOrEqual
            | BinaryOperatorKind::GreaterThan
            | BinaryOperatorKind::GreaterThanOrEqual => {
                self.verify_expr(&Type::int(), lhs, type_bindings, expected_return_ty);
                self.verify_expr(&Type::int(), rhs, type_bindings, expected_return_ty);

                Type::bool()
            }
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => {
                let lhs_ty = self.infer_expr(lhs, type_bindings, expected_return_ty);
                let rhs_ty = self.infer_expr(rhs, type_bindings, expected_return_ty);

                if !is_subtype(&lhs_ty, &rhs_ty) && !is_subtype(&rhs_ty, &lhs_ty) {
                    self.diagnostics.push(Diagnostic {
                        notes: vec![],
                        fixes: vec![],
                        severity: Severity::Warning,
                        message: ErrorMessage(vec![
                            msgtext!("You should compare values of the same type, but got "),
                            msgcode!("{}", lhs_ty),
                            msgtext!(" and "),
                            msgcode!("{}", rhs_ty),
                            msgtext!("."),
                        ]),
                        position: pos.clone(),
                    });
                }

                Type::bool()
            }
            BinaryOperatorKind::And | BinaryOperatorKind::Or => {
                self.verify_expr(&Type::bool(), lhs, type_bindings, expected_return_ty);
                self.verify_expr(&Type::bool(), rhs, type_bindings, expected_return_ty);

                Type::bool()
            }
            BinaryOperatorKind::StringConcat => {
                self.verify_expr(&Type::string(), lhs, type_bindings, expected_return_ty);
                self.verify_expr(&Type::string(), rhs, type_bindings, expected_return_ty);

                Type::string()
            }
        }
    }

    fn infer_struct_literal(
        &mut self,
        name_sym: &crate::parser::ast::TypeSymbol,
        fields: &[(Symbol, Rc<Expression>)],
        type_bindings: &TypeVarEnv,
        expected_return_ty: &Type,
    ) -> Type {
        let field_tys: Vec<(&Symbol, Position, Type)> = fields
            .iter()
            .map(|(sym, expr)| {
                let ty = self.infer_expr(expr, type_bindings, expected_return_ty);
                (sym, expr.position.clone(), ty)
            })
            .collect();

        if let Some(TypeDef::Struct(struct_info)) = self.env.get_type_def(&name_sym.name) {
            self.id_to_def_pos
                .insert(name_sym.id, struct_info.name_sym.position.clone());

            let mut ty_var_env = TypeVarEnv::default();
            for type_param in &struct_info.type_params {
                ty_var_env.insert(type_param.name.clone(), None);
            }

            let mut sym_to_expected_ty: FxHashMap<SymbolName, (Position, Type)> =
                FxHashMap::default();
            for field in &struct_info.fields {
                let ty =
                    Type::from_hint(&field.hint, &self.env.types, &ty_var_env).unwrap_or_err_ty();
                sym_to_expected_ty.insert(field.sym.name.clone(), (field.sym.position.clone(), ty));
            }

            for (sym, expr_pos, ty) in field_tys {
                let Some((field_pos, field_ty)) = sym_to_expected_ty.get(&sym.name) else {
                    continue;
                };

                self.id_to_def_pos.insert(sym.id, field_pos.clone());

                if !is_subtype(&ty, field_ty) {
                    self.diagnostics.push(Diagnostic {
                        notes: vec![],
                        fixes: vec![],
                        severity: Severity::Error,
                        message: ErrorMessage(vec![
                            msgtext!("Expected "),
                            msgcode!("{}", field_ty),
                            msgtext!(" for this field but got "),
                            msgcode!("{}", ty),
                            msgtext!("."),
                        ]),
                        position: expr_pos,
                    });
                }
            }

            Type::UserDefined {
                kind: TypeDefKind::Struct,
                name: name_sym.name.clone(),
                args: vec![],
            }
        } else {
            Type::Error {
                internal_reason: "Unbound struct name".to_owned(),
                inferred_type: None,
            }
        }
    }

    fn infer_if(
        &mut self,
        cond_expr: &Expression,
        then_block: &Block,
        else_block: &Option<Block>,
        type_bindings: &TypeVarEnv,
        expected_return_ty: &Type,
    ) -> Type {
        self.verify_expr(&Type::bool(), cond_expr, type_bindings, expected_return_ty);

        match else_block {
            Some(else_block) => {
                let then_ty = self.infer_block(then_block, type_bindings, expected_return_ty);
                let else_ty = self.infer_block(else_block, type_bindings, expected_return_ty);

                match unify(&then_ty, &else_ty) {
                    Some(ty) => ty,
                    None => {
                        let message = ErrorMessage(vec![
                            msgcode!("if"),
                            msgtext!(" and "),
                            msgcode!("else"),
                            msgtext!(" have incompatible types: "),
                            msgcode!("{}", then_ty),
                            msgtext!(" and "),
                            msgcode!("{}", else_ty),
                            msgtext!("."),
                        ]);

                        let position = match then_block.exprs.last() {
                            Some(last_expr) => last_expr.position.clone(),
                            None => cond_expr.position.clone(),
                        };

                        self.diagnostics.push(Diagnostic {
                            notes: vec![],
                            fixes: vec![],
                            severity: Severity::Error,
                            message,
                            position,
                        });

                        Type::error("Incompatible if blocks")
                    }
                }
            }
            None => {
                self.verify_block(&Type::unit(), then_block, type_bindings, expected_return_ty);
                Type::unit()
            }
        }
    }

    fn infer_namespace_access(
        &mut self,
        recv: &Expression,
        sym: &Symbol,
        type_bindings: &TypeVarEnv,
        expected_return_ty: &Type,
    ) -> Type {
        self.verify_expr(&Type::namespace(), recv, type_bindings, expected_return_ty);

        let Expression_::Variable(recv_symbol) = &recv.expr_ else {
            self.diagnostics.push(Diagnostic {
                notes: vec![],
                fixes: vec![],
                severity: Severity::Warning,
                message: ErrorMessage(vec![msgtext!(
                    "Cannot statically determine the namespace here."
                )]),
                position: recv.position.clone(),
            });

            return Type::Any;
        };

        if let Some((ty, binding_pos)) = self.bindings.get(&recv_symbol.name) {
            // The namespace receiver symbol is locally bound, e.g.
            //
            // ```
            // let x = some_ns
            // x::foo()
            // ```
            self.id_to_def_pos
                .insert(recv_symbol.id, binding_pos.clone());

            if ty.is_error() {
                return Type::error("Accessing namespace on error value");
            }

            self.diagnostics.push(Diagnostic {
                notes: vec![],
                fixes: vec![],
                severity: Severity::Warning,
                message: ErrorMessage(vec![msgtext!(
                    "Cannot statically determine the namespace here."
                )]),
                position: recv.position.clone(),
            });

            return Type::Any;
        }

        let Some(value) = self.get_var(&recv_symbol.name) else {
            return Type::error("Unbound symbol (expected a namespace)");
        };

        match value.as_ref() {
            Value_::Namespace {
                ns_info,
                imported_name_sym,
            } => {
                self.id_to_def_pos
                    .insert(recv_symbol.id, imported_name_sym.position.clone());

                let ns_info = ns_info.borrow();
                let values = &ns_info.values;

                match values.get(&sym.name) {
                    Some(value) => {
                        if let Some(fun_info) = value.fun_info() {
                            self.id_to_def_pos.insert(sym.id, fun_info.pos.clone());
                        }

                        if !ns_info.exported_syms.contains(&sym.name) {
                            let mut name_pos = None;
                            match value.as_ref() {
                                Value_::Fun { fun_info, .. }
                                | Value_::BuiltInFunction(_, Some(fun_info), _) => {
                                    if let Some(name_sym) = &fun_info.name_sym {
                                        name_pos = Some(name_sym.position.clone());
                                    }
                                }
                                _ => {}
                            }

                            let notes = match name_pos {
                                Some(name_pos) => {
                                    vec![(
                                        ErrorMessage(vec![
                                            msgcode!("{}", sym.name),
                                            msgtext!(" is defined here."),
                                        ]),
                                        name_pos,
                                    )]
                                }
                                None => vec![],
                            };

                            self.diagnostics.push(Diagnostic {
                                notes,
                                fixes: vec![],
                                severity: Severity::Error,
                                message: ErrorMessage(vec![
                                    msgcode!("{}", sym.name),
                                    msgtext!(" is not marked as "),
                                    msgcode!("external"),
                                    msgtext!(
                                        " so it cannot be used outside the file that contains it."
                                    ),
                                ]),
                                position: sym.position.clone(),
                            });
                        }

                        Type::from_value(value)
                    }
                    None => {
                        if !values.contains_key(&SymbolName {
                            text: "__PLACEHOLDER_NAMESPACE".to_owned(),
                        }) {
                            // If this is only a placeholder because the
                            // imported file didn't exist, avoid cascading
                            // errors.

                            // TODO: suggest similar names here.
                            self.diagnostics.push(Diagnostic {
                                notes: vec![],
                                fixes: vec![],
                                severity: Severity::Error,
                                message: ErrorMessage(vec![
                                    msgcode!(
                                        "{}",
                                        self.env.relative_to_project(&ns_info.abs_path).display()
                                    ),
                                    msgtext!(" does not contain an item named "),
                                    msgcode!("{}", sym.name),
                                    msgtext!("."),
                                ]),
                                position: sym.position.clone(),
                            });
                        }

                        Type::error("No such symbol in this namespace")
                    }
                }
            }
            _ => Type::error("Expected a namespace for namespace receiver"),
        }
    }

    fn get_var(&self, name: &SymbolName) -> Option<Value> {
        let ns = self.env.get_namespace(&self.path.path).unwrap();
        if let Some(v) = ns.borrow().values.get(name) {
            return Some(v.clone());
        }

        None
    }

    /// Get the type of `sym` in the current scope (checking both
    /// locals and globals).
    ///
    /// Emit a diagnostic is `sym` is unbound or cannot be reassigned.
    ///
    /// If `sym` is unbound, also insert it in the current scope, to
    /// prevent cascading errors.
    fn get_var_for_assignment(&mut self, sym: &Symbol) -> Type {
        if let Some((sym_ty, position)) = self.bindings.get(&sym.name) {
            self.id_to_def_pos.insert(sym.id, position.clone());
            return sym_ty.clone();
        }

        if let Some(value) = self.get_var(&sym.name) {
            self.diagnostics.push(Diagnostic {
                notes: vec![],
                fixes: vec![],
                severity: Severity::Error,
                message: ErrorMessage(vec![
                    msgcode!("{}", &sym.name),
                    msgtext!(" is a function definition, which cannot be reassigned."),
                ]),
                position: sym.position.clone(),
            });

            return Type::from_value(&value);
        }

        // No such variable, user probably forgot `let`.
        self.diagnostics.push(Diagnostic {
            notes: vec![],
            fixes: vec![],
            severity: Severity::Error,
            message: ErrorMessage(vec![
                msgtext!("No such variable "),
                msgcode!("{}", &sym.name),
                msgtext!(". If you want to define a new local variable, write "),
                msgcode!("let {} =", sym.name),
                msgtext!("."),
            ]),
            position: sym.position.clone(),
        });

        let ty = Type::error("Unbound variable");
        self.bindings.set(sym, ty.clone());

        ty
    }

    fn set_dest_binding(&mut self, dest: &LetDestination, expr_pos: &Position, ty: Type) {
        match dest {
            LetDestination::Symbol(symbol) => self.set_binding(symbol, ty),
            LetDestination::Destructure(symbols) => match ty {
                Type::Tuple(item_tys) => {
                    if item_tys.len() != symbols.len() {
                        self.diagnostics.push(Diagnostic {
                            notes: vec![],
                            fixes: vec![],
                            severity: Severity::Error,
                            message: ErrorMessage(vec![Text(format!(
                                "Expected a tuple of size {}, but got {}.",
                                symbols.len(),
                                item_tys.len()
                            ))]),
                            position: expr_pos.clone(),
                        });
                    }

                    for (i, symbol) in symbols.iter().enumerate() {
                        let ty = match item_tys.get(i) {
                            Some(ty) => ty.clone(),
                            None => Type::error("Tuple has too many items for the value"),
                        };
                        self.set_binding(symbol, ty);
                    }
                }
                Type::Error { .. } => {
                    for symbol in symbols {
                        self.set_binding(symbol, ty.clone());
                    }
                }
                _ => {
                    self.diagnostics.push(Diagnostic {
                        notes: vec![],
                        fixes: vec![],
                        severity: Severity::Error,
                        message: ErrorMessage(vec![
                            msgtext!("Expected a tuple, but got "),
                            msgcode!("{}", ty),
                            msgtext!("."),
                        ]),
                        position: expr_pos.clone(),
                    });

                    let ty = Type::error("Tuple destructuring with non-tuple");
                    for symbol in symbols {
                        self.set_binding(symbol, ty.clone());
                    }
                }
            },
        }
    }

    /// Verify that `expr` has a type that is a subtype of
    /// `expected_ty`. Return the inferred type.
    ///
    /// These two types may differ: for example, `[]` has an inferred
    /// type of `List<NoValue>`, which is a subtype of `List<Int>`.
    ///
    /// For function literals, this allows more expressions to be
    /// checked. Inference cannot handle `fun(x) { x.foo }` but
    /// verifying/checking can because we know the expected lambda
    /// type.
    ///
    /// When in doubt, prefer adding more cases to `infer_expr`.
    fn verify_expr(
        &mut self,
        expected_ty: &Type,
        expr: &Expression,
        type_bindings: &TypeVarEnv,
        expected_return_ty: &Type,
    ) -> Type {
        let inferred_ty = self.verify_expr_(
            expected_ty,
            &expr.expr_,
            &expr.position,
            expr.id,
            type_bindings,
            expected_return_ty,
        );

        self.id_to_ty.insert(expr.id, inferred_ty.clone());
        inferred_ty
    }

    fn verify_expr_(
        &mut self,
        expected_ty: &Type,
        expr_: &Expression_,
        pos: &Position,
        expr_id: SyntaxId,
        type_bindings: &TypeVarEnv,
        expected_return_ty: &Type,
    ) -> Type {
        let mut ty = match (expr_, expected_ty) {
            (
                Expression_::FunLiteral(fun_info),
                Type::Fun {
                    name_sym: _,
                    type_params: _,
                    params: expected_params,
                    return_: expected_return_ty,
                },
            ) => {
                self.bindings.enter_block();

                let mut param_tys = vec![];
                for (i, param) in fun_info.params.params.iter().enumerate() {
                    let param_ty = match &param.hint {
                        Some(hint) => {
                            let hint_ty = Type::from_hint(hint, &self.env.types, type_bindings)
                                .unwrap_or_err_ty();
                            self.save_hint_ty_id(hint, &hint_ty);
                            hint_ty
                        }
                        None => expected_params.get(i).cloned().unwrap_or(Type::Any),
                    };

                    self.set_binding(&param.symbol, param_ty.clone());
                    param_tys.push(param_ty);
                }

                let block_ty = self.verify_block(
                    expected_return_ty,
                    &fun_info.body,
                    type_bindings,
                    expected_return_ty,
                );

                if let Some(hint) = &fun_info.return_hint {
                    let hint_ty =
                        Type::from_hint(hint, &self.env.types, type_bindings).unwrap_or(Type::Any);
                    self.save_hint_ty_id(hint, &hint_ty);

                    if !is_subtype(&block_ty, &hint_ty) {
                        self.diagnostics.push(Diagnostic {
                            notes: vec![],
                            fixes: vec![],
                            severity: Severity::Error,
                            message: ErrorMessage(vec![
                                msgtext!("Expected a function with return type "),
                                msgcode!("{}", hint_ty),
                                msgtext!(" but got "),
                                msgcode!("{}", block_ty),
                                msgtext!("."),
                            ]),
                            position: pos.clone(),
                        });
                    }
                }

                self.bindings.exit_block();

                Type::Fun {
                    type_params: vec![],
                    params: param_tys,
                    return_: Box::new(block_ty),
                    name_sym: None,
                }
            }
            (
                Expression_::ListLiteral(items),
                Type::UserDefined {
                    kind: TypeDefKind::Struct,
                    name,
                    args,
                },
            ) if name.text == "List" && args.len() == 1 => {
                let expected_elem_ty = &args[0];

                // Even if we verify, we should return the more
                // specific type. E.g. if we're verifying List<Any>
                // and we infer List<Int>.

                let mut item_tys = vec![];
                for item in items {
                    let item_ty = self.verify_expr(
                        expected_elem_ty,
                        &item.expr,
                        type_bindings,
                        expected_return_ty,
                    );
                    item_tys.push((item_ty, item.expr.position.clone()));
                }

                let elem_ty = unify_all(&item_tys).unwrap_or(Type::error("Could not unify list"));
                Type::list(elem_ty)
            }
            (Expression_::Match(scrutinee, cases), _) => self.verify_match(
                expected_ty,
                type_bindings,
                expected_return_ty,
                expr_id,
                scrutinee,
                cases,
            ),
            (Expression_::If(cond_expr, then_block, else_block), _) => {
                self.verify_expr(&Type::bool(), cond_expr, type_bindings, expected_return_ty);

                match else_block {
                    Some(else_block) => {
                        self.verify_block(
                            expected_ty,
                            then_block,
                            type_bindings,
                            expected_return_ty,
                        );
                        self.verify_block(
                            expected_ty,
                            else_block,
                            type_bindings,
                            expected_return_ty,
                        );
                        expected_ty.clone()
                    }
                    None => {
                        self.verify_block(
                            &Type::unit(),
                            then_block,
                            type_bindings,
                            expected_return_ty,
                        );
                        Type::unit()
                    }
                }
            }
            _ => self.infer_expr_(expr_, pos, expr_id, type_bindings, expected_return_ty),
        };

        if !is_subtype(&ty, expected_ty) {
            self.diagnostics.push(Diagnostic {
                notes: vec![],
                fixes: vec![],
                severity: Severity::Error,
                message: format_type_mismatch(expected_ty, &ty),
                position: pos.clone(),
            });

            ty = Type::Error {
                internal_reason: "Type mismatch".to_owned(),
                inferred_type: Some(Box::new(ty)),
            };
        }

        ty
    }

    fn set_binding(&mut self, symbol: &Symbol, ty: Type) {
        self.bindings.set(symbol, ty.clone());
        self.id_to_ty.insert(symbol.id, ty);
        self.id_to_def_pos
            .insert(symbol.id, symbol.position.clone());
    }
}

fn enum_payload_type(env: &Env, scrutinee_ty: &Type, variant_sym: &Symbol) -> Type {
    let Some(scrutinee_ty_name) = scrutinee_ty.type_name() else {
        return Type::error(
            "No type name for match scrutinee, we should have errored elsewhere already.",
        );
    };

    let Some(type_def) = env.get_type_def(&scrutinee_ty_name) else {
        return Type::error("No type definition found with this type name.");
    };

    let TypeDef::Enum(enum_info) = type_def else {
        return Type::error("Matching on a type that isn't an enum.");
    };

    let mut relevant_variant = None;
    for variant in &enum_info.variants {
        if variant.name_sym.name == variant_sym.name {
            relevant_variant = Some(variant.clone());
        }
    }

    let Some(variant) = relevant_variant else {
        return Type::error(format!(
            "No variant found in `{}` named `{}`.",
            scrutinee_ty_name, variant_sym.name
        ));
    };

    let Some(payload_hint) = variant.payload_hint else {
        return Type::error("This enum variant does not have a payload.");
    };

    // If this is a variant like `Some(T)`, find the value for this
    // generic parameter.
    let Type::UserDefined { args, .. } = scrutinee_ty else {
        return Type::error("Match scrutinee value is not an enum.");
    };

    // If the payload is a generic type from the enum definition, use
    // the value for that generic from the value.
    for (type_def_param, value_type_param) in type_def.params().iter().zip(args) {
        if payload_hint.sym.name == type_def_param.name {
            return value_type_param.clone();
        }
    }

    // The payload is not a generic type, so the type hint is
    // referring to a defined type.
    Type::from_hint(&payload_hint, &env.types, &FxHashMap::default()).unwrap_or_err_ty()
}

/// Solve the type variables in this method, and return the resolved
/// type of the return type hint.
fn subst_type_vars_in_meth_return_ty(
    env: &Env,
    method_info: &MethodInfo,
    receiver_pos: &Position,
    receiver_ty: &Type,
    arg_tys: &[(Type, Position, Option<Position>)],
    ty_var_env: &mut TypeVarEnv,
) -> (Vec<Diagnostic>, Type) {
    let mut diagnostics = vec![];
    let Some(fun_info) = method_info.fun_info() else {
        return (diagnostics, Type::error("This method has no fun_info"));
    };

    if let Err(diagnostic) = unify_and_solve_hint(
        env,
        &method_info.receiver_hint,
        receiver_pos,
        receiver_ty,
        ty_var_env,
    ) {
        diagnostics.push(diagnostic);
    }

    subst_type_vars_in_fun_info_return_ty(env, fun_info, arg_tys, ty_var_env)
}

/// Solve the type variables in this function, and return the resolved
/// type of the return type hint.
fn subst_type_vars_in_fun_info_return_ty(
    env: &Env,
    fun_info: &FunInfo,
    arg_tys: &[(Type, Position, Option<Position>)],
    ty_var_env: &mut TypeVarEnv,
) -> (Vec<Diagnostic>, Type) {
    let mut diagnostics = vec![];

    for ((arg_ty, arg_pos, _), param) in arg_tys.iter().zip(&fun_info.params.params) {
        if let Some(param_hint) = &param.hint {
            if let Err(diagnostic) =
                unify_and_solve_hint(env, param_hint, arg_pos, arg_ty, ty_var_env)
            {
                diagnostics.push(diagnostic);
            }
        }
    }

    let ret_ty = match &fun_info.return_hint {
        Some(return_hint) => {
            let hint_name = &return_hint.sym.name;
            if let Some(ty_var_val) = ty_var_env.get(hint_name) {
                match ty_var_val {
                    Some(ty) => ty.clone(),
                    None => {
                        // This type variable was never used, other
                        // than the return position. Solve to bottom.
                        Type::no_value()
                    }
                }
            } else {
                Type::from_hint(return_hint, &env.types, ty_var_env).unwrap_or_err_ty()
            }
        }
        None => Type::Any,
    };

    (diagnostics, ret_ty)
}

/// Substitute type variables in `ty` with the bindings in
/// `ty_var_env`.
///
/// For example, if we originally saw `List<T>` but we know in this
/// context the value of `T` is `Int`, we want `List<Int>`.
fn substitute_ty_vars(ty: &Type, ty_var_env: &TypeVarEnv) -> Type {
    match ty {
        Type::Error { .. } | Type::Any => ty.clone(),
        Type::Tuple(elem_tys) => Type::Tuple(
            elem_tys
                .iter()
                .map(|ty| substitute_ty_vars(ty, ty_var_env))
                .collect(),
        ),
        Type::Fun {
            type_params,
            params,
            return_,
            name_sym,
        } => {
            let params = params
                .iter()
                .map(|p| substitute_ty_vars(p, ty_var_env))
                .collect();
            let return_ = substitute_ty_vars(return_, ty_var_env);

            Type::Fun {
                type_params: type_params.clone(),
                params,
                return_: Box::new(return_),
                name_sym: name_sym.clone(),
            }
        }
        Type::UserDefined { kind, name, args } => Type::UserDefined {
            kind: kind.clone(),
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| substitute_ty_vars(arg, ty_var_env))
                .collect(),
        },
        Type::TypeParameter(name) => {
            match ty_var_env.get(name) {
                Some(ty_var_val) => ty_var_val.as_ref().cloned().unwrap_or(Type::no_value()),
                None => ty.clone(), // TODO: define an error type
            }
        }
    }
}

/// Solve type parameters in `decl_ty` (a type in a declaration
/// position, e.g. `List<T>`) so that the type matches the fully
/// solved type (e.g. `List<Int>`).
fn unify_and_solve_ty(decl_ty: &Type, solved_ty: &Type, ty_var_env: &mut TypeVarEnv) {
    match (decl_ty, solved_ty) {
        (Type::TypeParameter(n), _) => {
            ty_var_env.insert(n.clone(), Some(solved_ty.clone()));
        }
        (
            Type::Fun {
                params: decl_params,
                return_: decl_return,
                ..
            },
            Type::Fun {
                params: solved_params,
                return_: solved_return,
                ..
            },
        ) => {
            unify_and_solve_ty(decl_return, solved_return, ty_var_env);
            for (decl_param, solved_param) in decl_params.iter().zip(solved_params.iter()) {
                unify_and_solve_ty(decl_param, solved_param, ty_var_env);
            }
        }
        (
            Type::UserDefined {
                kind: decl_kind,
                name: decl_name,
                args: decl_args,
            },
            Type::UserDefined {
                kind: solved_kind,
                name: solved_name,
                args: solved_args,
            },
        ) if decl_kind == solved_kind && decl_name.text == solved_name.text => {
            for (decl_arg, solved_arg) in decl_args.iter().zip(solved_args.iter()) {
                unify_and_solve_ty(decl_arg, solved_arg, ty_var_env);
            }
        }
        _ => {}
    }
}

/// If `hint` is `Option<T>` and `ty` is the type representation of
/// `Option<Int>`, insert `T = Int` into `ty_var_env`.
fn unify_and_solve_hint(
    env: &Env,
    hint: &TypeHint,
    position: &Position,
    ty: &Type,
    ty_var_env: &mut TypeVarEnv,
) -> Result<(), Diagnostic> {
    let hint_name = &hint.sym.name;
    if let Some(ty_var_val) = ty_var_env.get(hint_name) {
        // If the type named in this hint is a generic type, we're done.
        match ty_var_val {
            Some(bound_ty) => {
                // We've already solved this type variable. Confirm
                // that this occurrence of the type variable has the
                // same type.
                if !is_subtype(ty, bound_ty) {
                    return Err(Diagnostic {
                        message: ErrorMessage(vec![
                            msgtext!("Expected "),
                            msgcode!("{}", bound_ty),
                            msgtext!(" (because "),
                            msgcode!("{}", hint.as_src()),
                            msgtext!(" is "),
                            msgcode!("{}", bound_ty),
                            msgtext!("), but got "),
                            msgcode!("{}", ty),
                            msgtext!("."),
                        ]),
                        position: position.clone(),
                        notes: vec![],
                        fixes: vec![],
                        severity: Severity::Warning,
                    });
                }
            }
            None => {
                // First occurrence of this type variable, assume it
                // has the type we're seeing here.
                //
                // TODO: Accumulate constraints and solve later, so we
                // can handle multiple occurrences where some are
                // NoValue.
                ty_var_env.insert(hint_name.clone(), Some(ty.clone()));
                return Ok(());
            }
        }
    }

    if hint_name.text == "Fun" && hint.args.len() == 2 {
        if let Type::Fun {
            params, return_, ..
        } = ty
        {
            // TODO: define a syntax for functions with 0 or >1 arguments.
            // TODO: this stops us typechecking invalid arity on function values passed as arguments.
            if params.len() != 1 {
                return Ok(());
            }

            // Unify the parameter type.
            unify_and_solve_hint(env, &hint.args[0], position, &params[0], ty_var_env)?;
            // Unify the return type.
            unify_and_solve_hint(env, &hint.args[1], position, return_, ty_var_env)?;
        }

        return Ok(());
    }

    let Some(type_def) = env.get_type_def(hint_name) else {
        // This hint isn't defined, and we check that elsewhere, so give up unifying.
        return Ok(());
    };

    if hint.args.len() != type_def.params().len() {
        // The hint has the wrong number of type arguments for this
        // type, which is checked elsewhere, so give up unifying.
        return Ok(());
    }

    match ty {
        Type::UserDefined {
            name: name_sym,
            args,
            ..
        } if name_sym.text == hint_name.text => {
            // TODO: stop assuming that all types are covariant.
            for (hint_arg, arg) in hint.args.iter().zip(args) {
                unify_and_solve_hint(env, hint_arg, position, arg, ty_var_env)?;
            }
        }
        _ => {
            // This value is unrelated to this hint, no solving to do.
        }
    }

    Ok(())
}

/// Unify all the types given, to a single type, if we can find a
/// compatible type.
///
/// If we can't find a compatible type, return the position of the
/// first type that didn't unify.
fn unify_all(tys: &[(Type, Position)]) -> Result<Type, Position> {
    let mut unified_ty = Type::no_value();

    // Unify the types pairwise.
    for (ty, position) in tys {
        let Some(new_unified_ty) = unify(&unified_ty, ty) else {
            return Err(position.clone());
        };
        unified_ty = new_unified_ty;
    }

    Ok(unified_ty)
}

/// If these two types are compatible, return the most general
/// compatible type.
///
/// ```gdn
/// (Int, NoValue) -> Int
/// (NoValue, Int) -> Int
/// (List<Int>, List<NoValue>) -> List<Int>
/// (String, Top) -> Top
/// (Int, String) -> return None
/// ```
fn unify(ty_1: &Type, ty_2: &Type) -> Option<Type> {
    if matches!(ty_1, Type::Any) || matches!(ty_2, Type::Any) {
        return Some(Type::Any);
    }

    if ty_1.is_no_value() || ty_1.is_error() {
        return Some(ty_2.clone());
    }
    if ty_2.is_no_value() || ty_2.is_error() {
        return Some(ty_1.clone());
    }
    if ty_1 == ty_2 {
        return Some(ty_1.clone());
    }

    match (ty_1, ty_2) {
        (
            Type::UserDefined {
                kind: kind_1,
                name: name_1,
                args: args_1,
            },
            Type::UserDefined {
                kind: kind_2,
                name: name_2,
                args: args_2,
            },
        ) => {
            if kind_1 != kind_2 || name_1.text != name_2.text || args_1.len() != args_2.len() {
                return None;
            }

            let mut unified_args = vec![];
            for (arg_1, arg_2) in args_1.iter().zip(args_2) {
                unified_args.push(unify(arg_1, arg_2)?);
            }

            Some(Type::UserDefined {
                kind: kind_1.clone(),
                name: name_1.clone(),
                args: unified_args,
            })
        }
        _ => {
            // TODO: functions are generic types and should be handled
            // similar to lists.
            None
        }
    }
}

fn check_match_exhaustive(
    env: &Env,
    scrutinee_pos: &Position,
    type_name: &TypeName,
    patterns: &[Pattern],
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(type_def) = env.get_type_def(type_name) else {
        return;
    };
    let TypeDef::Enum(enum_info) = type_def else {
        return;
    };

    let mut variants_remaining: FxHashMap<SymbolName, VariantInfo> = FxHashMap::default();
    for variant in &enum_info.variants {
        variants_remaining.insert(variant.name_sym.name.clone(), variant.clone());
    }

    if !enum_info.variants.is_empty() {
        let mut seen_underscore = false;
        for pattern in patterns {
            if pattern.variant_sym.name.is_underscore() {
                seen_underscore = true;
                continue;
            }

            match variants_remaining.remove(&pattern.variant_sym.name) {
                Some(_) => {
                    // First time we've seen this variant.
                }
                None => {
                    diagnostics.push(Diagnostic {
                        notes: vec![],
                        fixes: vec![],
                        severity: Severity::Error,
                        message: ErrorMessage(vec![Text(
                            "Duplicate case in pattern match.".to_owned(),
                        )]),
                        position: pattern.variant_sym.position.clone(),
                    });
                }
            }
        }

        // If any cases are _, this match is exhaustive.
        if seen_underscore {
            return;
        }
    }

    // If we're missing any variants, complain about the first one in
    // source code order.
    //
    // TODO: mention the other variants missing and/or the total.
    for variant in &enum_info.variants {
        if variants_remaining.contains_key(&variant.name_sym.name) {
            diagnostics.push(Diagnostic {
                notes: vec![],
                fixes: vec![],
                severity: Severity::Error,
                message: ErrorMessage(vec![
                    msgtext!("This match expression does not cover all the cases of ",),
                    msgcode!("{}", type_name),
                    msgtext!(". It's missing ",),
                    msgcode!("{}", variant.name_sym.name.text),
                    msgtext!("."),
                ]),
                position: scrutinee_pos.clone(),
            });
            break;
        }
    }
}

fn format_type_mismatch(expected_ty: &Type, actual_ty: &Type) -> ErrorMessage {
    ErrorMessage(vec![
        Text("Expected ".to_owned()),
        Code(expected_ty.to_string()),
        Text(", but got ".to_owned()),
        Code(actual_ty.to_string()),
        Text(".".to_owned()),
    ])
}
