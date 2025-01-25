use std::collections::HashSet;

use garden_lang_parser::ast::{
    BinaryOperatorKind, Block, EnumInfo, Expression, Expression_, FunInfo, LetDestination,
    MethodInfo, Pattern, StructInfo, Symbol, SymbolName, SyntaxId, TestInfo, ToplevelItem,
    ToplevelItemId, ToplevelItem_, TypeHint, TypeName, VariantInfo,
};
use garden_lang_parser::position::Position;
use garden_lang_parser::visitor::Visitor;
use rustc_hash::FxHashMap;

use crate::diagnostics::{Diagnostic, Level};
use crate::env::Env;
use crate::eval::most_similar;
use crate::garden_type::{is_subtype, Type, TypeDefKind, TypeVarEnv, UnwrapOrErrTy as _};
use crate::types::TypeDef;
use crate::values::Value;

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
}

pub(crate) fn check_types(items: &[ToplevelItem], env: &Env) -> TCSummary {
    let mut visitor = TypeCheckVisitor {
        env,
        diagnostics: vec![],
        bindings: LocalBindings::default(),
        id_to_ty: FxHashMap::default(),
        id_to_doc_comment: FxHashMap::default(),
        id_to_def_pos: FxHashMap::default(),
        callees: FxHashMap::default(),
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
    }
}

#[derive(Debug)]
struct LocalBindings {
    blocks: Vec<FxHashMap<SymbolName, (Type, Position)>>,
}

impl Default for LocalBindings {
    fn default() -> Self {
        Self {
            blocks: vec![FxHashMap::default()],
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
}

#[derive(Debug)]
struct TypeCheckVisitor<'a> {
    env: &'a Env,
    diagnostics: Vec<Diagnostic>,
    bindings: LocalBindings,
    id_to_ty: FxHashMap<SyntaxId, Type>,
    id_to_doc_comment: FxHashMap<SyntaxId, String>,
    id_to_def_pos: FxHashMap<SyntaxId, Position>,
    current_item: Option<ToplevelItemId>,
    callees: FxHashMap<Option<ToplevelItemId>, HashSet<ToplevelItemId>>,
}

impl Visitor for TypeCheckVisitor<'_> {
    fn visit_toplevel_item(&mut self, item: &ToplevelItem) {
        if let ToplevelItem_::Import(info) = &item.2 {
            self.id_to_def_pos.insert(
                info.id,
                Position {
                    start_offset: 0,
                    end_offset: 0,
                    line_number: 0,
                    end_line_number: 0,
                    column: 0,
                    path: info.path.clone(),
                },
            );
        }

        self.visit_item_(&item.2);
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

        let mut type_bindings = self.env.stack.type_bindings();
        if let Some(fun_info) = method_info.fun_info() {
            for type_param in &fun_info.type_params {
                type_bindings.insert(type_param.name.clone(), None);
            }
        }

        let type_hint = &method_info.receiver_hint;
        if type_bindings.contains_key(&type_hint.sym.name) {
            self.diagnostics.push(Diagnostic {
                level: Level::Error,
                message:
                    "Methods must be defined on specific types, such as `List`, not generic types."
                        .to_owned(),
                position: type_hint.position.clone(),
            });
        }

        let mut generic_args_seen = HashSet::new();
        for type_arg in &type_hint.args {
            if type_bindings.contains_key(&type_arg.sym.name) {
                if generic_args_seen.contains(&type_arg.sym.name) {
                    self.diagnostics.push(Diagnostic {
                        level: Level::Error,
                        message: "Methods cannot repeat generic type parameters.".to_owned(),
                        position: type_arg.position.clone(),
                    });
                } else {
                    generic_args_seen.insert(type_arg.sym.name.clone());
                }
            } else {
                self.diagnostics.push(Diagnostic {
                    level: Level::Error,
                    message:
                      "Methods must be generic in all their type parameters, e.g. `List<T>` not `List<Int>`."
                        .to_owned(),
                    position: type_arg.position.clone(),
                });
            }
        }

        let self_ty = Type::from_hint(&method_info.receiver_hint, &self.env.types, &type_bindings)
            .unwrap_or_err_ty();
        self.bindings.set(&method_info.receiver_sym, self_ty);

        // TODO: generic variables are bound here.

        self.visit_method_info_default(method_info);

        self.bindings.exit_block();
    }

    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        if std::env::var_os("GDN_TRUST_PRELUDE").is_some() {
            // Skip typechecking builtins and prelude to help print debugging.
            if let Some(n) = &fun_info.name_sym {
                let path = n.position.path.clone();
                if path.display().to_string().ends_with("prelude.gdn") {
                    return;
                }
                if path.display().to_string().ends_with("builtins.gdn") {
                    return;
                }
            }
        }

        let old_def_id = self.current_item;
        if let Some(def_id) = fun_info.item_id {
            self.current_item = Some(def_id);
        }

        let mut type_bindings = self.env.stack.type_bindings();
        for type_param in &fun_info.type_params {
            type_bindings.insert(type_param.name.clone(), None);
        }

        self.bindings.enter_block();

        for param in &fun_info.params {
            let param_ty = match &param.hint {
                Some(hint) => {
                    let hint_ty =
                        Type::from_hint(hint, &self.env.types, &type_bindings).unwrap_or_err_ty();
                    self.save_hint_ty_id(hint, &hint_ty);
                    hint_ty
                }
                None => Type::Top,
            };

            self.set_binding(&param.symbol, param_ty);
        }

        let return_ty = match &fun_info.return_hint {
            Some(hint) => {
                let hint_ty =
                    Type::from_hint(hint, &self.env.types, &type_bindings).unwrap_or_err_ty();
                self.save_hint_ty_id(hint, &hint_ty);

                Some(hint_ty)
            }
            None => None,
        };

        self.verify_block(
            &return_ty.clone().unwrap_or(Type::Top),
            &fun_info.body,
            &type_bindings,
            return_ty.as_ref(),
        );

        self.bindings.exit_block();

        self.current_item = old_def_id;
    }

    fn visit_struct_info(&mut self, struct_info: &StructInfo) {
        self.bindings.enter_block();

        let mut type_bindings = self.env.stack.type_bindings();
        for type_param in &struct_info.type_params {
            type_bindings.insert(type_param.name.clone(), None);
        }

        for field in &struct_info.fields {
            let field_ty =
                Type::from_hint(&field.hint, &self.env.types, &type_bindings).unwrap_or_err_ty();
            self.save_hint_ty_id(&field.hint, &field_ty);
        }

        self.bindings.exit_block();

        self.visit_struct_info_default(struct_info);
    }

    fn visit_enum_info(&mut self, enum_info: &EnumInfo) {
        self.bindings.enter_block();

        let mut type_bindings = self.env.stack.type_bindings();
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

        self.visit_enum_info_default(enum_info);
    }

    fn visit_block(&mut self, block: &Block) {
        // infer_block recurses, so don't recurse in the visitor.
        self.infer_block(block, &self.env.stack.type_bindings(), None);
    }
}

impl TypeCheckVisitor<'_> {
    /// Update `id_to_pos` for this occurrence of an enum variant,
    /// e.g. an instance of the literal `True`.
    fn save_enum_variant_id(&mut self, occurrence_sym: &Symbol, value: &Value) {
        let type_name = match value {
            Value::EnumVariant { type_name, .. } | Value::EnumConstructor { type_name, .. } => {
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
    fn save_hint_ty_id(&mut self, hint: &TypeHint, param_ty: &Type) {
        // Recurse on hint arguments.
        let param_ty_args: Vec<Type> = match param_ty {
            Type::Tuple(args) => args.clone(),
            Type::Fun {
                params, return_, ..
            } => vec![Type::Tuple(params.clone()), *return_.clone()],
            Type::UserDefined { args, .. } => args.clone(),
            _ => vec![],
        };
        for (param_ty_arg, hint_arg) in param_ty_args.iter().zip(&hint.args) {
            self.save_hint_ty_id(hint_arg, param_ty_arg)
        }

        let ty_name: TypeName = match &param_ty {
            Type::UserDefined { name: name_sym, .. } => name_sym.clone(),
            _ => {
                return;
            }
        };

        if let Some(type_def) = self.env.get_type_def(&ty_name) {
            let def_name_sym = match type_def {
                TypeDef::Builtin(_, Some(struct_info)) => &struct_info.name_sym,
                TypeDef::Builtin(_, None) => {
                    return;
                }
                TypeDef::Enum(enum_info) => &enum_info.name_sym,
                TypeDef::Struct(struct_info) => &struct_info.name_sym,
            };

            let hint_id = hint.sym.id;
            self.id_to_def_pos
                .insert(hint_id, def_name_sym.position.clone());
        }
    }

    /// Run type inference on this block, and return the type of the
    /// last expression.
    fn infer_block(
        &mut self,
        block: &Block,
        type_bindings: &TypeVarEnv,
        expected_return_ty: Option<&Type>,
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
        expected_return_ty: Option<&Type>,
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

        self.bindings.exit_block();
        ty
    }

    fn infer_expr(
        &mut self,
        expr: &Expression,
        type_bindings: &TypeVarEnv,
        expected_return_ty: Option<&Type>,
    ) -> Type {
        let ty = self.infer_expr_(
            &expr.expr_,
            &expr.position,
            type_bindings,
            expected_return_ty,
        );
        self.id_to_ty.insert(expr.id, ty.clone());

        ty
    }

    fn infer_expr_(
        &mut self,
        expr_: &Expression_,
        pos: &Position,
        type_bindings: &TypeVarEnv,
        expected_return_ty: Option<&Type>,
    ) -> Type {
        match expr_ {
            Expression_::Match(scrutinee, cases) => {
                let scrutinee_ty = self.infer_expr(scrutinee, type_bindings, expected_return_ty);
                let scrutinee_ty_name = scrutinee_ty.type_name();

                let mut case_tys = vec![];

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

                for (pattern, case_expr) in cases {
                    self.bindings.enter_block();

                    if let Some(payload_sym) = &pattern.argument {
                        if !payload_sym.name.is_underscore() {
                            self.set_binding(
                                payload_sym,
                                enum_payload_type(self.env, &scrutinee_ty, &pattern.symbol),
                            );
                        }
                    }

                    case_tys.push((
                        self.infer_block(case_expr, type_bindings, expected_return_ty),
                        Position::merge(&case_expr.open_brace, &case_expr.close_brace),
                    ));

                    self.bindings.exit_block();

                    // Matching `_` works for any type.
                    if pattern.symbol.name.is_underscore() {
                        continue;
                    }

                    let Some(value) = self.env.file_scope.get(&pattern.symbol.name) else {
                        self.diagnostics.push(Diagnostic {
                            level: Level::Error,
                            message: format!("No such type `{}`.", pattern.symbol.name),
                            position: pattern.symbol.position.clone(),
                        });
                        continue;
                    };

                    let pattern_type_name = match value {
                        Value::EnumVariant { type_name, .. } => type_name,
                        Value::EnumConstructor { type_name, .. } => type_name,
                        _ => {
                            self.diagnostics.push(Diagnostic {
                                level: Level::Error,
                                message: format!(
                                    "Expected an enum variant here, but got `{}`.",
                                    value.display(self.env)
                                ),
                                position: pattern.symbol.position.clone(),
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
                            level: Level::Error,
                            message: format!(
                                "This match case is for `{}`, but you're matching on a `{}`.",
                                pattern_type_name, &scrutinee_ty_name,
                            ),
                            position: pattern.symbol.position.clone(),
                        });
                    }
                }

                match unify_all(&case_tys) {
                    Ok(ty) => ty,
                    Err(position) => {
                        self.diagnostics.push(Diagnostic {
                            level: Level::Error,
                            message: "`match` cases have different types.".to_owned(),
                            position,
                        });

                        Type::error("Cases had incompatible types.")
                    }
                }
            }
            Expression_::If(cond_expr, then_block, else_block) => {
                self.verify_expr(&Type::bool(), cond_expr, type_bindings, expected_return_ty);

                match else_block {
                    Some(else_block) => {
                        let then_ty =
                            self.infer_block(then_block, type_bindings, expected_return_ty);
                        let else_ty =
                            self.infer_block(else_block, type_bindings, expected_return_ty);

                        match unify(&then_ty, &else_ty) {
                            Some(ty) => ty,
                            None => {
                                let message = format!(
                                    "`if` and `else` have incompatible types: `{}` and `{}`.",
                                    then_ty, else_ty
                                );

                                let position = match then_block.exprs.last() {
                                    Some(last_expr) => last_expr.position.clone(),
                                    None => cond_expr.position.clone(),
                                };

                                self.diagnostics.push(Diagnostic {
                                    level: Level::Error,
                                    message,
                                    position,
                                });

                                Type::error("Incompatible if blocks")
                            }
                        }
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
            Expression_::While(cond_expr, block) => {
                self.verify_expr(&Type::bool(), cond_expr, type_bindings, expected_return_ty);
                self.infer_block(block, type_bindings, expected_return_ty);
                Type::unit()
            }
            Expression_::ForIn(sym, expr, body) => {
                let expr_ty = self.infer_expr(expr, type_bindings, expected_return_ty);

                self.bindings.enter_block();

                let elem_ty = match expr_ty {
                    Type::UserDefined { name, args, .. } if name.name == "List" => {
                        if let Some(arg) = args.first() {
                            arg.clone()
                        } else {
                            Type::error("Bad list arity")
                        }
                    }
                    _ => {
                        self.diagnostics.push(Diagnostic {
                            level: Level::Error,
                            message: format!(
                                "Expected `List` for a `for` loop, but got `{}`.",
                                expr_ty
                            ),
                            position: expr.position.clone(),
                        });
                        Type::error("For loop expression that isn't a list")
                    }
                };

                self.set_binding(sym, elem_ty);

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
                let expected_ty = match self.bindings.get(&sym.name) {
                    Some((sym_ty, position)) => {
                        self.id_to_def_pos.insert(sym.id, position.clone());
                        sym_ty.clone()
                    }
                    None => Type::Top,
                };

                self.verify_expr(&expected_ty, expr, type_bindings, expected_return_ty);
                Type::unit()
            }
            Expression_::AssignUpdate(sym, op, expr) => {
                // TODO: also enforce the type of an assignment at runtime.

                let sym_ty = match self.bindings.get(&sym.name) {
                    Some((sym_ty, position)) => {
                        self.id_to_def_pos.insert(sym.id, position.clone());
                        sym_ty.clone()
                    }
                    None => Type::Top,
                };

                if !is_subtype(&sym_ty, &Type::int()) {
                    self.diagnostics.push(Diagnostic {
                        level: Level::Error,
                        message: format!(
                            "`{}` can only be used with `Int` variables, but got `{}`.",
                            op.as_src(),
                            sym_ty
                        ),
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

                match dest {
                    LetDestination::Symbol(symbol) => self.set_binding(symbol, ty),
                    LetDestination::Destructure(symbols) => match ty {
                        Type::Tuple(item_tys) => {
                            if item_tys.len() != symbols.len() {
                                self.diagnostics.push(Diagnostic {
                                    level: Level::Error,
                                    message: format!(
                                        "Expected a tuple of size {}, but got {}.",
                                        symbols.len(),
                                        item_tys.len()
                                    ),
                                    position: expr.position.clone(),
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
                        Type::Error(_) => {
                            for symbol in symbols {
                                self.set_binding(symbol, ty.clone());
                            }
                        }
                        _ => {
                            self.diagnostics.push(Diagnostic {
                                level: Level::Error,
                                message: format!("Expected a tuple, but got `{}`.", ty),
                                position: expr.position.clone(),
                            });
                        }
                    },
                }

                Type::unit()
            }
            Expression_::Return(inner_expr) => {
                let expr_ty = expected_return_ty.unwrap_or(&Type::Top);
                match inner_expr {
                    Some(expr) => {
                        self.verify_expr(expr_ty, expr, type_bindings, expected_return_ty);
                    }
                    None => {
                        if !is_subtype(&Type::unit(), expr_ty) {
                            self.diagnostics.push(Diagnostic {
                                level: Level::Error,
                                message: format!(
                                    "Expected this function to return `{}`, but got `Unit`.",
                                    expr_ty,
                                ),
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
                            self.infer_expr(item, type_bindings, expected_return_ty),
                            item.position.clone(),
                        )
                    })
                    .collect::<Vec<_>>();

                let elem_ty = match unify_all(&item_tys) {
                    Ok(ty) => ty,
                    Err(position) => {
                        self.diagnostics.push(Diagnostic {
                            level: Level::Error,
                            message: "List elements have different types.".to_owned(),
                            position,
                        });
                        Type::error("List elements have different types")
                    }
                };

                Type::list(elem_ty)
            }
            Expression_::TupleLiteral(items) => {
                let item_tys: Vec<_> = items
                    .iter()
                    .map(|item| self.infer_expr(item, type_bindings, expected_return_ty))
                    .collect();
                Type::Tuple(item_tys)
            }
            Expression_::StructLiteral(name_sym, fields) => {
                let field_tys: Vec<_> = fields
                    .iter()
                    .map(|(sym, expr)| {
                        let ty = self.infer_expr(expr, type_bindings, expected_return_ty);
                        (sym, expr.position.clone(), ty)
                    })
                    .collect();

                if let Some(TypeDef::Struct(struct_info)) = self.env.get_type_def(&name_sym.name) {
                    let mut ty_var_env = TypeVarEnv::default();
                    for type_param in &struct_info.type_params {
                        ty_var_env.insert(type_param.name.clone(), None);
                    }

                    let mut sym_to_expected_ty = FxHashMap::default();
                    for field in &struct_info.fields {
                        let ty = Type::from_hint(&field.hint, &self.env.types, &ty_var_env)
                            .unwrap_or_err_ty();
                        sym_to_expected_ty.insert(field.sym.name.clone(), ty);
                    }

                    for (sym, expr_pos, ty) in field_tys {
                        let Some(field_ty) = sym_to_expected_ty.get(&sym.name) else {
                            continue;
                        };

                        if !is_subtype(&ty, field_ty) {
                            self.diagnostics.push(Diagnostic {
                                level: Level::Error,
                                message: format!(
                                    "Expected `{}` for this field but got `{}`.",
                                    field_ty, ty
                                ),
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
                    Type::Error("Unbound struct name".to_owned())
                }
            }
            Expression_::BinaryOperator(lhs, op, rhs) => match op {
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
                                level: Level::Warning,
                                message: format!(
                                    "You should compare values of the same type, but got `{}` and `{}`.",
                                    lhs_ty, rhs_ty
                                ),
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
            },
            Expression_::Variable(sym) => {
                if let Some((value_ty, position)) = self.bindings.get(&sym.name) {
                    self.id_to_def_pos.insert(sym.id, position.clone());
                    return value_ty.clone();
                }

                match self.env.file_scope.get(&sym.name) {
                    Some(value) => {
                        let fun_info = match value {
                            Value::Fun { fun_info, .. } => Some(fun_info),
                            Value::Closure(_, fun_info) => Some(fun_info),
                            Value::BuiltinFunction(_, fun_info) => fun_info.as_ref(),
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
                            value,
                            Value::EnumVariant { .. } | Value::EnumConstructor { .. }
                        ) {
                            self.save_enum_variant_id(sym, value);
                        }

                        Type::from_value(value, &self.env.types, type_bindings)
                    }
                    None => Type::Error("Unbound variable".to_owned()),
                }
            }
            Expression_::Call(recv, paren_args) => {
                if let Expression_::Variable(s) = &recv.expr_ {
                    if s.name.name == "todo"
                        && self.bindings.get(&SymbolName::from("todo")).is_none()
                    {
                        self.diagnostics.push(Diagnostic {
                            message: "Unfinished code.".to_owned(),
                            position: pos.clone(),
                            level: Level::Warning,
                        });
                    }
                }

                let recv_ty = self.infer_expr(recv, type_bindings, expected_return_ty);

                match recv_ty {
                    Type::Fun {
                        type_params,
                        params,
                        return_,
                        name,
                    } => {
                        let formatted_name = match name {
                            Some(name) => format!("`{}`", name.name),
                            None => "This function".to_owned(),
                        };

                        if params.len() < paren_args.arguments.len() {
                            // Got too many arguments.
                            let first_excess_arg = &paren_args.arguments[params.len()];
                            let last_arg = paren_args.arguments.last().unwrap();

                            let position =
                                Position::merge(&first_excess_arg.position, &last_arg.position);

                            self.diagnostics.push(Diagnostic {
                                level: Level::Error,
                                message: format!(
                                    "{} expects {} argument{}, but got {}.",
                                    formatted_name,
                                    params.len(),
                                    if params.len() == 1 { "" } else { "s" },
                                    paren_args.arguments.len()
                                ),
                                position,
                            });
                        } else if params.len() > paren_args.arguments.len() {
                            // Got too few arguments.
                            self.diagnostics.push(Diagnostic {
                                level: Level::Error,
                                message: format!(
                                    "{} expects {} argument{}, but got {}.",
                                    formatted_name,
                                    params.len(),
                                    if params.len() == 1 { "" } else { "s" },
                                    paren_args.arguments.len()
                                ),
                                position: recv.position.clone(),
                            });
                        }

                        let mut ty_var_env = TypeVarEnv::default();
                        for type_param in type_params {
                            ty_var_env.insert(type_param.clone(), None);
                        }

                        let mut arg_tys = vec![];
                        for arg in &paren_args.arguments {
                            let arg_ty = self.infer_expr(arg, type_bindings, expected_return_ty);
                            arg_tys.push((arg_ty, arg.position.clone()));
                        }

                        for (param_ty, (arg_ty, _)) in params.iter().zip(arg_tys.iter()) {
                            unify_and_solve_ty(param_ty, arg_ty, &mut ty_var_env);
                        }

                        let params = params
                            .iter()
                            .map(|p| subst_ty_vars(p, &ty_var_env))
                            .collect::<Vec<_>>();

                        for (param_ty, (arg_ty, arg_pos)) in params.iter().zip(arg_tys) {
                            if !is_subtype(&arg_ty, param_ty) {
                                self.diagnostics.push(Diagnostic {
                                    level: Level::Error,
                                    message: format!(
                                        "Expected `{}` argument but got `{}`.",
                                        param_ty, arg_ty
                                    ),
                                    position: arg_pos,
                                });
                            }
                        }

                        subst_ty_vars(&return_, &ty_var_env)
                    }
                    Type::Error(_) => {
                        for arg in &paren_args.arguments {
                            // We still want to check arguments as far as possible.
                            self.infer_expr(arg, type_bindings, expected_return_ty);
                        }

                        // If the receiver is an error, use that error
                        // type for the return type of this call.
                        recv_ty.clone()
                    }
                    _ => {
                        for arg in &paren_args.arguments {
                            // We still want to check arguments as far as possible.
                            self.infer_expr(arg, type_bindings, expected_return_ty);
                        }

                        self.diagnostics.push(Diagnostic {
                            level: Level::Error,
                            message: format!("Expected a function, but got a `{}`.", recv_ty),
                            position: recv.position.clone(),
                        });

                        Type::Error("Calling something that isn't a function".to_owned())
                    }
                }
            }
            Expression_::MethodCall(recv, sym, paren_args) => {
                let receiver_ty = self.infer_expr(recv, type_bindings, expected_return_ty);
                if matches!(receiver_ty, Type::Error(_)) {
                    for arg in &paren_args.arguments {
                        self.infer_expr(arg, type_bindings, expected_return_ty);
                    }

                    // Allow calling methods on error types, to avoid cascading errors.
                    return Type::error("Called method on an error type");
                }

                let Some(receiver_ty_name) = receiver_ty.type_name() else {
                    for arg in &paren_args.arguments {
                        self.infer_expr(arg, type_bindings, expected_return_ty);
                    }

                    self.diagnostics.push(Diagnostic {
                        level: Level::Error,
                        message: format!(
                            "Expected a type with a `{}` method, but got a `{}`.",
                            &sym.name, receiver_ty
                        ),
                        position: recv.position.clone(),
                    });
                    return Type::error("No type name for this method receiver");
                };

                let methods = self
                    .env
                    .methods
                    .get(&receiver_ty_name)
                    .cloned()
                    .unwrap_or_default();

                match methods.get(&sym.name) {
                    Some(method_info) => {
                        self.id_to_def_pos
                            .insert(sym.id, method_info.name_sym.position.clone());

                        let Some(fun_info) = method_info.fun_info() else {
                            return Type::error("This method has no fun_info");
                        };

                        if let Some(def_id) = fun_info.item_id {
                            self.callees
                                .entry(self.current_item)
                                .or_default()
                                .insert(def_id);
                        }

                        if fun_info.params.len() != paren_args.arguments.len() {
                            self.diagnostics.push(Diagnostic {
                                level: Level::Error,
                                message: format!(
                                    "`{}::{}` requires {} argument{}, but got {}.",
                                    receiver_ty_name,
                                    sym.name,
                                    fun_info.params.len(),
                                    if fun_info.params.len() == 1 { "" } else { "s" },
                                    paren_args.arguments.len()
                                ),
                                position: sym.position.clone(),
                            });
                        }

                        let mut ty_var_env = TypeVarEnv::default();
                        for type_param in &fun_info.type_params {
                            ty_var_env.insert(type_param.name.clone(), None);
                        }

                        let param_decl_tys: Vec<Type> = fun_info
                            .params
                            .iter()
                            .map(|sym_with_hint| match &sym_with_hint.hint {
                                Some(hint) => Type::from_hint(hint, &self.env.types, &ty_var_env)
                                    .unwrap_or_err_ty(),
                                None => Type::Top,
                            })
                            .collect();

                        let recv_decl_ty = Type::from_hint(
                            &method_info.receiver_hint,
                            &self.env.types,
                            &ty_var_env,
                        )
                        .unwrap_or_err_ty();
                        unify_and_solve_ty(&recv_decl_ty, &receiver_ty, &mut ty_var_env);

                        let mut arg_tys = vec![];
                        for arg in &paren_args.arguments {
                            let arg_ty = self.infer_expr(arg, type_bindings, expected_return_ty);
                            arg_tys.push((arg_ty, arg.position.clone()));
                        }

                        for (param_ty, (arg_ty, _)) in param_decl_tys.iter().zip(arg_tys.iter()) {
                            unify_and_solve_ty(param_ty, arg_ty, &mut ty_var_env);
                        }

                        let params = param_decl_tys
                            .iter()
                            .map(|p| subst_ty_vars(p, &ty_var_env))
                            .collect::<Vec<_>>();

                        for (param_ty, (arg_ty, arg_pos)) in params.iter().zip(&arg_tys) {
                            if !is_subtype(arg_ty, param_ty) {
                                self.diagnostics.push(Diagnostic {
                                    level: Level::Error,
                                    message: format!(
                                        "Expected `{}` argument but got `{}`.",
                                        param_ty, arg_ty
                                    ),
                                    position: arg_pos.clone(),
                                });
                            }
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

                        subst_ty_vars(&ret_ty, &ty_var_env)
                    }
                    None => {
                        // No method exists with that name on this type.

                        for arg in &paren_args.arguments {
                            self.infer_expr(arg, type_bindings, expected_return_ty);
                        }

                        let available_methods = methods.keys().collect::<Vec<_>>();
                        let suggest =
                            if let Some(similar) = most_similar(&available_methods, &sym.name) {
                                // TODO: consider arity and types when
                                // trying to suggest the best
                                // alternative.
                                format!(" Did you mean `{}`?", similar)
                            } else {
                                "".to_owned()
                            };

                        self.diagnostics.push(Diagnostic {
                            level: Level::Error,
                            message: format!(
                                "`{}` has no method `{}`.{}",
                                receiver_ty_name, sym.name, suggest
                            ),
                            position: sym.position.clone(),
                        });
                        Type::error("No such method on this type")
                    }
                }
            }
            Expression_::DotAccess(recv, field_sym) => {
                let recv_ty = self.infer_expr(recv, type_bindings, expected_return_ty);
                let Some(recv_ty_name) = recv_ty.type_name() else {
                    return Type::error("No type name found for this receiver");
                };

                match recv_ty {
                    Type::UserDefined {
                        kind: TypeDefKind::Struct,
                        name,
                        ..
                    } => {
                        if let Some(TypeDef::Struct(struct_info)) = self.env.get_type_def(&name) {
                            for field in &struct_info.fields {
                                if field.sym.name == field_sym.name {
                                    let field_ty = Type::from_hint(
                                        &field.hint,
                                        &self.env.types,
                                        type_bindings,
                                    )
                                    .unwrap_or_err_ty();
                                    return field_ty;
                                }
                            }

                            self.diagnostics.push(Diagnostic {
                                level: Level::Error,
                                message: format!(
                                    "Struct `{}` has no field `{}`.",
                                    recv_ty_name, field_sym.name
                                ),
                                position: field_sym.position.clone(),
                            });

                            Type::error("No struct field with this name")
                        } else {
                            self.diagnostics.push(Diagnostic {
                                level: Level::Error,
                                message: format!("`{}` is not a struct.", recv_ty_name),
                                position: field_sym.position.clone(),
                            });

                            Type::error("No struct with this name")
                        }
                    }
                    _ => {
                        self.diagnostics.push(Diagnostic {
                            level: Level::Error,
                            message: format!("`{}` is not a struct.", recv_ty_name),
                            position: field_sym.position.clone(),
                        });

                        Type::error("This type is not a struct")
                    }
                }
            }
            Expression_::FunLiteral(fun_info) => {
                let param_tys = fun_info
                    .params
                    .iter()
                    .map(|param| match &param.hint {
                        Some(hint) => {
                            Type::from_hint(hint, &self.env.types, type_bindings).unwrap_or_err_ty()
                        }
                        None => Type::Top,
                    })
                    .collect::<Vec<_>>();

                let return_ty = match &fun_info.return_hint {
                    Some(hint) => {
                        Type::from_hint(hint, &self.env.types, type_bindings).unwrap_or_err_ty()
                    }
                    None => Type::Top,
                };

                let expected_ty = Type::Fun {
                    type_params: vec![],
                    params: param_tys,
                    return_: Box::new(return_ty),
                    name: None,
                };
                self.verify_expr_(&expected_ty, expr_, pos, type_bindings, expected_return_ty)
            }
            Expression_::Assert(expr) => {
                self.verify_expr(&Type::bool(), expr, type_bindings, expected_return_ty);
                Type::unit()
            }
            Expression_::Invalid => {
                // We've already emitted a parse error, so use the
                // bottom type to prevent later type errors.
                Type::no_value()
            }
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
        expected_return_ty: Option<&Type>,
    ) -> Type {
        self.verify_expr_(
            expected_ty,
            &expr.expr_,
            &expr.position,
            type_bindings,
            expected_return_ty,
        )
    }

    fn verify_expr_(
        &mut self,
        expected_ty: &Type,
        expr_: &Expression_,
        pos: &Position,
        type_bindings: &TypeVarEnv,
        expected_return_ty: Option<&Type>,
    ) -> Type {
        let ty = match (expr_, expected_ty) {
            (
                Expression_::FunLiteral(fun_info),
                Type::Fun {
                    name: _,
                    type_params: _,
                    params: expected_params,
                    return_: expected_return_ty,
                },
            ) => {
                self.bindings.enter_block();

                let mut param_tys = vec![];
                for (i, param) in fun_info.params.iter().enumerate() {
                    let param_ty = match &param.hint {
                        Some(hint) => {
                            let hint_ty = Type::from_hint(hint, &self.env.types, type_bindings)
                                .unwrap_or_err_ty();
                            self.save_hint_ty_id(hint, &hint_ty);
                            hint_ty
                        }
                        None => expected_params.get(i).cloned().unwrap_or(Type::Top),
                    };

                    self.set_binding(&param.symbol, param_ty.clone());
                    param_tys.push(param_ty);
                }

                let return_ty = match &fun_info.return_hint {
                    Some(hint) => {
                        let hint_ty = Type::from_hint(hint, &self.env.types, type_bindings)
                            .unwrap_or_err_ty();
                        self.save_hint_ty_id(hint, &hint_ty);

                        if !is_subtype(&hint_ty, expected_return_ty) {
                            self.diagnostics.push(Diagnostic {
                                level: Level::Error,
                                message: format!(
                                    "Expected a function with return type `{}` but got `{}`.",
                                    expected_return_ty, hint_ty
                                ),
                                position: pos.clone(),
                            });
                        }

                        self.verify_block(&hint_ty, &fun_info.body, type_bindings, Some(&hint_ty));
                        hint_ty
                    }
                    None => self.infer_block(&fun_info.body, type_bindings, None),
                };

                self.bindings.exit_block();

                Type::Fun {
                    type_params: vec![],
                    params: param_tys,
                    return_: Box::new(return_ty),
                    name: None,
                }
            }
            _ => self.infer_expr_(expr_, pos, type_bindings, expected_return_ty),
        };

        if !is_subtype(&ty, expected_ty) {
            self.diagnostics.push(Diagnostic {
                level: Level::Error,
                message: format!("Expected `{}`, but got `{}`.", expected_ty, ty),
                position: pos.clone(),
            });
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

fn enum_payload_type(env: &Env, scrutinee_ty: &Type, pattern_sym: &Symbol) -> Type {
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
        if variant.name_sym.name == pattern_sym.name {
            relevant_variant = Some(variant.clone());
        }
    }

    let Some(variant) = relevant_variant else {
        return Type::error(format!(
            "No variant found in `{}` named `{}`.",
            scrutinee_ty_name, pattern_sym.name
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
    Type::from_hint(&payload_hint, &env.types, &env.stack.type_bindings()).unwrap_or_err_ty()
}

/// Solve the type variables in this method, and return the resolved
/// type of the return type hint.
fn subst_type_vars_in_meth_return_ty(
    env: &Env,
    method_info: &MethodInfo,
    receiver_pos: &Position,
    receiver_ty: &Type,
    arg_tys: &[(Type, Position)],
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
    arg_tys: &[(Type, Position)],
    ty_var_env: &mut TypeVarEnv,
) -> (Vec<Diagnostic>, Type) {
    let mut diagnostics = vec![];

    for ((arg_ty, arg_pos), param) in arg_tys.iter().zip(&fun_info.params) {
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
        None => Type::Top,
    };

    (diagnostics, ret_ty)
}

fn subst_ty_vars(ty: &Type, ty_var_env: &TypeVarEnv) -> Type {
    match ty {
        Type::Error(_) | Type::Top => ty.clone(),
        Type::Tuple(elem_tys) => Type::Tuple(
            elem_tys
                .iter()
                .map(|ty| subst_ty_vars(ty, ty_var_env))
                .collect(),
        ),
        Type::Fun {
            type_params,
            params,
            return_,
            name,
        } => {
            let params = params
                .iter()
                .map(|p| subst_ty_vars(p, ty_var_env))
                .collect();
            let return_ = subst_ty_vars(return_, ty_var_env);

            Type::Fun {
                type_params: type_params.clone(),
                params,
                return_: Box::new(return_),
                name: name.clone(),
            }
        }
        Type::UserDefined { kind, name, args } => Type::UserDefined {
            kind: kind.clone(),
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| subst_ty_vars(arg, ty_var_env))
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
        ) if decl_kind == solved_kind && decl_name.name == solved_name.name => {
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
                        message: format!(
                            "Expected `{bound_ty}` (because {} is {bound_ty}), but got `{ty}`.",
                            hint.as_src(),
                        ),
                        position: position.clone(),
                        level: Level::Warning,
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

    if hint_name.name == "Fun" && hint.args.len() == 2 {
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
        } if name_sym.name == hint_name.name => {
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
    if matches!(ty_1, Type::Top) || matches!(ty_2, Type::Top) {
        return Some(Type::Top);
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
            if kind_1 != kind_2 || name_1.name != name_2.name || args_1.len() != args_2.len() {
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
            if pattern.symbol.name.is_underscore() {
                seen_underscore = true;
                continue;
            }

            match variants_remaining.remove(&pattern.symbol.name) {
                Some(_) => {
                    // First time we've seen this variant.
                }
                None => {
                    diagnostics.push(Diagnostic {
                        level: Level::Error,
                        message: "Duplicate case in pattern match.".to_owned(),
                        position: pattern.symbol.position.clone(),
                    });
                }
            }
        }

        // If any cases are _, this match is exhaustive.
        if seen_underscore {
            return;
        }
    }

    let missing: Vec<_> = variants_remaining.keys().collect();

    if let Some(missing_case) = missing.first() {
        diagnostics.push(Diagnostic {
            level: Level::Error,
            message: format!(
                "This match expression does not cover all the cases of `{}`. It's missing `{}`.",
                type_name, missing_case
            ),
            position: scrutinee_pos.clone(),
        });
    }
}
