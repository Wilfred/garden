use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

use gen_lsp_types::{
    ActiveParameter, Documentation, ParameterInformation, ParameterInformationLabel, SignatureHelp,
    SignatureInformation,
};

use crate::checks::type_checker::{check_types, TCSummary};
use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::namespaces::NamespaceInfo;
use crate::parser::ast::{
    Expression, Expression_, FunInfo, IdGenerator, ParenthesizedArguments, ToplevelItem,
};
use crate::parser::parse_toplevel_items;
use crate::parser::visitor::Visitor;
use crate::values::Value_;
use crate::Vfs;

/// Compute signature help for the call enclosing `offset`, if any.
pub(crate) fn signature_help(src: &str, path: &Path, offset: usize) -> Option<SignatureHelp> {
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_or_create_namespace(path);
    load_toplevel_items(&items, &mut env, Rc::clone(&ns));

    let summary = check_types(&vfs_path, &items, &env, Rc::clone(&ns));

    let call = enclosing_call(&items, offset)?;
    let paren_args = call_arguments(&call)?;

    let (fun_info, name) = resolve_fun_info(&call, &env, &ns, &summary)?;

    let active_param = active_param_index(paren_args, offset);
    let signature = build_signature(&fun_info, &name, active_param);

    Some(SignatureHelp {
        signatures: vec![signature],
        active_signature: Some(0),
        active_parameter: Some(ActiveParameter::Int(active_param)),
    })
}

/// The innermost call or method call whose argument list contains
/// `offset`.
fn enclosing_call(items: &[ToplevelItem], offset: usize) -> Option<Expression> {
    let mut finder = CallFinder {
        offset,
        calls: vec![],
    };
    for item in items {
        finder.visit_toplevel_item(item);
    }

    // Prefer the innermost call, i.e. the one whose argument list
    // starts latest.
    finder
        .calls
        .into_iter()
        .max_by_key(|call| match call_arguments(call) {
            Some(args) => args.open_paren.start_offset,
            None => 0,
        })
}

struct CallFinder {
    offset: usize,
    calls: Vec<Expression>,
}

impl Visitor for CallFinder {
    fn visit_expr(&mut self, expr: &Expression) {
        if let Some(paren_args) = call_arguments(expr) {
            // The cursor must be inside the parentheses, i.e. after the
            // opening `(` and no later than the closing `)`.
            if self.offset > paren_args.open_paren.start_offset
                && self.offset <= paren_args.close_paren.end_offset
            {
                self.calls.push(expr.clone());
            }
        }

        self.visit_expr_(&expr.expr_);
    }
}

/// The parenthesized arguments of a call or method call expression.
fn call_arguments(expr: &Expression) -> Option<&ParenthesizedArguments> {
    match &expr.expr_ {
        Expression_::Call(_, paren_args) | Expression_::MethodCall(_, _, paren_args) => {
            Some(paren_args)
        }
        _ => None,
    }
}

/// Resolve the function definition and display name for `call`.
fn resolve_fun_info(
    call: &Expression,
    env: &Env,
    ns: &Rc<RefCell<NamespaceInfo>>,
    summary: &TCSummary,
) -> Option<(FunInfo, String)> {
    match &call.expr_ {
        Expression_::Call(callee, _) => match &callee.expr_ {
            Expression_::Variable(sym) => {
                let ns = ns.borrow();
                let value = ns.values.get(&sym.name)?;
                let fun_info = value.fun_info()?.clone();
                Some((fun_info, sym.name.text.clone()))
            }
            Expression_::NamespaceAccess(recv, sym) => {
                let Expression_::Variable(ns_sym) = &recv.expr_ else {
                    return None;
                };

                let ns = ns.borrow();
                let value = ns.values.get(&ns_sym.name)?;
                let Value_::Namespace { ns_info, .. } = value.as_ref() else {
                    return None;
                };

                let ns_info = ns_info.borrow();
                let target = ns_info.values.get(&sym.name)?;
                let fun_info = target.fun_info()?.clone();
                Some((fun_info, sym.name.text.clone()))
            }
            _ => None,
        },
        Expression_::MethodCall(recv, sym, _) => {
            let recv_ty = summary.id_to_ty.get(&recv.id)?;
            let type_name = recv_ty.type_name()?;
            let method_info = env.types.get(&type_name)?.methods.get(&sym.name)?;
            let fun_info = method_info.fun_info()?.clone();
            Some((fun_info, sym.name.text.clone()))
        }
        _ => None,
    }
}

/// The index of the parameter the cursor is currently on, determined by
/// counting the argument commas before `offset`.
fn active_param_index(paren_args: &ParenthesizedArguments, offset: usize) -> u32 {
    let mut index = 0;
    for arg in &paren_args.arguments {
        if let Some(comma) = &arg.comma {
            if comma.end_offset <= offset {
                index += 1;
            }
        }
    }
    index
}

/// Build the signature information for `fun_info`, rendering its
/// parameters and return type.
fn build_signature(fun_info: &FunInfo, name: &str, active_param: u32) -> SignatureInformation {
    let mut label = String::new();
    label.push_str(name);
    label.push('(');

    let mut parameters = vec![];
    for (i, param) in fun_info.params.params.iter().enumerate() {
        if i > 0 {
            label.push_str(", ");
        }

        let param_label = match &param.hint {
            Some(hint) => format!("{}: {}", param.symbol.name, hint.as_src()),
            None => param.symbol.name.text.clone(),
        };

        parameters.push(ParameterInformation {
            label: ParameterInformationLabel::String(param_label.clone()),
            documentation: None,
        });
        label.push_str(&param_label);
    }
    label.push(')');

    if let Some(return_hint) = &fun_info.return_hint {
        label.push_str(": ");
        label.push_str(&return_hint.as_src());
    }

    SignatureInformation {
        label,
        documentation: fun_info.doc_comment.clone().map(Documentation::String),
        parameters: Some(parameters),
        active_parameter: Some(ActiveParameter::Int(active_param)),
    }
}
