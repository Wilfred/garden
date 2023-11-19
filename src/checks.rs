use crate::ast::FunInfo;
use crate::diagnostics::Warning;
use crate::env::Env;

pub(crate) fn check_types_exist(fun_info: &FunInfo, env: &Env) -> Vec<Warning> {
    let mut warnings = vec![];

    for param in &fun_info.params {
        if let Some(return_type) = &param.type_ {
            if !env.types.contains_key(return_type) {
                warnings.push(Warning {
                    message: format!("No such type: {return_type}"),
                });
            }
        }
    }

    if let Some(return_type) = &fun_info.return_type {
        if !env.types.contains_key(return_type) {
            warnings.push(Warning {
                message: format!("No such type: {return_type}"),
            });
        }
    }

    warnings
}
