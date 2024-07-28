use std::io::{Read, Write};
use std::path::PathBuf;
use std::time::Instant;
use std::{
    io::BufRead,
    sync::{atomic::AtomicBool, Arc},
};

use serde::{Deserialize, Serialize};

use crate::checks::assign_ids::assign_toplevel_item_ids;
use crate::diagnostics::{format_error_with_stack, format_parse_error, Diagnostic, Level};
use crate::env::Env;
use crate::eval::{eval_all_toplevel_items, eval_env, push_test_stackframe};
use crate::pos_to_id::{find_expr_of_id, find_item_at};
use crate::types::TypeDef;
use crate::values::Value;
use crate::{
    commands::{print_available_commands, run_command, Command, CommandParseError, EvalAction},
    eval::{EvalError, Session},
};
use garden_lang_parser::ast::{SourceString, SymbolName, SyntaxId, ToplevelItem, TypeName};
use garden_lang_parser::position::Position;
use garden_lang_parser::{parse_toplevel_items, parse_toplevel_items_from_span, ParseError};

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
enum Method {
    Run,
    FindDefinition,
    EvalUpToId,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "method", rename_all = "snake_case")]
enum Request {
    Run {
        input: String,
        // TODO: distinguish methods 'running in REPL' from 'running a snippet
        // in a file', so we don't need these optional fields.
        #[serde(skip_serializing_if = "Option::is_none")]
        path: Option<PathBuf>,
        #[serde(skip_serializing_if = "Option::is_none")]
        offset: Option<usize>,
        #[serde(skip_serializing_if = "Option::is_none")]
        end_offset: Option<usize>,
    },
    FindDefinition {
        name: String,
    },
    EvalUpToId {
        path: Option<PathBuf>,
        src: String,
        offset: usize,
    },
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ResponseKind {
    Evaluate,
    RunCommand,
    Ready,
    MalformedRequest,
    Printed,
    // TODO: find a nice way to suspend the interpreter.
    InteractivePrompt,
    FoundDefinition,
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct ResponseError {
    position: Option<Position>,
    message: String,
    stack: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct Response {
    pub(crate) kind: ResponseKind,
    pub(crate) value: Result<String, ResponseError>,
    pub(crate) warnings: Vec<Diagnostic>,
}

pub(crate) fn sample_request_as_json() -> String {
    serde_json::to_string(&Request::Run {
        input: "1 + 2".into(),
        path: Some(PathBuf::from("/foo/bar.gdn")),
        offset: Some(100),
        end_offset: Some(110),
    })
    .unwrap()
}

fn handle_eval_request(
    path: Option<&PathBuf>,
    input: &str,
    offset: Option<usize>,
    end_offset: Option<usize>,
    env: &mut Env,
    session: &mut Session,
    complete_src: &mut String,
) -> Response {
    complete_src.push_str(input);

    let (items, mut errors) = parse_toplevel_items_from_span(
        &path
            .cloned()
            .unwrap_or_else(|| PathBuf::from("__json_session_unnamed__")),
        input,
        offset.unwrap_or(0),
        end_offset.unwrap_or(input.len()),
    );

    // TODO: eval requests should return all parse errors.
    if let Some(e) = errors.pop() {
        match e {
            ParseError::Invalid {
                position,
                message,
                additional: _,
            } => {
                let stack = Some(format_parse_error(
                    &message,
                    &position,
                    Level::Error,
                    &SourceString {
                        src: input.to_owned(),
                        offset: 0,
                    },
                ));
                return Response {
                    kind: ResponseKind::Evaluate,
                    value: Err(ResponseError {
                        position: Some(position),
                        message: message.0,
                        stack,
                    }),
                    warnings: vec![],
                };
            }
            ParseError::Incomplete { message, .. } => {
                return Response {
                    kind: ResponseKind::Evaluate,
                    value: Err(ResponseError {
                        position: None,
                        message: message.0,
                        stack: None,
                    }),
                    warnings: vec![],
                };
            }
        }
    }

    match eval_all_toplevel_items(&items, env, session) {
        Ok(eval_summary) => {
            let definition_summary = if eval_summary.new_syms.len() == 1 {
                format!("Loaded {}", eval_summary.new_syms[0].0)
            } else {
                format!("Loaded {} definitions", eval_summary.new_syms.len())
            };

            let total_tests = eval_summary.tests_passed + eval_summary.tests_failed;
            let definition_summary = if total_tests == 0 {
                definition_summary
            } else {
                format!(
                    "{definition_summary}, ran {total_tests} {}",
                    if total_tests == 1 { "test" } else { "tests" }
                )
            };

            let value_summary = if let Some(last_value) = eval_summary.values.last() {
                if eval_summary.new_syms.is_empty() {
                    last_value.display(env)
                } else {
                    format!(
                        "{}, and evaluated {}",
                        definition_summary,
                        last_value.display(env)
                    )
                }
            } else {
                format!("{}.", definition_summary)
            };

            Response {
                kind: ResponseKind::Evaluate,
                value: Ok(value_summary),
                warnings: eval_summary.diagnostics,
            }
        }
        Err(EvalError::ResumableError(position, e)) => {
            // TODO: use the original SourceString rather than reconstructing.
            let stack = format_error_with_stack(&e, &position, &env.stack.0);

            Response {
                kind: ResponseKind::Evaluate,
                value: Err(ResponseError {
                    position: Some(position),
                    message: format!("Error: {}", e.0),
                    stack: Some(stack),
                }),
                warnings: vec![],
            }
        }
        Err(EvalError::Interrupted) => Response {
            kind: ResponseKind::Evaluate,
            value: Err(ResponseError {
                position: None,
                message: "Interrupted".to_owned(),
                stack: None,
            }),
            warnings: vec![],
        },
    }
}

fn handle_eval_up_to_id_request(
    path: Option<&PathBuf>,
    src: &str,
    offset: usize,
    env: &mut Env,
    session: &mut Session,
) -> Response {
    let (items, mut errors) = parse_toplevel_items(
        &path
            .cloned()
            .unwrap_or_else(|| PathBuf::from("__json_session_unnamed__")),
        src,
    );
    assign_toplevel_item_ids(&items);

    toplevel_item_containing_offset(&items, offset);

    if let Some(e) = errors.pop() {
        match e {
            ParseError::Invalid {
                position,
                message,
                additional: _,
            } => {
                let stack = Some(format_parse_error(
                    &message,
                    &position,
                    Level::Error,
                    &SourceString {
                        src: src.to_owned(),
                        offset: 0,
                    },
                ));
                return Response {
                    kind: ResponseKind::Evaluate,
                    value: Err(ResponseError {
                        position: Some(position),
                        message: message.0,
                        stack,
                    }),
                    warnings: vec![],
                };
            }
            ParseError::Incomplete { message, .. } => {
                return Response {
                    kind: ResponseKind::Evaluate,
                    value: Err(ResponseError {
                        position: None,
                        message: message.0,
                        stack: None,
                    }),
                    warnings: vec![],
                };
            }
        }
    }

    let mut expr_id: Option<SyntaxId> = None;
    for id in find_item_at(&items, offset).into_iter().rev() {
        // TODO: this is iterating items twice, which will be slower.
        if find_expr_of_id(&items, id).is_some() {
            expr_id = Some(id);
            break;
        }
    }

    let Some(expr_id) = expr_id else {
        todo!("Error report on no match found");
    };

    let item = toplevel_item_containing_offset(&items, offset);

    let Some(item) = item else {
        todo!("Error report on no match found");
    };

    match item {
        ToplevelItem::Def(_) => todo!(),
        ToplevelItem::Expr(_) => {
            session.stop_at_expr_id = Some(expr_id);

            let res = eval_all_toplevel_items(&[item.clone()], env, session);
            session.stop_at_expr_id = None;

            match res {
                Ok(eval_summary) => {
                    let value_summary = match eval_summary.values.last() {
                        Some(value) => value.display(env),
                        None => format!("{:?}", eval_summary),
                    };

                    Response {
                        kind: ResponseKind::Evaluate,
                        value: Ok(value_summary),
                        warnings: eval_summary.diagnostics,
                    }
                }
                Err(_) => todo!("error during eval"),
            }
        }
    }
}

fn toplevel_item_containing_offset(items: &[ToplevelItem], offset: usize) -> Option<&ToplevelItem> {
    for item in items {
        let pos = match item {
            ToplevelItem::Def(def) => &def.1,
            ToplevelItem::Expr(expr) => &expr.0.pos,
        };

        if pos.contains_offset(offset) {
            return Some(item);
        }
    }

    None
}

fn handle_request(
    req: Request,
    env: &mut Env,
    session: &mut Session,
    complete_src: &mut String,
) -> Response {
    match req {
        Request::Run {
            path,
            input,
            offset,
            end_offset,
        } => match Command::from_string(&input) {
            Ok(command) => {
                let mut out_buf: Vec<u8> = vec![];
                match run_command(&mut out_buf, &command, env, session) {
                    Ok(()) => Response {
                        kind: ResponseKind::RunCommand,
                        value: Ok(format!("{}", String::from_utf8_lossy(&out_buf))),
                        warnings: vec![],
                    },
                    Err(EvalAction::Abort) => Response {
                        kind: ResponseKind::RunCommand,
                        value: Ok("Aborted".to_owned()),
                        warnings: vec![],
                    },
                    Err(EvalAction::Resume) => eval_to_response(env, session),
                    Err(EvalAction::RunTest(name)) => {
                        let test_opt = env.tests.get(&name).cloned();
                        match test_opt {
                            Some(test) => {
                                push_test_stackframe(&test, env);
                                eval_to_response(env, session)
                            }
                            None => Response {
                                kind: ResponseKind::MalformedRequest,
                                value: Err(ResponseError {
                                    position: None,
                                    message: format!("No such test: {}", name),
                                    stack: None,
                                }),
                                warnings: vec![],
                            },
                        }
                    }
                    Err(EvalAction::Replace(expr)) => {
                        let stack_frame = env.stack.0.last_mut().unwrap();

                        stack_frame.evalled_values.pop();
                        stack_frame.exprs_to_eval.push((false, expr));

                        eval_to_response(env, session)
                    }
                    Err(EvalAction::Skip) => {
                        let stack_frame = env.stack.0.last_mut().unwrap();

                        stack_frame
                            .exprs_to_eval
                            .pop()
                            .expect("Tried to skip an expression, but none in this frame.");

                        eval_to_response(env, session)
                    }
                }
            }
            Err(CommandParseError::NoSuchCommand) => {
                let mut out_buf: Vec<u8> = vec![];
                write!(&mut out_buf, "No such command. ").unwrap();
                print_available_commands(&mut out_buf);

                Response {
                    kind: ResponseKind::RunCommand,
                    value: Err(ResponseError {
                        position: None,
                        message: format!("{}", String::from_utf8_lossy(&out_buf)),
                        stack: None,
                    }),
                    warnings: vec![],
                }
            }
            Err(CommandParseError::NotCommandSyntax) => handle_eval_request(
                path.as_ref(),
                &input,
                offset,
                end_offset,
                env,
                session,
                complete_src,
            ),
        },
        Request::FindDefinition { name } => handle_find_def_request(&name, env),
        Request::EvalUpToId { path, src, offset } => {
            handle_eval_up_to_id_request(path.as_ref(), &src, offset, env, session)
        }
    }
}

fn position_of_name(name: &str, env: &Env) -> Result<Position, String> {
    if let Some(type_) = env.get_type_def(&TypeName {
        name: name.to_owned(),
    }) {
        let pos = match type_ {
            TypeDef::Builtin(_) => return Err(format!("`{}` is a built-in type.", name)),
            TypeDef::Enum(enum_info) => &enum_info.name_sym.position,
            TypeDef::Struct(struct_info) => &struct_info.name_sym.position,
        };

        return Ok(pos.clone());
    }

    if let Some(v) = env.file_scope.get(&SymbolName(name.to_owned())) {
        return position_of_fun(name, v);
    }

    Err(format!("`{}` is not a function or type.", name))
}

fn position_of_fun(name: &str, v: &Value) -> Result<Position, String> {
    let fun_info = match v {
        Value::Fun { fun_info, .. } => Some(fun_info),
        Value::BuiltinFunction(_, fun_info) => fun_info.as_ref(),
        _ => {
            return Err(format!("`{}` is not a function.", name));
        }
    };

    let Some(fun_info) = fun_info else {
        return Err(format!("`{}` does not have any function information (closure or undocumented built-in function).", name));
    };
    let Some(name) = &fun_info.name else {
        return Err(format!(
            "`{}` does not have a name symbol (it's a closure).",
            name
        ));
    };

    Ok(name.position.clone())
}

fn handle_find_def_request(name: &str, env: &mut Env) -> Response {
    let value = match position_of_name(name, env) {
        Ok(pos) => Ok(pos.as_ide_string()),
        Err(message) => Err(ResponseError {
            position: None,
            message,
            stack: None,
        }),
    };

    Response {
        kind: ResponseKind::FoundDefinition,
        value,
        warnings: vec![],
    }
}

fn eval_to_response(env: &mut Env, session: &mut Session<'_>) -> Response {
    match eval_env(env, session) {
        Ok(result) => Response {
            kind: ResponseKind::Evaluate,
            value: Ok(result.display(env)),
            warnings: vec![],
        },
        Err(EvalError::ResumableError(position, e)) => Response {
            kind: ResponseKind::Evaluate,
            value: Err(ResponseError {
                position: Some(position),
                message: format!("Error: {}", e.0),
                stack: None,
            }),
            warnings: vec![],
        },
        Err(EvalError::Interrupted) => Response {
            kind: ResponseKind::Evaluate,
            value: Err(ResponseError {
                position: None,
                message: "Interrupted".to_owned(),
                stack: None,
            }),
            warnings: vec![],
        },
    }
}

pub(crate) fn json_session(interrupted: &Arc<AtomicBool>) {
    let response = Response {
        kind: ResponseKind::Ready,
        value: Ok("The Garden: Good programs take time to grow.".into()),
        warnings: vec![],
    };
    let serialized = serde_json::to_string(&response).unwrap();
    println!("{}", serialized);

    let mut env = Env::default();
    let mut complete_src = String::new();
    let mut session = Session {
        interrupted,
        has_attached_stdout: false,
        start_time: Instant::now(),
        trace_exprs: false,
        // TODO: set this position limit from the request.
        stop_at_expr_id: None,
    };

    loop {
        let mut line = String::new();
        let stdin = std::io::stdin();
        stdin
            .lock()
            .read_line(&mut line)
            .expect("Could not read line");

        if line.trim() == "" {
            // TODO: Once Emacs flushes properly, we won't see blank lines.
            continue;
        }

        if let Some(length_str) = line.trim().strip_prefix("Content-Length: ") {
            let length: usize = length_str.parse().expect("TODO: handle malformed length");

            let mut buf = vec![0; length];
            stdin
                .lock()
                .read_exact(&mut buf)
                .expect("Could not read payload");

            let buf_str = String::from_utf8(buf).unwrap();

            let response = match serde_json::from_str::<Request>(&buf_str) {
                Ok(req) => handle_request(req, &mut env, &mut session, &mut complete_src),
                Err(_) => Response {
                    kind: ResponseKind::MalformedRequest,
                    value: Err(ResponseError {
                        position: None,
                        message: format!(
                            "Invalid request (JSON decode failed). A valid request looks like: {}. The request received was:\n\n{}",
                            sample_request_as_json(),
                            buf_str,
                        ),
                        stack: None,
                    }),
                    warnings: vec![],
                },
            };
            let serialized = serde_json::to_string(&response).unwrap();
            println!("{}", serialized);
        } else {
            let err_response = Response {
                kind: ResponseKind::MalformedRequest,
                value: Err(ResponseError {
                    position: None,
                    message: format!(
                        "Invalid request (header missing). A valid request looks like: {}. The request received was:\n\n{}",
                        sample_request_as_json(),
                        line,
                    ),
                    stack: None,
                }),
                warnings: vec![],
            };
            let serialized = serde_json::to_string(&err_response).unwrap();
            println!("{}", serialized);
        }
    }
}
