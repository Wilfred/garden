use std::io::{Read, Write};
use std::path::PathBuf;
use std::time::Instant;
use std::{
    io::BufRead,
    sync::{atomic::AtomicBool, Arc},
};

use serde::{Deserialize, Serialize};

use crate::diagnostics::{format_error_with_stack, format_parse_error, Diagnostic, Level};
use crate::env::Env;
use crate::eval::{
    eval_all_toplevel_items, eval_env, eval_up_to, push_test_stackframe, EvaluatedState,
};
use crate::types::TypeDef;
use crate::values::Value;
use crate::{
    commands::{print_available_commands, run_command, Command, CommandParseError, EvalAction},
    eval::{EvalError, Session},
};
use garden_lang_parser::ast::{SourceString, SymbolName, ToplevelItem, TypeName};
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
    pub(crate) value: Result<Option<String>, ResponseError>,
    pub(crate) position: Option<Position>,
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
        &mut env.id_gen,
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
                    position: None,
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
                    position: None,
                    warnings: vec![],
                };
            }
        }
    }

    match eval_all_toplevel_items(&items, env, session) {
        Ok(eval_summary) => {
            let definition_summary = if eval_summary.new_syms.is_empty() {
                "".to_owned()
            } else if eval_summary.new_syms.len() == 1 {
                format!("Loaded {}", eval_summary.new_syms[0].0)
            } else {
                format!("Loaded {} definitions", eval_summary.new_syms.len())
            };

            let total_tests = eval_summary.tests_passed + eval_summary.tests_failed;
            let test_summary = if total_tests == 0 {
                "".to_owned()
            } else {
                format!(
                    "{total_tests} {}",
                    if total_tests == 1 { "test" } else { "tests" }
                )
            };

            let test_summary = match (test_summary.is_empty(), definition_summary.is_empty()) {
                (true, _) => "".to_owned(),
                (false, true) => format!("Ran {test_summary}"),
                (false, false) => format!(", ran {test_summary}"),
            };

            let summary = format!("{definition_summary}{test_summary}");

            let value_summary = if let Some(last_value) = eval_summary.values.last() {
                Some(if summary.is_empty() {
                    last_value.display(env)
                } else {
                    format!(
                        "{summary}, and the expression evaluated to {}.",
                        last_value.display(env)
                    )
                })
            } else if summary.is_empty() {
                None
            } else {
                Some(format!("{summary}."))
            };

            Response {
                kind: ResponseKind::Evaluate,
                value: Ok(value_summary),
                position: None,
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
                position: None,
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
            position: None,
            warnings: vec![],
        },
    }
}

fn handle_eval_up_to_request(
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
        &mut env.id_gen,
    );

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
                    position: None,
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
                    position: None,
                    warnings: vec![],
                };
            }
        }
    }

    match eval_up_to(env, session, &items, offset) {
        Some(eval_res) => match eval_res {
            Ok((v, pos)) => Response {
                kind: ResponseKind::Evaluate,
                value: Ok(Some(v.display(env))),
                position: Some(pos),
                warnings: vec![],
            },
            Err(e) => match e {
                EvalError::Interrupted => Response {
                    kind: ResponseKind::Evaluate,
                    value: Err(ResponseError {
                        position: None,
                        message: "Interrupted.".to_owned(),
                        stack: None,
                    }),
                    position: None,
                    warnings: vec![],
                },
                EvalError::ResumableError(_, message) => Response {
                    kind: ResponseKind::Evaluate,
                    value: Err(ResponseError {
                        position: None,
                        message: message.0,
                        stack: None,
                    }),
                    position: None,
                    warnings: vec![],
                },
            },
        },
        None => Response {
            kind: ResponseKind::Evaluate,
            value: Ok(Some("Did not find an expression to evaluate".to_owned())),
            position: None,
            warnings: vec![],
        },
    }
}

pub(crate) fn toplevel_item_containing_offset(
    items: &[ToplevelItem],
    offset: usize,
) -> Option<&ToplevelItem> {
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

pub(crate) fn handle_request(
    req_src: &str,
    env: &mut Env,
    session: &mut Session,
    complete_src: &mut String,
) -> Response {
    let Ok(req) = serde_json::from_str::<Request>(req_src) else {
        return Response {
            kind: ResponseKind::MalformedRequest,
            value: Err(ResponseError {
                position: None,
                message: format!(
                    "Invalid request (JSON decode failed). A valid request looks like: {}. The request received was:\n\n{}",
                    sample_request_as_json(),
                    req_src,
                ),
                stack: None,
            }),
            position: None,
            warnings: vec![],
        };
    };

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
                        value: Ok(Some(format!("{}", String::from_utf8_lossy(&out_buf)))),
                        position: None,
                        warnings: vec![],
                    },
                    Err(EvalAction::Abort) => Response {
                        kind: ResponseKind::RunCommand,
                        value: Ok(Some("Aborted".to_owned())),
                        position: None,
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
                                position: None,
                                warnings: vec![],
                            },
                        }
                    }
                    Err(EvalAction::Replace(expr)) => {
                        let stack_frame = env.stack.0.last_mut().unwrap();

                        stack_frame.evalled_values.pop();
                        stack_frame
                            .exprs_to_eval
                            .push((EvaluatedState::NotEvaluated, expr));

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
                    position: None,
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
            handle_eval_up_to_request(path.as_ref(), &src, offset, env, session)
        }
    }
}

fn position_of_name(name: &str, env: &Env) -> Result<Position, String> {
    if let Some(type_) = env.get_type_def(&TypeName {
        name: name.to_owned(),
    }) {
        let pos = match type_ {
            TypeDef::Builtin(_, Some(struct_info)) => &struct_info.name_sym.position,
            TypeDef::Builtin(_, None) => return Err(format!("`{}` is a built-in type.", name)),
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
        Ok(pos) => Ok(Some(pos.as_ide_string())),
        Err(message) => Err(ResponseError {
            position: None,
            message,
            stack: None,
        }),
    };

    Response {
        kind: ResponseKind::FoundDefinition,
        value,
        position: None,
        warnings: vec![],
    }
}

fn eval_to_response(env: &mut Env, session: &mut Session) -> Response {
    match eval_env(env, session) {
        Ok(result) => Response {
            kind: ResponseKind::Evaluate,
            value: Ok(Some(result.display(env))),
            position: None,
            warnings: vec![],
        },
        Err(EvalError::ResumableError(position, e)) => Response {
            kind: ResponseKind::Evaluate,
            value: Err(ResponseError {
                position: Some(position),
                message: format!("Error: {}", e.0),
                stack: None,
            }),
            position: None,
            warnings: vec![],
        },
        Err(EvalError::Interrupted) => Response {
            kind: ResponseKind::Evaluate,
            value: Err(ResponseError {
                position: None,
                message: "Interrupted".to_owned(),
                stack: None,
            }),
            position: None,
            warnings: vec![],
        },
    }
}

pub(crate) fn json_session(interrupted: Arc<AtomicBool>) {
    let response = Response {
        kind: ResponseKind::Ready,
        value: Ok(Some(
            "The Garden: Good programs take time to grow.".to_owned(),
        )),
        position: None,
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

            let response = handle_request(&buf_str, &mut env, &mut session, &mut complete_src);
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
                        position: None,
                warnings: vec![],
            };
            let serialized = serde_json::to_string(&err_response).unwrap();
            println!("{}", serialized);
        }
    }
}
