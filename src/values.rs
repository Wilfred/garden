use garden_lang_parser::values::Value;

use crate::env::Env;
use crate::types::Type;

pub(crate) trait ValueExt {
    fn display(&self, env: &Env) -> String;
    fn display_unless_unit(&self, env: &Env) -> Option<String>;
}

impl ValueExt for Value {
    /// Pretty-print `self` in a human friendly way.
    fn display(&self, env: &Env) -> String {
        match self {
            Value::Integer(i) => format!("{}", i),
            Value::Fun(name_sym, _) => format!("(function: {})", name_sym.name),
            Value::Closure(..) => "(closure)".to_string(),
            Value::BuiltinFunction(kind) => format!("(function: {})", kind),
            Value::String(s) => escape_string_literal(s),
            Value::List(items) => {
                let mut s = String::new();

                s.push('[');

                for (i, item) in items.iter().enumerate() {
                    if i != 0 {
                        s.push_str(", ");
                    }

                    s.push_str(&item.display(env));
                }

                s.push(']');

                s
            }
            Value::Enum(name, variant_idx, payload) => {
                let type_ = match env.types.get(name) {
                    Some(type_) => type_,
                    None => {
                        return format!("{}__OLD_DEFINITION::{}", name, variant_idx);
                    }
                };

                let mut variant_takes_payload = false;
                let variant_name = match type_ {
                    Type::Builtin(_) => {
                        unreachable!("Enum type names should never map to built-in types")
                    }
                    Type::Enum(enum_info) => match enum_info.variants.get(*variant_idx) {
                        Some(variant_sym) => {
                            variant_takes_payload = variant_sym.has_payload;
                            format!("{}", variant_sym)
                        }
                        None => format!("{}::__OLD_VARIANT_{}", name, variant_idx),
                    },
                };

                match payload {
                    Some(value) => format!("{variant_name}({})", value.display(env)),
                    None => {
                        if variant_takes_payload {
                            format!("{variant_name} (constructor)")
                        } else {
                            variant_name
                        }
                    }
                }
            }
        }
    }

    fn display_unless_unit(&self, env: &Env) -> Option<String> {
        match self {
            Value::Enum(name, variant_idx, _) if name.0 == "Unit" && *variant_idx == 0 => None,
            _ => Some(self.display(env)),
        }
    }
}

/// Convert "foo" to "\"foo\"", a representation that we can print as
/// a valid Garden string literal.
pub(crate) fn escape_string_literal(s: &str) -> String {
    let mut res = String::new();
    res.push('"');

    // Escape inner double quotes and backslashes.
    for c in s.chars() {
        match c {
            '"' => res.push_str("\\\""),
            '\n' => res.push_str("\\n"),
            '\\' => res.push_str("\\\\"),
            _ => res.push(c),
        }
    }

    res.push('"');
    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_value_for_string_with_doublequote() {
        let env = Env::default();
        let value = Value::String("foo \\ \" \n bar".into());
        assert_eq!(value.display(&env), "\"foo \\\\ \\\" \\n bar\"");
    }
}
