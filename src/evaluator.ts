import { developerError } from "./errors";
import { Expression, Statement, ToplevelSyntax } from "./parser";

type NumberValue = {
  kind: "number";
  value: number;
};

export type Value = NumberValue;

export type Env = Map<string, Value>;

function evalExpression(env: Env, expr: Expression): Value | null {
  switch (expr.kind) {
    case "integerLiteral":
      return { value: expr.value, kind: "number" };
    case "symbol":
      const value = env.get(expr.name);
      if (value) {
        return value;
      } else {
        developerError("No such variable: " + expr.name);
        return null;
      }
    case "binaryOperator":
      const lhsValue = evalExpression(env, expr.lhs);
      const rhsValue = evalExpression(env, expr.rhs);

      if (lhsValue && rhsValue) {
        const operator = expr.operator;
        switch (operator) {
          case "+":
            return { value: lhsValue.value + rhsValue.value, kind: "number" };
          case "-":
            return { value: lhsValue.value - rhsValue.value, kind: "number" };
          default:
            return null;
        }
      } else {
        return null;
      }
  }
}

function evalStatement(env: Env, stmt: Statement): Value | null {
  switch (stmt.kind) {
    case "expression":
      return evalExpression(env, stmt.expression);
    case "let": {
      const rhs = evalExpression(env, stmt.expression);
      if (rhs) {
        env.set(stmt.variable, rhs);
      }
      return rhs;
    }
  }
}

export function evalStatements(env: Env, stmts: Statement[]): Value | null {
  let result = null;

  for (let stmt of stmts) {
    const value = evalStatement(env, stmt);
    if (value) {
      result = value;
    } else {
      return null;
    }
  }

  return result;
}

export function evalTopLevelSyntax(
  env: Env,
  items: ToplevelSyntax[]
): Value | null {
  let result = null;

  for (let item of items) {
    switch (item.kind) {
      case "function": {
        developerError("Cannot evaluate function definitions yet.");
        return null;
      }
      default: {
        const value = evalStatement(env, item);
        if (value) {
          result = value;
        } else {
          return null;
        }
      }
    }
  }

  return result;
}
