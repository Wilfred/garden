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

        let lhsNumber;
        if (lhsValue.kind == "number") {
          lhsNumber = lhsValue.value;
        } else {
          developerError("Expected a number, got: " + lhsValue.kind);
          return null;
        }

        let rhsNumber;
        if (rhsValue.kind == "number") {
          rhsNumber = rhsValue.value;
        } else {
          developerError("Expected a number, got: " + rhsValue.kind);
          return null;
        }

        switch (operator) {
          case "+":
            return { value: lhsNumber + rhsNumber, kind: "number" };
          case "-":
            return { value: lhsNumber - rhsNumber, kind: "number" };
          default:
            return null;
        }
      } else {
        return null;
      }
    case "call": {
      developerError("Not yet implemented: function calls");
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
