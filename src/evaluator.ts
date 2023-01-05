import { developerError } from "./errors";
import { Expression, Statement } from "./parser";

export type Value = {
  value: number;
};

export type Env = Map<string, Value>;

function evalExpression(env: Env, expr: Expression): Value | null {
  switch (expr.kind) {
    case "integerLiteral":
      return { value: expr.value };
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
            return { value: lhsValue.value + rhsValue.value };
          case "-":
            return { value: lhsValue.value - rhsValue.value };
          default:
            return null;
        }
      } else {
        return null;
      }
  }
}

function evalStatement(env: Env, stmt: Statement): Value | null {
  return evalExpression(env, stmt.expression);
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
