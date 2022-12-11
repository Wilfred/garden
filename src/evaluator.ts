import { Expression, Statement } from "./parser";

type Value = {
  value: number;
};

function evalExpression(expr: Expression): Value | null {
  switch (expr.kind) {
    case "integerLiteral":
      return { value: expr.value };
    case "binaryOperator":
      const lhsValue = evalExpression(expr.lhs);
      const rhsValue = evalExpression(expr.rhs);

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

function evalStatement(stmt: Statement): Value | null {
  return evalExpression(stmt.expression);
}

export function evalStatements(stmts: Statement[]): Value | null {
  let result = null;

  for (let stmt of stmts) {
    const value = evalStatement(stmt);
    if (value) {
      result = value;
    } else {
      return null;
    }
  }

  return result;
}
