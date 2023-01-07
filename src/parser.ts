import { popToken, peekToken, requireToken, VARIABLE_NAME } from "./lexer";
import { developerError } from "./errors";

type IntegerLiteral = { kind: "integerLiteral"; value: number };

type BinaryOperator = {
  kind: "binaryOperator";
  operator: string;
  lhs: Expression;
  rhs: Expression;
};

type Symbol = {
  kind: "symbol";
  name: string;
};

export type Expression = IntegerLiteral | BinaryOperator | Symbol;

type ExpressionStatement = {
  expression: Expression;
};

type LetStatement = {
  variable: string;
  expression: Expression;
};

export type Statement = ExpressionStatement | LetStatement;

function parseExpression(tokens: string[]): Expression | null {
  const token = popToken(tokens);
  if (!token) {
    developerError("Expected an expression, got an empty input");
    return null;
  }

  if (token.match(/[0-9]+/)) {
    return { kind: "integerLiteral", value: parseInt(token, 10) };
  } else if (token.match(VARIABLE_NAME)) {
    return { kind: "symbol", name: token };
  }

  developerError("Expected an expression, got " + token);
  return null;
}

function parseBinaryOpOrExpression(tokens: string[]): Expression | null {
  const expr = parseExpression(tokens);

  const token = peekToken(tokens);
  if (expr && token && token.match(/[+-]/)) {
    popToken(tokens);

    const rhsExpr = parseExpression(tokens);
    if (rhsExpr) {
      return {
        kind: "binaryOperator",
        operator: token,
        lhs: expr,
        rhs: rhsExpr,
      };
    }
  }

  return expr;
}

function parseStatement(tokens: string[]): Statement | null {
  const expr = parseBinaryOpOrExpression(tokens);
  if (!expr) {
    return null;
  }

  const terminator = requireToken(tokens, ";");
  if (!terminator) {
    return null;
  }

  return { expression: expr };
}

export function parse(tokens: string[]): Statement[] | null {
  const statements = [];

  while (tokens.length > 0) {
    const statement = parseStatement(tokens);
    if (statement) {
      statements.push(statement);
    } else {
      return null;
    }
  }

  return statements;
}
