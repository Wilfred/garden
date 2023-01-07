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
  kind: "expression";
  expression: Expression;
};

type LetStatement = {
  kind: "let";
  variable: string;
  expression: Expression;
};

export type Statement = ExpressionStatement | LetStatement;

type FunctionDefinition = {
  kind: "function";
  parameters: string[];
  body: Statement[];
};

export type ToplevelSyntax = FunctionDefinition | Statement;

function parseSimpleExpression(tokens: string[]): Expression | null {
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

function parseExpression(tokens: string[]): Expression | null {
  const expr = parseSimpleExpression(tokens);

  const token = peekToken(tokens);
  if (expr && token && token.match(/[+-]/)) {
    popToken(tokens);

    const rhsExpr = parseSimpleExpression(tokens);
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

// Parse `let x = 1;`
function parseLetStatement(tokens: string[]): LetStatement | null {
  const letToken = requireToken(tokens, "let");
  if (!letToken) {
    return null;
  }

  const variable = popToken(tokens);
  if (!variable) {
    return null;
  }
  if (variable.match(VARIABLE_NAME)) {
    // TODO: Keywords (e.g. let should not be a valid variable name).
  } else {
    developerError("Expected a variable name, got " + variable);
    return null;
  }

  const equalsSign = requireToken(tokens, "=");
  if (!equalsSign) {
    developerError("Expected =, got " + equalsSign);
    return null;
  }

  const expression = parseExpression(tokens);
  if (!expression) {
    return null;
  }

  const terminator = requireToken(tokens, ";");
  if (!terminator) {
    return null;
  }

  return { kind: "let", variable, expression };
}

function parseExpressionStatement(
  tokens: string[]
): ExpressionStatement | null {
  const expr = parseExpression(tokens);
  if (!expr) {
    return null;
  }

  const terminator = requireToken(tokens, ";");
  if (!terminator) {
    return null;
  }

  return { expression: expr, kind: "expression" };
}

function parseStatement(tokens: string[]): Statement | null {
  const token = peekToken(tokens);
  if (token == "let") {
    return parseLetStatement(tokens);
  }

  return parseExpressionStatement(tokens);
}

function parseFunctionParameters(tokens: string[]): string[] | null {
  const openParen = requireToken(tokens, "(");
  if (!openParen) {
    return null;
  }

  const parameters = [];
  while (tokens.length > 0) {
    const firstToken = peekToken(tokens);
    if (firstToken == ")") {
      break;
    }

    const variable = popToken(tokens);
    if (!variable) {
      return null;
    }
    if (variable.match(VARIABLE_NAME)) {
      // TODO: Keywords (e.g. let should not be a valid variable name).
      parameters.push(variable);
    } else {
      developerError("Expected a variable name, got " + variable);
      return null;
    }

    // Require a comma or a closing parenthesis after this symbol.
    const token = peekToken(tokens);
    if (token == ")") {
      break;
    }

    const comma = requireToken(tokens, ",");
    if (!comma) {
      return null;
    }
  }

  const closeParen = requireToken(tokens, ")");
  if (!closeParen) {
    return null;
  }

  return parameters;
}

function parseFunctionBody(_tokens: string[]): Statement[] | null {
  return [];
}

function parseFunction(tokens: string[]): FunctionDefinition | null {
  const fnKeyword = requireToken(tokens, "fn");
  if (!fnKeyword) {
    developerError("Expected fn, got: " + fnKeyword);
    return null;
  }

  const parameters = parseFunctionParameters(tokens);
  if (!parameters) {
    return null;
  }

  const body = parseFunctionBody(tokens);
  if (!body) {
    return null;
  }

  return { kind: "function", parameters, body };
}

function parseTopLevel(
  tokens: string[]
): FunctionDefinition | Statement | null {
  const token = peekToken(tokens);
  if (token == "fn") {
    return parseFunction(tokens);
  }

  return parseStatement(tokens);
}

export function parse(tokens: string[]): ToplevelSyntax[] | null {
  const result = [];

  while (tokens.length > 0) {
    const item = parseTopLevel(tokens);
    if (item) {
      result.push(item);
    } else {
      return null;
    }
  }

  return result;
}
