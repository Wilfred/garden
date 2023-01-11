import { popToken, peekToken, requireToken, VARIABLE_NAME } from "./lexer";
import { developerError } from "./errors";

type IntegerLiteral = { kind: "integerLiteral"; value: number };

type BinaryOperator = {
  kind: "binaryOperator";
  operator: string;
  lhs: Expression;
  rhs: Expression;
};

type CallExpression = {
  kind: "call";
  receiver: Expression;
  arguments: Expression[];
};

type Symbol = {
  kind: "symbol";
  name: string;
};

export type Expression =
  | IntegerLiteral
  | BinaryOperator
  | Symbol
  | CallExpression;

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
  name: string;
  parameters: string[];
  body: Statement[];
};

export type ToplevelSyntax = FunctionDefinition | Statement;

function parseCallArguments(tokens: string[]): Expression[] | null {
  const openParen = requireToken(tokens, "(");
  if (!openParen) {
    return null;
  }

  const args = [];
  while (tokens.length > 0) {
    const firstToken = peekToken(tokens);
    if (firstToken == ")") {
      break;
    }

    const expr = parseExpression(tokens);
    if (!expr) {
      return null;
    }
    args.push(expr);

    // Require a comma or a closing parenthesis afterwards.
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

  return args;
}

function parseSimpleExpression(tokens: string[]): Expression | null {
  const token = popToken(tokens);
  if (!token) {
    developerError("Expected an expression, got an empty input");
    return null;
  }

  let expr: Expression | null = null;
  if (token.match(/[0-9]+/)) {
    expr = { kind: "integerLiteral", value: parseInt(token, 10) };
  } else if (token.match(VARIABLE_NAME)) {
    expr = { kind: "symbol", name: token };
  }

  const nextToken = peekToken(tokens);
  if (expr && nextToken == "(") {
    const args = parseCallArguments(tokens);
    if (!args) {
      return null;
    }
    expr = { kind: "call", receiver: expr, arguments: args };
  }

  if (expr) {
    return expr;
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

const RESERVED_WORDS = ["let", "fn"];

function parseVariable(tokens: string[]): string | null {
  const variable = popToken(tokens);
  if (!variable) {
    developerError("Expected a variable name, got EOF");
    return null;
  }

  if (variable.match(VARIABLE_NAME)) {
    if (RESERVED_WORDS.includes(variable)) {
      developerError("Expected a variable name, got reserved word " + variable);
      return null;
    }

    return variable;
  } else {
    developerError("Expected a variable name, got " + variable);
    return null;
  }
}

// Parse `let x = 1;`
function parseLetStatement(tokens: string[]): LetStatement | null {
  const letToken = requireToken(tokens, "let");
  if (!letToken) {
    return null;
  }

  const variable = parseVariable(tokens);
  if (!variable) {
    return null;
  }

  const equalsSign = requireToken(tokens, "=");
  if (!equalsSign) {
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

    const variable = parseVariable(tokens);
    if (!variable) {
      return null;
    }
    parameters.push(variable);

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

function parseFunctionBody(tokens: string[]): Statement[] | null {
  const openBrace = requireToken(tokens, "{");
  if (!openBrace) {
    return null;
  }

  const statements = [];
  while (tokens.length > 0) {
    const token = peekToken(tokens);
    if (token == "}") {
      break;
    }

    const statement = parseStatement(tokens);
    if (!statement) {
      return null;
    }

    statements.push(statement);
  }

  const closeBrace = requireToken(tokens, "}");
  if (!closeBrace) {
    return null;
  }

  return statements;
}

function parseFunction(tokens: string[]): FunctionDefinition | null {
  const fnKeyword = requireToken(tokens, "fn");
  if (!fnKeyword) {
    return null;
  }

  const name = parseVariable(tokens);
  if (!name) {
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

  return { kind: "function", parameters, body, name };
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
