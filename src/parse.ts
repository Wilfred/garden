function lex(src: string): string[] {
  const tokenRegex = /[0-9]+|[+-]|;/g;
  return Array.from(src.matchAll(tokenRegex), (m) => m[0]);
}

function popToken(tokens: string[]): string | null {
  return tokens.shift() || null;
}

function peekToken(tokens: string[]): string | null {
  return tokens[0] || null;
}

type IntegerLiteral = { kind: "integerLiteral"; value: number };

type BinaryOperator = {
  kind: "binaryOperator";
  operator: string;
  lhs: Expression;
  rhs: Expression;
};

type Expression = IntegerLiteral | BinaryOperator;

type Statement = {
  expression: Expression;
};

function parseExpression(tokens: string[]): Expression | null {
  const token = popToken(tokens);
  if (!token) {
    developerError("Expected an expression, got an empty input");
    return null;
  }

  if (token.match(/[0-9]+/)) {
    return { kind: "integerLiteral", value: parseInt(token, 10) };
  }

  developerError("Expected integer literal, got: " + token);
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

  const terminator = popToken(tokens);
  if (terminator != ";") {
    developerError("Expected ; terminator, got: " + terminator);
    return null;
  }

  return { expression: expr };
}

function parse(tokens: string[]): Statement[] | null {
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

function reset(): void {
  document.querySelectorAll("#error")[0].textContent = "";
}

function developerError(msg: string): void {
  document.querySelectorAll("#error")[0].textContent = msg;
}

setInterval(() => {
  reset();

  const editor = document.querySelectorAll("#editor")[0] as HTMLInputElement;
  const src = editor.value;
  const ast = parse(lex(src));

  document.querySelectorAll("#result")[0].textContent = JSON.stringify(
    ast,
    null,
    2
  );
}, 1000);
