function lex(src) {
  const tokenRegex = /[0-9]+|[+-]|;/g;
  return Array.from(src.matchAll(tokenRegex), (m) => m[0]);
}

function papToken(tokens) {
  return tokens.shift();
}

function peekToken(tokens) {
  return tokens[0];
}

function parseExpression(tokens) {
  const token = papToken(tokens);

  if (token.match(/[0-9]+/)) {
    return { intLiteral: parseInt(token, 10) };
  }

  developerError("Expected integer literal, got: " + token);
  return null;
}

function parseBinaryOpOrExpression(tokens) {
  const expr = parseExpression(tokens);

  const token = peekToken(tokens);
  if (token.match(/[+-]/)) {
    papToken(tokens);

    const rhsExpr = parseExpression(tokens);
    return { binaryOperator: token, lhs: expr, rhs: rhsExpr };
  } else {
    return expr;
  }
}

function parseTerminatedExpression(tokens) {
  const expr = parseBinaryOpOrExpression(tokens);

  const terminator = papToken(tokens);
  if (terminator != ";") {
    developerError("Expected ; terminator, got: " + terminator);
    return null;
  }

  return { terminatedExpression: expr };
}

function parse(tokens) {
  return parseTerminatedExpression(tokens);
}

function reset() {
  document.querySelectorAll("#error")[0].textContent = "";
}

function developerError(msg) {
  document.querySelectorAll("#error")[0].textContent = msg;
}

setInterval(() => {
  reset();

  const src = document.querySelectorAll("#editor")[0].value;
  const ast = parse(lex(src));

  document.querySelectorAll("#result")[0].textContent = JSON.stringify(ast);
}, 1000);
