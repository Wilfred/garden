function lex(src) {
  const tokenRegex = /[0-9]+|[+-]|;/g;
  return Array.from(src.matchAll(tokenRegex), (m) => m[0]);
}

function parse(tokens) {
  if (tokens.length == 1 || tokens.length == 3) {
    return tokens;
  }

  developerError("Wrong number of tokens: " + tokens.length);
  return [];
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
