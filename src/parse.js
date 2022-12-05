function lex(src) {
  return src.split(" ");
}

function parse(tokens) {
  if (tokens.length == 1 || tokens.length == 3) {
    return tokens;
  }

  return tokens;
}

setInterval(() => {
  const src = document.querySelectorAll("#editor")[0].value;
  const ast = parse(lex(src));

  document.querySelectorAll("#result")[0].textContent = JSON.stringify(ast);
}, 1000);
