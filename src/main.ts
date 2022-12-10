import { lex } from "./lexer";
import { parse } from "./parser";
import { evalStatements } from "./evaluator";

function reset(): void {
  document.querySelectorAll("#result")[0].textContent = "";
  document.querySelectorAll("#parse-tree")[0].textContent = "";
  document.querySelectorAll("#error")[0].textContent = "";
}

setInterval(() => {
  reset();

  const editor = document.querySelectorAll("#editor")[0] as HTMLInputElement;
  const src = editor.value;
  const ast = parse(lex(src));

  document.querySelectorAll("#parse-tree")[0].textContent = JSON.stringify(
    ast,
    null,
    2
  );

  if (ast) {
    const value = evalStatements(ast);

    if (value) {
      document.querySelectorAll("#result")[0].textContent = JSON.stringify(
        value,
        null,
        2
      );
    }
  }
}, 1000);
