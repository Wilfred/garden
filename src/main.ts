import { lex } from "./lexer";
import { parse } from "./parser";
import { evalStatements } from "./evaluator";

function reset(): void {
  document.querySelectorAll("#result")[0].textContent = "";
  document.querySelectorAll("#parse-tree")[0].textContent = "";
  document.querySelectorAll("#error")[0].textContent = "";
}

function update(src: string): void {
  reset();

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
}

const editor = document.querySelectorAll("#editor")[0] as HTMLInputElement;

update(editor.value);
editor.addEventListener("input", (e) => {
  const target = e.target as HTMLInputElement;
  update(target.value);
});
