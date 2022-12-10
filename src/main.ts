import { lex } from "./lexer";
import { parse } from "./parser";

function reset(): void {
  document.querySelectorAll("#error")[0].textContent = "";
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
