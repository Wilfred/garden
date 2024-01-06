import init, { check_parse } from "./pkg/garden_lang_web.js";

function showParseState(textarea) {
  let src = textarea.value;
  const result = check_parse(src);

  let parseStatusSpan = document.getElementById("parse-status");
  if (result == null) {
    textarea.classList.remove("is-invalid");
    parseStatusSpan.innerHTML = "";
  } else {
    textarea.classList.add("is-invalid");
    parseStatusSpan.innerHTML = result;
  }
}

async function run() {
  await init();

  let parseInput = document.getElementById("parse-input");
  let src = parseInput.value;
  showParseState(parseInput);

  parseInput.addEventListener("input", () => showParseState(parseInput), false);
}

run();
