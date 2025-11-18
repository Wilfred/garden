console.log("hello world");

function evalSnippet(src: string) {
  fetch("/good.json")
    .then((response) => response.text())
    .then((responseText) => {
      let evalResult = JSON.parse(responseText);

      console.log("Evaluating: " + src);
      console.log(evalResult);
    });
}

for (const button of document.querySelectorAll(".run-snippet")) {
  console.log(button);

  let codeNode = button?.parentNode?.nextSibling;
  let src = codeNode?.textContent || "";

  button.addEventListener("click", (_e) => {
    evalSnippet(src);
  });
}

console.log("Ready");
