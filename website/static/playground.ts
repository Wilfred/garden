function evalSnippet(src: string, snippetDiv: Element) {
  // TODO: why doesn't typescript know about the hidden field on Element?
  let snippetElement = snippetDiv as Element & { hidden: boolean };
  snippetElement.hidden = false;

  fetch("/good.json")
    .then((response) => response.text())
    .then((responseText) => {
      let evalResult = JSON.parse(responseText);

      snippetDiv.innerHTML = responseText;

      console.log("Evaluating: " + src);
      console.log(evalResult);
    });
}

for (const button of document.querySelectorAll(".run-snippet")) {
  console.log(button);

  let snippetDiv = button?.parentNode?.parentNode;
  let codeNode = button?.parentNode?.nextSibling;
  let src = codeNode?.textContent || "";

  button.addEventListener("click", (_e) => {
    if (snippetDiv) {
      let outputDiv = snippetDiv.querySelector(".snippet-output");
      if (outputDiv) {
        evalSnippet(src, outputDiv);
      }
    }
  });
}
