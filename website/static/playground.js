console.log("hello world");

function evalSnippet(_src) {
  fetch("/good.json")
    .then((response) => response.text())
    .then((responseText) => {
      let evalResult = JSON.parse(responseText);
      console.log(evalResult);
    });
}

evalSnippet("1 + 2");
