type Position = {
  start_offset: number;
  end_offset: number;
  line_number: number;
  end_line_number: number;
  column: number;
  end_column: number;
  path: String;
};

type EvalErr = {
  position: Position;
  message: string;
  stack: string;
};

type EvalValueOk = { Ok: string };
type EvalValueErr = { Err: EvalErr[] };

type EvalResponse = {
  evaluate: {
    warnings: any[];
    value: EvalValueOk | EvalValueErr;
    stack_frame_name: string;
  };
};

function evalSnippet(src: string, snippetDiv: HTMLElement) {
  snippetDiv.hidden = false;

  snippetDiv.innerHTML = "...";

  fetch("http://localhost:3000/good.json")
    .then((response) => response.text())
    .then((responseText) => {
      let evalResult = JSON.parse(responseText) as EvalResponse;
      let evalValue = evalResult.evaluate.value;

      if ("Ok" in evalValue) {
        snippetDiv.innerHTML = evalValue.Ok;
      } else {
        let errors = evalValue.Err;
        snippetDiv.innerHTML = errors
          .map((error) => {
            return error.message;
          })
          .join("\n");
      }
    });
}

document.querySelectorAll(".run-snippet").forEach((button) => {
  console.log(button);

  let snippetDiv = button?.parentNode?.parentNode;
  let codeNode = button?.parentNode?.nextSibling;
  let src = codeNode?.textContent || "";

  button.addEventListener("click", (_e) => {
    if (snippetDiv) {
      let outputDiv = snippetDiv.querySelector(".snippet-output");
      if (outputDiv instanceof HTMLElement) {
        evalSnippet(src, outputDiv);
      }
    }
  });
});
