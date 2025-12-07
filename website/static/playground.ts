type StdoutOutput = {
  printed: {
    s: string;
  };
};

type PlaygroundResult = {
  error: string | null;
  value: string | null;
};

type PlaygroundResponse = {
  success: boolean;
  results?: (StdoutOutput | PlaygroundResult)[];
  error?: string;
  rawOutput?: string;
};

function evalSnippet(src: string, snippetDiv: HTMLElement) {
  snippetDiv.hidden = false;

  snippetDiv.innerHTML = "...";

  fetch("http://5.175.183.111:3000/run", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ src }),
  })
    .then((response) => response.json())
    .then((data: PlaygroundResponse) => {
      if (!data.success) {
        snippetDiv.innerHTML = `Error: ${data.error || "Unknown error"}`;
        return;
      }

      if (!data.results || data.results.length === 0) {
        snippetDiv.innerHTML = "No output";
        return;
      }

      // Iterate over all results
      const stdoutParts: string[] = [];
      let finalValue = "";
      let hasError = false;

      for (const item of data.results) {
        // Check if this is a stdout object
        if ("printed" in item) {
          const stdoutItem = item as StdoutOutput;
          stdoutParts.push(stdoutItem.printed.s);
        }
        // Check if this is a result object
        else if ("error" in item || "value" in item) {
          const result = item as PlaygroundResult;

          if (result.error) {
            snippetDiv.innerHTML = `Error: ${result.error}`;
            hasError = true;
            break;
          }

          if (result.value) {
            finalValue = result.value;
          }
        }
      }

      if (!hasError) {
        let output = "";
        if (stdoutParts.length > 0) {
          output = stdoutParts.join("") + "\n";
        }
        output += finalValue;

        snippetDiv.innerHTML = output;
      }
    })
    .catch((error) => {
      snippetDiv.innerHTML = `Fetch error: ${error.message}`;
    });
}

document.querySelectorAll(".run-snippet").forEach((button) => {
  let snippetDiv = (button as HTMLElement).closest(".snippet");
  let codeNode = snippetDiv?.querySelector("pre");
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

let playgroundRunButton = document.querySelector("#playground-run");
let playgroundEditor = document.querySelector("#playground-editor");
let playgroundOutput = document.querySelector("#playground-output");
if (
  playgroundRunButton &&
  playgroundOutput &&
  playgroundOutput instanceof HTMLElement &&
  playgroundEditor &&
  playgroundEditor instanceof HTMLTextAreaElement
) {
  playgroundRunButton.addEventListener("click", () => {
    evalSnippet(playgroundEditor.value, playgroundOutput);
  });
}
