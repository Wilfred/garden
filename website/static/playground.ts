import { EditorView, basicSetup } from "codemirror";
import { EditorState } from "@codemirror/state";

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

  // This is the live site. To test local versions:
  //
  // $ cd playground
  // $ npm start
  //
  // and change this to localhost:3000.
  fetch("https://playground.garden-lang.org/run", {
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

function setupSnippetButtons() {
  document.querySelectorAll(".snippet").forEach((snippetDiv) => {
    // Set up run button
    let runButton = snippetDiv.querySelector(".run-snippet");
    if (runButton) {
      runButton.addEventListener("click", (_e) => {
        let src = "";

        // Check for CodeMirror editor first (edit mode)
        let editorView = (snippetDiv as any)._editorView as EditorView | undefined;
        if (editorView) {
          src = editorView.state.sliceDoc();
        } else {
          // Check for textarea (legacy edit mode) or pre (view mode)
          let textarea = snippetDiv.querySelector("textarea");
          let codeNode = snippetDiv.querySelector("pre");

          if (textarea instanceof HTMLTextAreaElement) {
            src = textarea.value;
          } else if (codeNode) {
            src = codeNode.textContent || "";
          }
        }

        let outputDiv = snippetDiv.querySelector(".snippet-output");
        if (outputDiv instanceof HTMLElement) {
          evalSnippet(src, outputDiv);
        }
      });
    }

    // Set up edit button
    let editButton = snippetDiv.querySelector(".edit-snippet");
    if (editButton) {
      let originalCodeNode: HTMLPreElement | null = null;
      let originalTextContent = "";

      editButton.addEventListener("click", (_e) => {
        let codeNode = snippetDiv.querySelector("pre");
        let editorView = (snippetDiv as any)._editorView as EditorView | undefined;

        // Check if we're in CodeMirror edit mode
        if (editorView) {
          // Currently in edit mode with CodeMirror, switch back to view mode
          let currentValue = editorView.state.sliceDoc();
          let editorDom = editorView.dom;

          if (currentValue === originalTextContent && originalCodeNode) {
            // Content unchanged, restore original with syntax highlighting
            editorDom.replaceWith(originalCodeNode);
            editorView.destroy();
          } else {
            // Content changed, create plain pre element
            let pre = document.createElement("pre");
            pre.textContent = currentValue;
            editorDom.replaceWith(pre);
            editorView.destroy();
          }

          (snippetDiv as any)._editorView = null;
          originalCodeNode = null;
          originalTextContent = "";
        } else if (codeNode instanceof HTMLPreElement) {
          // Currently in view mode, switch to CodeMirror edit mode
          originalCodeNode = codeNode.cloneNode(true) as HTMLPreElement;
          originalTextContent = codeNode.textContent || "";

          // Create CodeMirror editor
          let state = EditorState.create({
            doc: originalTextContent,
            extensions: [basicSetup, EditorView.lineWrapping],
          });

          let newEditorView = new EditorView({
            state,
          });

          // Store reference on the snippet div so Run button can access it
          (snippetDiv as any)._editorView = newEditorView;

          codeNode.replaceWith(newEditorView.dom);
        }
      });
    }
  });
}

function setupPlayground() {
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
}

setupSnippetButtons();
setupPlayground();
