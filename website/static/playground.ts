import { evalSnippet, checkSnippet } from "./api";
import { createGardenEditor } from "./editor";
import { setupSnippetButtons } from "./snippets";

function setupPlayground(): void {
  const playgroundRunButton = document.querySelector("#playground-run");
  const playgroundCheckButton = document.querySelector("#playground-check");
  const playgroundEditor = document.querySelector("#playground-editor");
  const playgroundOutput = document.querySelector("#playground-output");
  if (
    playgroundRunButton &&
    playgroundOutput &&
    playgroundOutput instanceof HTMLElement &&
    playgroundEditor &&
    playgroundEditor instanceof HTMLTextAreaElement
  ) {
    const initialContent = playgroundEditor.value + "\n\n";

    const editorView = createGardenEditor(initialContent);

    // Replace textarea with CodeMirror editor
    playgroundEditor.replaceWith(editorView.dom);

    playgroundRunButton.addEventListener("click", () => {
      const src = editorView.state.sliceDoc();
      evalSnippet(src, playgroundOutput);
    });

    if (playgroundCheckButton) {
      playgroundCheckButton.addEventListener("click", () => {
        const src = editorView.state.sliceDoc();
        checkSnippet(src, playgroundOutput);
      });
    }

    editorView.focus();
  }
}

setupSnippetButtons();
setupPlayground();
