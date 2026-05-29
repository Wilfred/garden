import { evalSnippet, fetchDiagnostics } from "./api";
import { createGardenEditor, setEditorDiagnostics } from "./editor";
import { setupSnippetButtons } from "./snippets";

// How long to wait after the last edit before checking the code.
const CHECK_DEBOUNCE_MS = 250;

function setupPlayground(): void {
  const playgroundRunButton = document.querySelector("#playground-run");
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

    let checkTimer: ReturnType<typeof setTimeout> | undefined;
    const scheduleCheck = (src: string): void => {
      clearTimeout(checkTimer);
      checkTimer = setTimeout(() => {
        fetchDiagnostics(src, (diagnostics) => {
          setEditorDiagnostics(editorView, diagnostics);
        });
      }, CHECK_DEBOUNCE_MS);
    };

    const editorView = createGardenEditor(initialContent, (src) => {
      scheduleCheck(src);
    });

    // Replace textarea with CodeMirror editor
    playgroundEditor.replaceWith(editorView.dom);

    playgroundRunButton.addEventListener("click", () => {
      const src = editorView.state.sliceDoc();
      evalSnippet(src, playgroundOutput);
    });

    // Check the initial content so diagnostics show without an edit.
    scheduleCheck(editorView.state.sliceDoc());

    editorView.focus();
  }
}

setupSnippetButtons();
setupPlayground();
