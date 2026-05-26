import { EditorView } from "codemirror";
import { evalSnippet } from "./api";
import { createGardenEditor } from "./editor";

// Store EditorView instances for each snippet
const editorViews = new WeakMap<Element, EditorView>();

function resetSnippet(
  snippetDiv: Element,
  editorView: EditorView,
  originalCodeNode: HTMLPreElement | null,
  editButton: Element,
): void {
  const editorDom = editorView.dom;

  if (originalCodeNode) {
    // Restore original with syntax highlighting
    editorDom.replaceWith(originalCodeNode);
    editorView.destroy();
  }

  editorViews.delete(snippetDiv);

  // Hide the output div
  const outputDiv = snippetDiv.querySelector(".snippet-output");
  if (outputDiv instanceof HTMLElement) {
    outputDiv.hidden = true;
  }

  // Change button text back to "Edit"
  editButton.textContent = "Edit";
}

export function setupSnippetButtons(): void {
  document.querySelectorAll(".snippet").forEach((snippetDiv) => {
    // Set up run button
    const runButton = snippetDiv.querySelector(".run-snippet");
    if (runButton) {
      runButton.addEventListener("click", (_e) => {
        let src = "";

        // Check for CodeMirror editor first (edit mode)
        const editorView = editorViews.get(snippetDiv);
        if (editorView) {
          src = editorView.state.sliceDoc();
        } else {
          // Check for textarea (legacy edit mode) or pre (view mode)
          const textarea = snippetDiv.querySelector("textarea");
          const codeNode = snippetDiv.querySelector("pre");

          if (textarea instanceof HTMLTextAreaElement) {
            src = textarea.value;
          } else if (codeNode) {
            src = codeNode.textContent || "";
          }
        }

        const outputDiv = snippetDiv.querySelector(".snippet-output");
        if (outputDiv instanceof HTMLElement) {
          evalSnippet(src, outputDiv);
        }
      });
    }

    // Set up edit button
    const editButton = snippetDiv.querySelector(".edit-snippet");
    if (editButton) {
      let originalCodeNode: HTMLPreElement | null = null;
      let originalTextContent = "";

      editButton.addEventListener("click", (_e) => {
        const codeNode = snippetDiv.querySelector("pre");
        const editorView = editorViews.get(snippetDiv);

        // Check if we're in CodeMirror edit mode
        if (editorView) {
          // Currently in edit mode, reset to original pre element
          resetSnippet(snippetDiv, editorView, originalCodeNode, editButton);

          originalCodeNode = null;
          originalTextContent = "";
        } else if (codeNode instanceof HTMLPreElement) {
          // Currently in view mode, switch to CodeMirror edit mode
          originalCodeNode = codeNode.cloneNode(true) as HTMLPreElement;
          originalTextContent = codeNode.textContent || "";

          const newEditorView = createGardenEditor(originalTextContent);

          // Store reference in WeakMap so Run button can access it
          editorViews.set(snippetDiv, newEditorView);

          codeNode.replaceWith(newEditorView.dom);

          // Change button text to "Reset"
          editButton.textContent = "Reset";

          newEditorView.focus();
        }
      });
    }
  });
}
