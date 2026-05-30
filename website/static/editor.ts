import { EditorView, minimalSetup } from "codemirror";
import { EditorState } from "@codemirror/state";
import { history, historyKeymap } from "@codemirror/commands";
import { keymap, highlightActiveLine } from "@codemirror/view";
import { defaultKeymap } from "@codemirror/commands";
import { linter, setDiagnostics } from "@codemirror/lint";
import type { Diagnostic } from "@codemirror/lint";
import { gardenLanguage, gardenHighlighting } from "./language";
import type { CheckDiagnostic } from "./api";

export function createGardenEditor(
  doc: string,
  onDocChanged?: (src: string) => void,
): EditorView {
  const state = EditorState.create({
    doc,
    extensions: [
      minimalSetup,
      EditorView.lineWrapping,
      history(),
      keymap.of([...defaultKeymap, ...historyKeymap]),
      highlightActiveLine(),
      gardenLanguage,
      gardenHighlighting,
      // Install the lint UI but don't auto-run; diagnostics are
      // pushed via setEditorDiagnostics after a /check call.
      linter(() => [], { delay: 0 }),
      EditorView.updateListener.of((update) => {
        if (update.docChanged && onDocChanged) {
          onDocChanged(update.state.sliceDoc());
        }
      }),
    ],
  });

  return new EditorView({ state });
}

export function setEditorContent(view: EditorView, content: string): void {
  view.dispatch({
    changes: { from: 0, to: view.state.doc.length, insert: content },
  });
}

export function setEditorDiagnostics(
  view: EditorView,
  diagnostics: CheckDiagnostic[],
): void {
  const doc = view.state.doc;
  const lineCount = doc.lines;

  const cmDiagnostics: Diagnostic[] = diagnostics.map((d) => {
    // Garden line numbers are 1-indexed; columns are 0-indexed byte
    // offsets within the line.
    const startLineNum = Math.min(Math.max(d.line_number, 1), lineCount);
    const endLineNum = Math.min(
      Math.max(d.end_line_number, startLineNum),
      lineCount,
    );

    const startLine = doc.line(startLineNum);
    const endLine = doc.line(endLineNum);

    const from = Math.min(startLine.from + d.column, startLine.to);
    let to = Math.min(endLine.from + d.end_column, endLine.to);
    if (to <= from) {
      to = Math.min(from + 1, doc.length);
    }

    return {
      from,
      to,
      severity: d.severity,
      message: d.message,
    };
  });

  view.dispatch(setDiagnostics(view.state, cmDiagnostics));
}
