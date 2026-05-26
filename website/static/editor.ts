import { EditorView, minimalSetup } from "codemirror";
import { EditorState } from "@codemirror/state";
import { history, historyKeymap } from "@codemirror/commands";
import { keymap, highlightActiveLine } from "@codemirror/view";
import { defaultKeymap } from "@codemirror/commands";
import { gardenLanguage, gardenHighlighting } from "./language";

export function createGardenEditor(doc: string): EditorView {
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
    ],
  });

  return new EditorView({ state });
}
