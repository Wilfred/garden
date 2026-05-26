import { HighlightStyle, syntaxHighlighting } from "@codemirror/language";
import { tags } from "@lezer/highlight";
import { StreamLanguage } from "@codemirror/language";

// Garden language definition
export const gardenLanguage = StreamLanguage.define({
  token(stream) {
    // Comments
    if (stream.match("//")) {
      stream.skipToEnd();
      return "comment";
    }

    // Strings
    if (stream.match('"')) {
      while (!stream.eol()) {
        if (stream.next() === '"') {
          break;
        }
      }
      return "string";
    }

    // Keywords
    if (
      stream.match(
        /\b(as|assert|break|catch|continue|else|enum|for|fun|if|import|in|let|match|method|public|return|shared|struct|test|try|while)\b/,
      )
    ) {
      return "keyword";
    }

    // Types (CamelCase words)
    if (stream.match(/\b[A-Z][a-zA-Z0-9_]*\b/)) {
      return "type";
    }

    // Skip other characters
    stream.next();
    return null;
  },
});

// Syntax highlighting style for Garden
export const gardenHighlighting = syntaxHighlighting(
  HighlightStyle.define([
    { tag: tags.comment, color: "gray" },
    { tag: tags.string, color: "#910a0a" },
    { tag: tags.keyword, color: "#d05416", fontWeight: "bold" },
    { tag: tags.typeName, color: "#57269c" },
  ]),
);
