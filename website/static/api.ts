import escapeHtml from "escape-html";

// The live playground host. To test local versions:
//
// $ cd playground
// $ npm start
//
// and change this to "http://localhost:3000".
const PLAYGROUND_HOST = "https://playground.garden-lang.org";

type StdoutOutput = {
  printed: {
    s: string;
  };
};

type StderrOutput = {
  printed_stderr: {
    s: string;
  };
};

type PlaygroundResult = {
  error: string | null;
  value: string | null;
};

type PlaygroundResponse = {
  success: boolean;
  results?: (StdoutOutput | StderrOutput | PlaygroundResult)[];
  error?: string;
  rawOutput?: string;
};

export type CheckDiagnostic = {
  line_number: number;
  end_line_number: number;
  column: number;
  end_column: number;
  message: string;
  severity: "error" | "warning";
};

type CheckResponse = {
  success: boolean;
  diagnostics?: CheckDiagnostic[];
  error?: string;
  rawOutput?: string;
};

export function evalSnippet(src: string, snippetDiv: HTMLElement): void {
  snippetDiv.hidden = false;

  snippetDiv.innerHTML = '<div class="spinner"></div>';

  fetch(`${PLAYGROUND_HOST}/run`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ src }),
  })
    .then((response) => response.json())
    .then((data: PlaygroundResponse) => {
      if (!data.success) {
        snippetDiv.innerHTML = `Error: ${escapeHtml(data.error || "Unknown error")}`;
        return;
      }

      if (!data.results || data.results.length === 0) {
        snippetDiv.innerHTML = "No output";
        return;
      }

      // Iterate over all results, preserving stdout/stderr emission order.
      const outputParts: string[] = [];
      const valueParts: string[] = [];
      let hasError = false;

      for (const item of data.results) {
        if ("printed" in item) {
          outputParts.push(escapeHtml(item.printed.s));
        } else if ("printed_stderr" in item) {
          outputParts.push(
            `<span class="stderr">${escapeHtml(item.printed_stderr.s)}</span>`,
          );
        } else if ("error" in item || "value" in item) {
          const result = item;

          if (result.error) {
            snippetDiv.innerHTML = `Error: ${escapeHtml(result.error)}`;
            hasError = true;
            break;
          }

          if (result.value) {
            valueParts.push(escapeHtml(result.value));
          }
        }
      }

      if (!hasError) {
        let output = "";
        if (outputParts.length > 0) {
          output = outputParts.join("");
        }
        if (valueParts.length > 0) {
          if (output.length > 0) {
            output += "\n";
          }
          output += valueParts.join("\n");
        }

        snippetDiv.innerHTML = output;
      }
    })
    .catch((error: unknown) => {
      const message = error instanceof Error ? error.message : String(error);
      snippetDiv.innerHTML = `Fetch error: ${escapeHtml(message)}`;
    });
}

export function checkSnippet(
  src: string,
  snippetDiv: HTMLElement,
  onDiagnostics?: (diagnostics: CheckDiagnostic[]) => void,
): void {
  snippetDiv.hidden = false;

  snippetDiv.innerHTML = '<div class="spinner"></div>';

  fetch(`${PLAYGROUND_HOST}/check`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ src }),
  })
    .then((response) => response.json())
    .then((data: CheckResponse) => {
      if (!data.success) {
        snippetDiv.innerHTML = `Error: ${escapeHtml(data.error || "Unknown error")}`;
        return;
      }

      const diagnostics = data.diagnostics || [];
      if (onDiagnostics) {
        onDiagnostics(diagnostics);
      }
      if (diagnostics.length === 0) {
        snippetDiv.innerHTML = "No issues found.";
        return;
      }

      const lines = diagnostics.map((d) => {
        const label = d.severity === "error" ? "Error" : "Warning";
        const cls = d.severity === "error" ? "stderr" : "";
        const prefix = `${label} at line ${d.line_number}, column ${d.column}: `;
        const text = `${prefix}${d.message}`;
        return cls
          ? `<span class="${cls}">${escapeHtml(text)}</span>`
          : escapeHtml(text);
      });
      snippetDiv.innerHTML = lines.join("\n");
    })
    .catch((error: unknown) => {
      const message = error instanceof Error ? error.message : String(error);
      snippetDiv.innerHTML = `Fetch error: ${escapeHtml(message)}`;
    });
}
