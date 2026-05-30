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

type FormatResponse = {
  success: boolean;
  formatted?: string;
  error?: string;
};

export function formatSnippet(
  src: string,
  onFormatted: (formatted: string) => void,
  onError: (message: string) => void,
): void {
  fetch(`${PLAYGROUND_HOST}/format`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ src }),
  })
    .then((response) => response.json())
    .then((data: FormatResponse) => {
      if (!data.success || data.formatted === undefined) {
        onError(data.error || "Unknown error");
        return;
      }
      onFormatted(data.formatted);
    })
    .catch((error: unknown) => {
      const message = error instanceof Error ? error.message : String(error);
      onError(message);
    });
}

export function fetchDiagnostics(
  src: string,
  onDiagnostics: (diagnostics: CheckDiagnostic[]) => void,
): void {
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
        return;
      }
      onDiagnostics(data.diagnostics || []);
    })
    .catch(() => {
      // Ignore errors from background checks.
    });
}
