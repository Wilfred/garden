function evalSnippet(src, snippetDiv) {
    snippetDiv.hidden = false;
    snippetDiv.innerHTML = "...";
    fetch("http://playground.garden-lang.org:3000/run", {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
        },
        body: JSON.stringify({ src }),
    })
        .then((response) => response.json())
        .then((data) => {
        if (!data.success) {
            snippetDiv.innerHTML = `Error: ${data.error || "Unknown error"}`;
            return;
        }
        if (!data.results || data.results.length === 0) {
            snippetDiv.innerHTML = "No output";
            return;
        }
        // Iterate over all results
        const stdoutParts = [];
        let finalValue = "";
        let hasError = false;
        for (const item of data.results) {
            // Check if this is a stdout object
            if ("printed" in item) {
                const stdoutItem = item;
                stdoutParts.push(stdoutItem.printed.s);
            }
            // Check if this is a result object
            else if ("error" in item || "value" in item) {
                const result = item;
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
    document.querySelectorAll(".run-snippet").forEach((button) => {
        let snippetDiv = button.closest(".snippet");
        let codeNode = snippetDiv === null || snippetDiv === void 0 ? void 0 : snippetDiv.querySelector("pre");
        let src = (codeNode === null || codeNode === void 0 ? void 0 : codeNode.textContent) || "";
        button.addEventListener("click", (_e) => {
            if (snippetDiv) {
                let outputDiv = snippetDiv.querySelector(".snippet-output");
                if (outputDiv instanceof HTMLElement) {
                    evalSnippet(src, outputDiv);
                }
            }
        });
    });
}
function setupPlayground() {
    let playgroundRunButton = document.querySelector("#playground-run");
    let playgroundEditor = document.querySelector("#playground-editor");
    let playgroundOutput = document.querySelector("#playground-output");
    if (playgroundRunButton &&
        playgroundOutput &&
        playgroundOutput instanceof HTMLElement &&
        playgroundEditor &&
        playgroundEditor instanceof HTMLTextAreaElement) {
        playgroundRunButton.addEventListener("click", () => {
            evalSnippet(playgroundEditor.value, playgroundOutput);
        });
    }
}
setupSnippetButtons();
setupPlayground();
export {};
//# sourceMappingURL=playground.js.map