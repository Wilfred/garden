function evalSnippet(src, snippetDiv) {
    snippetDiv.hidden = false;
    snippetDiv.innerHTML = "...";
    fetch("http://playground.garden-lang.org:3000/run", {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
        },
        body: JSON.stringify({ src: src }),
    })
        .then(function (response) { return response.json(); })
        .then(function (data) {
        if (!data.success) {
            snippetDiv.innerHTML = "Error: ".concat(data.error || "Unknown error");
            return;
        }
        if (!data.results || data.results.length === 0) {
            snippetDiv.innerHTML = "No output";
            return;
        }
        // Iterate over all results
        var stdoutParts = [];
        var finalValue = "";
        var hasError = false;
        for (var _i = 0, _a = data.results; _i < _a.length; _i++) {
            var item = _a[_i];
            // Check if this is a stdout object
            if ("printed" in item) {
                var stdoutItem = item;
                stdoutParts.push(stdoutItem.printed.s);
            }
            // Check if this is a result object
            else if ("error" in item || "value" in item) {
                var result = item;
                if (result.error) {
                    snippetDiv.innerHTML = "Error: ".concat(result.error);
                    hasError = true;
                    break;
                }
                if (result.value) {
                    finalValue = result.value;
                }
            }
        }
        if (!hasError) {
            var output = "";
            if (stdoutParts.length > 0) {
                output = stdoutParts.join("") + "\n";
            }
            output += finalValue;
            snippetDiv.innerHTML = output;
        }
    })
        .catch(function (error) {
        snippetDiv.innerHTML = "Fetch error: ".concat(error.message);
    });
}
document.querySelectorAll(".run-snippet").forEach(function (button) {
    var snippetDiv = button.closest(".snippet");
    var codeNode = snippetDiv === null || snippetDiv === void 0 ? void 0 : snippetDiv.querySelector("pre");
    var src = (codeNode === null || codeNode === void 0 ? void 0 : codeNode.textContent) || "";
    button.addEventListener("click", function (_e) {
        if (snippetDiv) {
            var outputDiv = snippetDiv.querySelector(".snippet-output");
            if (outputDiv instanceof HTMLElement) {
                evalSnippet(src, outputDiv);
            }
        }
    });
});
var playgroundRunButton = document.querySelector("#playground-run");
var playgroundEditor = document.querySelector("#playground-editor");
var playgroundOutput = document.querySelector("#playground-output");
if (playgroundRunButton &&
    playgroundOutput &&
    playgroundOutput instanceof HTMLElement &&
    playgroundEditor &&
    playgroundEditor instanceof HTMLTextAreaElement) {
    playgroundRunButton.addEventListener("click", function () {
        evalSnippet(playgroundEditor.value, playgroundOutput);
    });
}
