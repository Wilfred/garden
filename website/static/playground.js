function evalSnippet(src, snippetDiv) {
    snippetDiv.hidden = false;
    snippetDiv.innerHTML = "...";
    fetch("http://localhost:3000/good.json")
        .then(function (response) { return response.text(); })
        .then(function (responseText) {
        var evalResult = JSON.parse(responseText);
        var evalValue = evalResult.evaluate.value;
        if ("Ok" in evalValue) {
            snippetDiv.innerHTML = evalValue.Ok;
        }
        else {
            var errors = evalValue.Err;
            snippetDiv.innerHTML = errors
                .map(function (error) {
                return error.message;
            })
                .join("\n");
        }
    });
}
document.querySelectorAll(".run-snippet").forEach(function (button) {
    var _a, _b;
    var snippetDiv = (_a = button === null || button === void 0 ? void 0 : button.parentNode) === null || _a === void 0 ? void 0 : _a.parentNode;
    var codeNode = (_b = button === null || button === void 0 ? void 0 : button.parentNode) === null || _b === void 0 ? void 0 : _b.nextSibling;
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
