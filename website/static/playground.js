var _a, _b;
function evalSnippet(src, snippetDiv) {
    // TODO: why doesn't typescript know about the hidden field on Element?
    var snippetElement = snippetDiv;
    snippetElement.hidden = false;
    fetch("/good.json")
        .then(function (response) { return response.text(); })
        .then(function (responseText) {
        var evalResult = JSON.parse(responseText);
        snippetDiv.innerHTML = responseText;
        console.log("Evaluating: " + src);
        console.log(evalResult);
    });
}
var _loop_1 = function (button) {
    console.log(button);
    var snippetDiv = (_a = button === null || button === void 0 ? void 0 : button.parentNode) === null || _a === void 0 ? void 0 : _a.parentNode;
    var codeNode = (_b = button === null || button === void 0 ? void 0 : button.parentNode) === null || _b === void 0 ? void 0 : _b.nextSibling;
    var src = (codeNode === null || codeNode === void 0 ? void 0 : codeNode.textContent) || "";
    button.addEventListener("click", function (_e) {
        if (snippetDiv) {
            var outputDiv = snippetDiv.querySelector(".snippet-output");
            if (outputDiv) {
                evalSnippet(src, outputDiv);
            }
        }
    });
};
for (var _i = 0, _c = document.querySelectorAll(".run-snippet"); _i < _c.length; _i++) {
    var button = _c[_i];
    _loop_1(button);
}
