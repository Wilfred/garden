var _a;
console.log("hello world");
function evalSnippet(src) {
    fetch("/good.json")
        .then(function (response) { return response.text(); })
        .then(function (responseText) {
        var evalResult = JSON.parse(responseText);
        console.log("Evaluating: " + src);
        console.log(evalResult);
    });
}
var _loop_1 = function (button) {
    console.log(button);
    var codeNode = (_a = button === null || button === void 0 ? void 0 : button.parentNode) === null || _a === void 0 ? void 0 : _a.nextSibling;
    var src = (codeNode === null || codeNode === void 0 ? void 0 : codeNode.textContent) || "";
    button.addEventListener("click", function (_e) {
        evalSnippet(src);
    });
};
for (var _i = 0, _b = document.querySelectorAll(".run-snippet"); _i < _b.length; _i++) {
    var button = _b[_i];
    _loop_1(button);
}
console.log("Ready");
