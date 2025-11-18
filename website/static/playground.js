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
    var enclosingPre = button.parentNode.parentNode;
    var codeNode = button.parentNode.nextSibling;
    var src = codeNode.textContent;
    button.addEventListener("click", function (e) {
        evalSnippet(src);
    });
};
for (var _i = 0, _a = document.querySelectorAll(".run-snippet"); _i < _a.length; _i++) {
    var button = _a[_i];
    _loop_1(button);
}
console.log("Ready");
