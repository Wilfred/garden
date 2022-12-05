function lex(src) {
  return src.split(" ");
}

setInterval(() => {
  const src = document.querySelectorAll("#editor")[0].value;
  document.querySelectorAll("#result")[0].textContent = src;
}, 1000);
