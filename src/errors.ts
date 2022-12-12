export function developerError(msg: string): void {
  document.querySelectorAll("#error")[0].textContent = msg;
}
