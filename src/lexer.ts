export function lex(src: string): string[] {
  const tokenRegex = /[0-9]+|[+-]|;/g;
  return Array.from(src.matchAll(tokenRegex), (m) => m[0]);
}

export function popToken(tokens: string[]): string | null {
  return tokens.shift() || null;
}

export function peekToken(tokens: string[]): string | null {
  return tokens[0] || null;
}
