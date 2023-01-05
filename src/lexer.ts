import { developerError } from "./errors";

export const VARIABLE_NAME = /[a-z_][a-z0-9_]*/i;

export function lex(src: string): string[] {
  // TODO: Reuse VARIABLE_NAME constant above.
  const tokenRegex = /[0-9]+|([a-z_][a-z0-9_]*)|[+-]|;/gi;
  return Array.from(src.matchAll(tokenRegex), (m) => m[0]);
}

export function popToken(tokens: string[]): string | null {
  return tokens.shift() || null;
}

export function peekToken(tokens: string[]): string | null {
  return tokens[0] || null;
}

export function requireToken(
  tokens: string[],
  wantedToken: string
): string | null {
  const token = popToken(tokens);
  if (!token) {
    developerError("Expected " + wantedToken + " but got an empty input");
    return null;
  }

  if (token !== wantedToken) {
    developerError("Expected " + wantedToken + " but got " + token);
    return null;
  }

  return token;
}
