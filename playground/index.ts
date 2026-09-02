import express from "express";
import type { Request, Response } from "express";
import cors from "cors";
import { exec } from "child_process";
import fs from "fs";
import path from "path";
import os from "os";
import pino from "pino";
import { LRUCache } from "lru-cache";

const logger = pino({
  level: process.env["LOG_LEVEL"] || "info",
  transport: {
    target: "pino-pretty",
    options: {
      colorize: true,
      translateTime: "SYS:standard",
      ignore: "pid,hostname",
    },
  },
});

const app = express();
const PORT = process.env["PORT"] || 3000;

let gardenVersion = "unknown";

// One JSON line of `garden playground-run` output. The evaluated
// value, or something the program printed. See `PlaygroundResponse` in
// src/sandboxed_playground.rs and `ResponseKind` in src/json_session.rs.
type RunResult =
  | { error: string | null; value: string | null }
  | { printed: { s: string } }
  | { printed_stderr: { s: string } };

// One JSON line of `garden check --json` output. See `CheckDiagnostic`
// in src/syntax_check.rs. Line numbers are 1-indexed.
interface CheckDiagnostic {
  line_number: number;
  end_line_number: number;
  column: number;
  end_column: number;
  message: string;
  severity: "error" | "warning";
}

interface RunResponse {
  success: boolean;
  results: RunResult[];
}

// Cache /run responses so common snippets from the website — hello
// world and other landing-page examples — feel instant on repeat
// visits without re-spawning the garden CLI each time.
const runCache = new LRUCache<string, RunResponse>({
  max: parseInt(process.env["RUN_CACHE_CAPACITY"] ?? "", 10) || 1000,
});

// Skip the cache when the program imports a built-in whose output
// can vary between runs (filesystem state, RNG). __reflect is
// deterministic for a fixed source, so it's fine to cache.
const NONDETERMINISTIC_IMPORT = /import\s+"(?:__fs|__random)\.gdn"/;

function isCacheable(src: string): boolean {
  return !NONDETERMINISTIC_IMPORT.test(src);
}

const SOURCE_NAME = "playground.gdn";

// Write `src` to `playground.gdn` in a new temporary directory, and
// call `callback` with that directory.
function writeTempSource(
  src: string,
  callback: (error: NodeJS.ErrnoException | null, tmpDir: string) => void,
): void {
  fs.mkdtemp(path.join(os.tmpdir(), "garden-"), (mkdtempError, tmpDir) => {
    if (mkdtempError) {
      return callback(mkdtempError, tmpDir);
    }

    fs.writeFile(path.join(tmpDir, SOURCE_NAME), src, (writeError) => {
      callback(writeError, tmpDir);
    });
  });
}

// Delete a directory created by `writeTempSource`.
function removeTempDir(tmpDir: string): void {
  fs.rm(tmpDir, { recursive: true, force: true }, (rmError) => {
    if (rmError) {
      logger.error(
        { error: rmError.message, tmpDir },
        "Failed to delete temp directory",
      );
    }
  });
}

// The source code submitted, or null if the request didn't include
// any.
function requestSource(req: Request): string | null {
  const src: unknown = (req.body as { src?: unknown }).src;
  return typeof src === "string" ? src : null;
}

function missingSource(res: Response): void {
  res.status(400).json({
    success: false,
    error: "src parameter is required",
  });
}

// Get Garden version on startup
exec("garden --version", (error, stdout, stderr) => {
  if (!error && stdout) {
    gardenVersion = stdout.trim();
    logger.info({ version: gardenVersion }, "Garden version detected");
  } else {
    logger.error(
      { error: error?.message, stderr },
      "Failed to get Garden version",
    );
  }
});

app.use(cors());
app.use(express.json());

app.get("/", (_req, res) => {
  res.json({
    name: "Garden Playground API",
    description:
      "REST API for executing Garden programming language code in a sandboxed environment",
    version: gardenVersion,
    endpoints: {
      "POST /run": {
        description: "Execute Garden code and return results",
        parameters: {
          src: "Garden source code to execute (string, required)",
        },
        returns:
          "JSON object with success status and execution results or error",
      },
      "POST /check": {
        description:
          "Statically check Garden code for issues without running it",
        parameters: {
          src: "Garden source code to check (string, required)",
        },
        returns: "JSON object with success status and a list of diagnostics",
      },
      "POST /format": {
        description: "Format Garden code and return the re-indented source",
        parameters: {
          src: "Garden source code to format (string, required)",
        },
        returns: "JSON object with success status and the formatted source",
      },
    },
  });
});

app.post("/run", (req, res) => {
  const src = requestSource(req);

  if (src === null) {
    missingSource(res);
    return;
  }

  // Log the submitted code
  const codePreview = src.length > 200 ? src.substring(0, 200) + "..." : src;
  logger.info(
    {
      codeLength: src.length,
      codePreview,
    },
    "Evaluating code",
  );

  const cacheable = isCacheable(src);
  if (cacheable) {
    const cached = runCache.get(src);
    if (cached) {
      logger.info({ codeLength: src.length }, "Cache hit");
      res.json(cached);
      return;
    }
  }

  writeTempSource(src, (writeError, tmpDir) => {
    if (writeError) {
      res.json({
        success: false,
        error: writeError.message,
      });
      return;
    }

    exec(
      `garden playground-run ${SOURCE_NAME}`,
      { cwd: tmpDir },
      (execError, stdout, stderr) => {
        removeTempDir(tmpDir);

        if (execError) {
          res.json({
            success: false,
            error: `Execution failed: ${execError.message}`,
            stderr: stderr,
          });
          return;
        }

        try {
          // Parse JSON lines from Garden output
          const lines = stdout
            .trim()
            .split("\n")
            .filter((line) => line.length > 0);
          const results = lines.map((line) => JSON.parse(line) as RunResult);

          const response = {
            success: true,
            results: results,
          };
          if (cacheable) {
            runCache.set(src, response);
          }
          res.json(response);
        } catch (parseError) {
          res.json({
            success: false,
            error: `Failed to parse Garden output: ${(parseError as Error).message}`,
            rawOutput: stdout,
          });
        }
      },
    );
  });
});

app.post("/check", (req, res) => {
  const src = requestSource(req);

  if (src === null) {
    missingSource(res);
    return;
  }

  const codePreview = src.length > 200 ? src.substring(0, 200) + "..." : src;
  logger.info(
    {
      codeLength: src.length,
      codePreview,
    },
    "Checking code",
  );

  writeTempSource(src, (writeError, tmpDir) => {
    if (writeError) {
      res.json({
        success: false,
        error: writeError.message,
      });
      return;
    }

    exec(
      `garden check --json ${SOURCE_NAME}`,
      { cwd: tmpDir },
      (execError, stdout, stderr) => {
        removeTempDir(tmpDir);

        // `garden check` exits non-zero when there are diagnostics, so
        // we don't treat that as a failure here — only a real spawn
        // error counts. `execError.code` is the process exit code; any
        // other error (e.g. ENOENT) means the binary couldn't be run.
        if (execError && execError.code === undefined) {
          res.json({
            success: false,
            error: `Check failed: ${execError.message}`,
            stderr: stderr,
          });
          return;
        }

        try {
          const lines = stdout.split("\n").filter((line) => line.length > 0);
          const diagnostics = lines.map(
            (line) => JSON.parse(line) as CheckDiagnostic,
          );
          res.json({
            success: true,
            diagnostics: diagnostics,
          });
        } catch (parseError) {
          res.json({
            success: false,
            error: `Failed to parse Garden output: ${(parseError as Error).message}`,
            rawOutput: stdout,
          });
        }
      },
    );
  });
});

app.post("/format", (req, res) => {
  const src = requestSource(req);

  if (src === null) {
    missingSource(res);
    return;
  }

  const codePreview = src.length > 200 ? src.substring(0, 200) + "..." : src;
  logger.info(
    {
      codeLength: src.length,
      codePreview,
    },
    "Formatting code",
  );

  writeTempSource(src, (writeError, tmpDir) => {
    if (writeError) {
      res.json({
        success: false,
        error: writeError.message,
      });
      return;
    }

    exec(
      `garden format ${SOURCE_NAME}`,
      { cwd: tmpDir },
      (execError, stdout, stderr) => {
        removeTempDir(tmpDir);

        if (execError) {
          res.json({
            success: false,
            error: `Format failed: ${execError.message}`,
            stderr: stderr,
          });
          return;
        }

        res.json({
          success: true,
          formatted: stdout,
        });
      },
    );
  });
});

const server = app.listen(PORT, () => {
  logger.info({ port: PORT }, "Server running");
});

process.on("SIGINT", () => {
  logger.info("Got SIGINT, shutting down gracefully");
  server.close(() => {
    logger.info("Server closed");
    process.exit(0);
  });
});

process.on("SIGTERM", () => {
  logger.info("Got SIGTERM, shutting down gracefully");
  server.close(() => {
    logger.info("Server closed");
    process.exit(0);
  });
});
