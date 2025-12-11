const express = require('express');
const cors = require('cors');
const { exec } = require('child_process');
const fs = require('fs');
const path = require('path');
const os = require('os');
const pino = require('pino');

const logger = pino({
  level: process.env.LOG_LEVEL || 'info',
  transport: {
    target: 'pino-pretty',
    options: {
      colorize: true,
      translateTime: 'SYS:standard',
      ignore: 'pid,hostname'
    }
  }
});

const app = express();
const PORT = process.env.PORT || 3000;

let gardenVersion = 'unknown';

// Get Garden version on startup
exec('garden --version', (error, stdout, stderr) => {
  if (!error && stdout) {
    gardenVersion = stdout.trim();
    logger.info({ version: gardenVersion }, 'Garden version detected');
  } else {
    logger.error({ error: error?.message, stderr }, 'Failed to get Garden version');
  }
});

app.use(cors());
app.use(express.json());

app.get('/', (req, res) => {
  res.json({
    name: 'Garden Playground API',
    description: 'REST API for executing Garden programming language code in a sandboxed environment',
    version: gardenVersion,
    endpoints: {
      'POST /run': {
        description: 'Execute Garden code and return results',
        parameters: {
          src: 'Garden source code to execute (string, required)'
        },
        returns: 'JSON object with success status and execution results or error'
      }
    }
  });
});

app.post('/run', (req, res) => {
  const { src } = req.body;

  if (src === undefined || src === null) {
    return res.status(400).json({
      success: false,
      error: 'src parameter is required'
    });
  }

  // Log the submitted code
  const codePreview = src.length > 200 ? src.substring(0, 200) + '...' : src;
  logger.info({
    codeLength: src.length,
    codePreview
  }, 'Evaluating code');

  // Create a temporary file path with .gdn extension
  const tmpFile = path.join(os.tmpdir(), `garden-tmp-${Date.now()}-${Math.random().toString(36).substr(2, 9)}.gdn`);

  // Write content to temp file
  fs.writeFile(tmpFile, src, (writeError) => {
    if (writeError) {
      return res.json({
        success: false,
        error: writeError.message
      });
    }

    exec(`garden playground-run "${tmpFile}"`, (execError, stdout, stderr) => {
      // Delete the temp file
      fs.unlink(tmpFile, (unlinkError) => {
        if (unlinkError) {
          logger.error({ error: unlinkError.message, tmpFile }, 'Failed to delete temp file');
        }
      });

      if (execError) {
        return res.json({
          success: false,
          error: `Execution failed: ${execError.message}`,
          stderr: stderr
        });
      }

      try {
        // Parse JSON lines from Garden output
        const lines = stdout.trim().split('\n').filter(line => line.length > 0);
        const results = lines.map(line => JSON.parse(line));

        res.json({
          success: true,
          results: results
        });
      } catch (parseError) {
        return res.json({
          success: false,
          error: `Failed to parse Garden output: ${parseError.message}`,
          rawOutput: stdout
        });
      }
    });
  });
});

const server = app.listen(PORT, () => {
  logger.info({ port: PORT }, 'Server running');
});

process.on('SIGINT', () => {
  logger.info('Got SIGINT, shutting down gracefully');
  server.close(() => {
    logger.info('Server closed');
    process.exit(0);
  });
});

process.on('SIGTERM', () => {
  logger.info('Got SIGTERM, shutting down gracefully');
  server.close(() => {
    logger.info('Server closed');
    process.exit(0);
  });
});
