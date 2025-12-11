const express = require('express');
const cors = require('cors');
const { exec } = require('child_process');
const fs = require('fs');
const path = require('path');
const os = require('os');

const app = express();
const PORT = process.env.PORT || 3000;

let gardenVersion = 'unknown';

// Get Garden version on startup
exec('garden --version', (error, stdout, stderr) => {
  if (!error && stdout) {
    gardenVersion = stdout.trim();
  } else {
    console.error('Failed to get Garden version:', error || stderr);
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
  const timestamp = new Date().toISOString();
  const codePreview = src.length > 200 ? src.substring(0, 200) + '...' : src;
  console.log(`[${timestamp}] Evaluating code (${src.length} chars):`);
  console.log(codePreview);
  console.log('---');

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
          console.error('Failed to delete temp file:', unlinkError);
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
  console.log(`Server running on port ${PORT}`);
});

process.on('SIGINT', () => {
  console.log('\nGot SIGINT');
  server.close(() => {
    process.exit(0);
  });
});

process.on('SIGTERM', () => {
  console.log('\nGot SIGTERM');
  server.close(() => {
    process.exit(0);
  });
});
