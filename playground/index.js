const express = require('express');
const cors = require('cors');
const { exec } = require('child_process');
const fs = require('fs');
const path = require('path');
const os = require('os');

const app = express();
const PORT = process.env.PORT || 3000;

app.use(cors());
app.use(express.json());

app.get('/', (req, res) => {
  res.send('hello world');
});

app.post('/run', (req, res) => {
  const { src } = req.body;

  if (src === undefined) {
    return res.status(400).json({
      success: false,
      error: 'src parameter is required'
    });
  }

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

app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`);
});
