const express = require('express');
const { exec } = require('child_process');
const fs = require('fs');
const path = require('path');
const os = require('os');

const app = express();
const PORT = process.env.PORT || 3000;

app.use(express.json());

app.get('/', (req, res) => {
  res.send('hello world');
});

app.post('/run', (req, res) => {
  const { src } = req.body;

  if (!src) {
    return res.status(400).json({
      success: false,
      error: 'src parameter is required'
    });
  }

  // Create a temporary file path
  const tmpFile = path.join(os.tmpdir(), `garden-tmp-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`);

  // Write content to temp file
  fs.writeFile(tmpFile, src, (writeError) => {
    if (writeError) {
      return res.json({
        success: false,
        error: writeError.message
      });
    }

    // Run ls -l on the temp file
    exec(`ls -l "${tmpFile}"`, (execError, stdout, stderr) => {
      // Delete the temp file
      fs.unlink(tmpFile, (unlinkError) => {
        if (unlinkError) {
          console.error('Failed to delete temp file:', unlinkError);
        }
      });

      if (execError) {
        return res.json({
          success: false,
          error: execError.message,
          stderr: stderr
        });
      }

      res.json({
        success: true,
        output: stdout.trim()
      });
    });
  });
});

app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`);
});
