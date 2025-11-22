const express = require('express');
const { exec } = require('child_process');

const app = express();
const PORT = process.env.PORT || 3000;

app.get('/', (req, res) => {
  res.send('hello world');
});

app.get('/run', (req, res) => {
  exec('ls /', (error, stdout, stderr) => {
    if (error) {
      return res.json({
        success: false,
        error: error.message,
        stderr: stderr
      });
    }

    const files = stdout.trim().split('\n');
    res.json({
      success: true,
      files: files
    });
  });
});

app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`);
});
