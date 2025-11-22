const http = require('http');
const { exec } = require('child_process');
const url = require('url');

const PORT = process.env.PORT || 3000;

const server = http.createServer((req, res) => {
  const parsedUrl = url.parse(req.url, true);

  if (parsedUrl.pathname === '/run') {
    exec('ls /', (error, stdout, stderr) => {
      res.writeHead(200, { 'Content-Type': 'application/json' });

      if (error) {
        res.end(JSON.stringify({
          success: false,
          error: error.message,
          stderr: stderr
        }));
        return;
      }

      const files = stdout.trim().split('\n');
      res.end(JSON.stringify({
        success: true,
        files: files
      }));
    });
  } else {
    res.writeHead(200, { 'Content-Type': 'text/plain' });
    res.end('hello world');
  }
});

server.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`);
});
