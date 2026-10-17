#!/usr/bin/env bash
set -eu

cd "$(dirname "$0")/../static"
./node_modules/.bin/esbuild playground.ts --bundle --format=esm \
  --outfile="../prototypes/05-night-garden/playground.js"
