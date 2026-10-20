#!/usr/bin/env bash
set -eu

cd "$(dirname "$0")/../static"
./node_modules/.bin/esbuild playground.ts --bundle --format=esm \
  --outfile="../prototypes/05-night-garden/playground.js"

cp ../prototypes/snippet-actions.html ../dist/snippet-actions.html

cp ../prototypes/footer-plants.html ../prototypes/footer-plants.css ../dist/
