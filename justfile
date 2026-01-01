default:
    @just --list

# Build and run golden test files.
watch:
    REGENERATE=y cargo watch -x 't golden' --watch-when-idle
    # We use --watch-when-idle to avoid re-running simply because the
    # expected output of a test file was regenerated.

# Build and run all tests.
watch-all:
    # Only watch for changes between runs. This avoids us re-running
    # simply because the expected output of a test file was
    # regenerated.
    REGENERATE=y cargo watch -x t --watch-when-idle

# Tag a new release, trigger a crates.io publish on GitHub, and bump version.
release:
    #!/bin/bash

    set -ex

    VERSION=$(cargo metadata --format-version=1 | jq -r '.packages | .[] | select(.name == "garden-lang") | .version')
    git tag $VERSION
    git push --tags

    cargo set-version --bump minor

# Start a webserver serving the latest website build.
site-serve:
    #!/bin/bash

    cd website/dist
    python -m http.server &
    SERVER_PID=$!
    trap "kill $SERVER_PID 2>/dev/null" EXIT INT
    sleep 1
    xdg-open http://localhost:8000
    wait $SERVER_PID

# Build the website.
site-build:
    garden run website/build_site.gdn

    cd website/static && npm ci --silent && npm run build --silent

# Build a Docker image for the playground backend service.
build-docker:
    sudo docker build -t wilfred/garden-playground:latest -f playground/Dockerfile .

