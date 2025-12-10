default:
    @just --list

# Build and run golden test files.
watch:
    # Only watch for changes between runs. This avoids us re-running
    # simply because the expected output of a test file was
    # regenerated.
    REGENERATE=y cargo watch -x b -x 't run_test_files' --watch-when-idle

# Build and run all tests.
watch-all:
    # Only watch for changes between runs. This avoids us re-running
    # simply because the expected output of a test file was
    # regenerated.
    REGENERATE=y cargo watch -x b -x t --watch-when-idle

# Tag, push and publish a new release.
release:
    #!/bin/bash

    set -ex

    VERSION=$(cargo metadata --format-version=1 | jq -r '.packages | .[] | select(.name == "garden-lang") | .version')
    git tag $VERSION
    git push --tags

    cargo publish
    cargo set-version --bump minor

# Start a webserver serving the latest website build.
site-serve:
    cd website && python -m http.server

# Build the website.
site-build:
    mkdir -p website/dist
    cp website/*.html website/dist
    cp website/*.png website/dist
    cp website/*.mp4 website/dist

    mkdir -p website/dist/blog
    cp website/blog/*.html website/dist/blog

    mkdir -p website/dist/static
    cp website/static/*.css website/dist/static
    cp website/static/*.js website/dist/static

    mkdir -p website/dist/static/fonts
    cp website/static/fonts/*.woff2 website/dist/static/fonts

# Build a Docker image for the playground backend service.
build-docker:
    sudo docker build -t wilfred/garden-playground:latest -f playground/Dockerfile .

