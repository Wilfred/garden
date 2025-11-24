default:
    @just --list

# Build and run golden test files.
watch:
    # Only watch for changes between runs. This avoids us re-running
    # simply because the expected output of a test file was
    # regenerated.
    REGENERATE=y GDN_TEST=y cargo watch -x b -x 't run_test_files' --watch-when-idle

# Build and run all tests.
watch-all:
    # Only watch for changes between runs. This avoids us re-running
    # simply because the expected output of a test file was
    # regenerated.
    REGENERATE=y GDN_TEST=y cargo watch -x b -x t --watch-when-idle

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
web:
    cd website && python -m http.server
