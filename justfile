default:
    @just --list

# Build and run tests on all changes.
watch:
    REGENERATE=y GDN_TEST=y cargo watch -x b -x t

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
