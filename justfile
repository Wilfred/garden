default:
    @just --list

watch:
    REGENERATE=y cargo watch -x b -x t

release:
    #!/bin/bash

    set -ex

    VERSION=$(cargo metadata --format-version=1 | jq -r '.packages | .[] | select(.name == "garden-lang") | .version')
    git tag $VERSION
    git push --tags

    cargo publish
    cargo set-version --bump minor

web:
    cd website && python -m http.server
