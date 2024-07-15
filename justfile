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

    cargo set-version --bump minor

wasm:
    cd garden-lang-web && wasm-pack build --target web
    cp -r garden-lang-web/pkg website

web:
    cd website && python -m http.server

publish:
    cd garden-lang-parser && cargo publish
    cargo publish
