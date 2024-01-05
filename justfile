default:
    @just --list

watch:
    cargo watch -x build

release:
    #!/bin/bash

    set -ex

    VERSION=$(cargo metadata --format-version=1 | jq -r '.packages | .[] | select(.name == "garden-lang") | .version')
    git tag $VERSION
    git push --tags

    cargo set-version --bump minor

wasm:
    cd garden_lang_web && wasm-pack build --target web
    cp -r garden_lang_web/pkg website

web:
    cd website && python -m http.server
