default:
    @just --list

watch:
    REGENERATE=y GDN_NO_ABS_PATH=y cargo watch -x b -x t

tag_then_bump:
    #!/bin/bash

    set -ex

    VERSION=$(cargo metadata --format-version=1 | jq -r '.packages | .[] | select(.name == "garden-lang") | .version')
    git tag $VERSION
    git push --tags

    cargo set-version --bump minor

web:
    cd website && python -m http.server

publish:
    cd garden-lang-parser && cargo publish
    cargo publish
