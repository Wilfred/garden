# Garden website preview

The current design is applied through `website/template.html`,
`website/static/style.css`, and the code-block renderer in `website/markdown.gdn`.
The standalone prototype is retained in `05-night-garden/` for reference.

Build the full site from the repository root:

```sh
cargo build
./target/debug/garden run website/build_site.gdn
cd website/static
npm ci
npm run build
cd ../..
```

Serve the generated site on the existing preview port:

```sh
python3 -m http.server 8105 --bind 0.0.0.0 --directory website/dist
```

Run, Edit, and Reset use the website's snippet code. The playground also
supports live diagnostics and formatting. Execution uses
`https://playground.garden-lang.org` and requires a network connection.

To rebuild the standalone reference's JavaScript, run
`bash website/prototypes/build.sh` after installing the website dependencies.

For a browser on another machine, use the server machine's hostname in place
of `localhost`, or forward port 8105.

## Snippet action comparison

`snippet-actions.html` compares six live action layouts using the
`Option::or_value()` example. After building the main site, run
`bash website/prototypes/build.sh`, then visit
http://localhost:8105/snippet-actions.html. The comparison uses the main site's
stylesheet and playground bundle.
