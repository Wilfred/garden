# Garden website prototype

Preview: http://localhost:8105

This standalone homepage prototype uses the original homepage wording and
local copies of Lora and Protest Riot. The page, code cards, and editor share
a pale green palette. Snippet titles use bold Lora captions. Buttons have
quiet outlines, with a light green tint for Run.

Run, Edit, and Reset use the website's existing snippet code. Reset restores
the original example and its documentation links. The embedded playground
also supports live diagnostics and formatting. All execution uses
`https://playground.garden-lang.org` and requires a network connection.
The sandboxed execution recording comes from the existing website.

Build the JavaScript after installing the website dependencies:

```sh
cd website/static
npm ci
cd ../..
bash website/prototypes/build.sh
```

Start the preview from the repository root:

```sh
python3 -m http.server 8105 --bind 0.0.0.0 --directory website/prototypes/05-night-garden
```

For a browser on another machine, use the server machine's hostname in place
of `localhost`, or forward port 8105.
