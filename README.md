# UI for Testing the Elm Compiler in Elm

## Usage:

Start CORS proxy and file server

```sh
cd cors-proxy
npm install
node cors-proxy.js
```

Compile Elm code

```sh
elm make src/Main.elm --output=dist/index.js
```

Run Elm code

Open [localhost:8088](http://localhost:8088) in your browser
