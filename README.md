# UI for Testing the Elm Compiler in Elm

<br>


## What Is It?

A simple `elm reactor`-like user interface to test
the [port of the Elm compiler](https://github.com/pithub/elm-compiler-in-elm)
from Haskell to Elm.

For more information see the recording of the August 2024 Elm Online Meetup:  
https://www.youtube.com/watch?v=OK9S_HUdReA.

<br>


## How to Run It?


#### Compile the App

```sh
make
```

or

```sh
elm make src/Main.elm --output dist/index.js
```


#### Start the CORS Proxy and File Server

```sh
cd scripts
npm install
node cors-proxy.js
```

#### Run the App

 Open [localhost:8088](http://localhost:8088) in your browser.  
 


#### Show Help Message

 Enter "h" or an empty line in the command input field.

<br>
