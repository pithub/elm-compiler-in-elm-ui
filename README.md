# UI for Testing the Elm Compiler in Elm

<br>


## What Is It?

A port of the Elm compiler from Haskell to Elm,
together with a simple Elm reactor like user interface.

For more information see the recording of the August 2024 Elm Online Meetup:  
https://www.youtube.com/watch?v=OK9S_HUdReA.

<br>


## How to Run It?


#### Compile the App

```sh
elm make src/Main.elm --output dist/index.js
```


#### Start the CORS Proxy and File Server

```sh
cd cors-proxy
npm install
node cors-proxy.js
```

#### Run the App

 Open [localhost:8088](http://localhost:8088) in your browser.  
 


#### Show Help Message

 Enter "h" or an empty line in the command input field.

<br>


## What's next?

As of today, I have no plans to further develop this application,
except perhaps to fix the remaining issues.
Instead, I want to use parts of the code for my "interactive
tutorials" as I talked about in the Elm Online Meetup.

If there is enough and serious interest, I could try to extract
self-contained parts of the code and publish them,
for example as Elm packages.

<br>
