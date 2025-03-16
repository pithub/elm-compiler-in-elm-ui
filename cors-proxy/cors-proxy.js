const express = require('express')
const cors = require('cors')
const { createProxyMiddleware } = require('http-proxy-middleware')
const fs = require('fs')
const os = require('os');
const staticDir = require('./static-dir');

const app = express()

// Proxy server
app.use('/proxy/', cors())
app.use('/proxy/', createProxyMiddleware({
    router: (req) => new URL(req.url.substring(1)),
    pathRewrite: (path, req) => (new URL(req.url.substring(1))).pathname,
    changeOrigin: true,
    followRedirects: true,
    logger: console
}))

// Serve directory trees or files from the local file system
app.use('/query/', (req, res) => {
    const path = req.url.startsWith('/~') ? req.url.replace('/~', os.homedir) : '..' + req.url
    if (fs.statSync(path).isDirectory()) {
        res.send(staticDir.readDir(path))
    } else {
        res.send(fs.readFileSync(path))
    }
})

// Serve other URLs from the local file system
app.use(express.static('../dist'))

app.listen(8088, () => {
    console.info('proxy server is running on port 8088')
})
