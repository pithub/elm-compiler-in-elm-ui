const express = require('express')
const cors = require('cors')
const { createProxyMiddleware } = require('http-proxy-middleware')
const fs = require('fs')
const os = require('os');

const app = express()

app.use('/proxy/', cors())
app.use('/proxy/', createProxyMiddleware({
    router: (req) => new URL(req.url.substring(1)),
    pathRewrite: (path, req) => (new URL(req.url.substring(1))).pathname,
    changeOrigin: true,
    followRedirects: true,
    logger: console
}))

// Serve directory listings or files from the local file system
app.use('/query/', (req, res) => {
    const path = req.url.startsWith('/~') ? req.url.replace('/~', os.homedir) : '..' + req.url
    if (fs.statSync(path).isDirectory()) {
        const dirs = []
        const files = []
        fs.readdirSync(path).forEach(file => {
            const stats = fs.statSync(path + '/' + file)
            if (stats.isDirectory()) {
                dirs.push({ name: file, mtime: stats.mtimeMs })
            } else {
                files.push({ name: file, mtime: stats.mtimeMs, size: stats.size })
            }
        })
        res.send({ dirs: dirs, files: files })
    } else {
        const content = fs.readFileSync(path)
        res.send(content)
    }
})

// Serve other URLs from the local file system
app.use(express.static('../dist'))

app.listen(8088, () => {
    console.info('proxy server is running on port 8088')
})
