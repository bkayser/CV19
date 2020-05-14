const express = require('express')
const bodyParser = require('body-parser')
const app = express()
app.use(bodyParser.json())
app.use(express.static('dist')) // 'dist' is a directory relative to this file, you might want to look at the 'path' module for filepath system helpers.
// app.post('/some-path', (req, res) => res.status(200).send('OK'))
app.listen(8080)
