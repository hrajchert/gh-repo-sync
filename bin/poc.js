#!/usr/bin/env node
const request = require('request-promise-native')
const {readFile} = require('fs')

const api = path => `https://api.github.com/${path}`
const getRepoUrl = (owner, repo) => api(`repos/${owner}/${repo}`)

const userAgent = {'User-Agent': 'gh-repo-sync'}
const maybeAuth = token => token ? {"Authorization": `Bearer ${token}`} : {}

const getRepo = (owner, repo, token) =>
    request({
        url: getRepoUrl(owner, repo),
        method: 'GET',
        headers: {
            ...userAgent,
            ...maybeAuth(token)
        }
    })
    .then(res => JSON.parse(res))


const readJSON = path => readFileP(path, 'UTF-8')
                            .then(buffer => buffer.toString())
                            .then(text => JSON.parse(text))

const validateConfig = (config) => {
    if (!config.organization) throw 'You need to specify an organization in the config'
    if (!config.repository) throw 'You need to specify a repository in the config'
    return config;
}

const showRepo = repo => `(Repository ${repo.full_name})`

function main () {
    readJSON('./config.json')
        .then(validateConfig)
        .then(config => getRepo(config.organization, config.repository, config.githubToken))
        .then(req => console.log(`Yeay = ${showRepo(req)}`))
        .catch(err => console.error('Err: ', '' + err))
}

main();

// Promised version of readFile.
function readFileP (path, opts) {
    return new Promise((resolve, reject) => {
        const cb = (err, buffer) => err ? reject(err) : resolve(buffer)

        if (opts) {
            readFile(path, opts, cb)
        } else {
            readFile(path, cb)
        }
    })
}