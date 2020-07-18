
const temp = require('temp-dir');
const mkdirp = require('mkdirp')

async function workdir() {
    const workdir = process.env['GITHUB_WORKSPACE'] || temp + path.sep + 'pak-builder';
    await mkdirp(workdir);
    return(workdir);
}

module.exports = workdir;
