
const exec = require('./exec');
const { promisify } = require('util');
const rimraf = promisify(require('rimraf'));
const get_workdir = require('./workdir');

const repo = process.env['GITHUB_REPOSITORY'] || 'r-lib/pak';
const url = 'https://github.com/' + repo + '.git';
const sha = process.env['GITHUB_SHA'] || 'master';

async function clone_pak() {
    const wd = process.cwd()
    try {
        workdir = await get_workdir();
        process.chdir(workdir);
        await rimraf('pak');
        await exec('git', ['clone', url, 'pak']);
        process.chdir('pak');
        await exec('git', ['checkout', sha]);
        console.log('pak is in ' + process.cwd());
    } finally {
        process.chdir(wd);
    }
}

module.exports = clone_pak;
