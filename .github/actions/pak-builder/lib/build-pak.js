
const exec = require('./exec');
const r = require('./r');
const { promisify } = require('util');
const rimraf = promisify(require('rimraf'));
const get_workdir = require('./workdir');

async function build_in_workdir(rversion) {
    await r(rversion, 'main()');
}

async function build_pak(rversion) {
    const wd = process.cwd();
    try {
        const workdir = await get_workdir();
        console.log("Building pak in " + workdir);
        process.chdir(workdir);
        return await build_in_workdir(rversion);
    } finally {
        process.chdir(wd);
    }
}

module.exports = build_pak;
