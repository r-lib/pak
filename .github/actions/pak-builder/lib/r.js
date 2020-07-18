
const exec = require('./exec');

async function r(rversion, code) {
    var cmd;
    if (rversion === undefined) {
        cmd = "R";
    } else {
        cmd = 'R-' + rversion;
    }
    const inst = path.join(__dirname, 'installer.R');
    const scode = `source("${inst}");${code}`
    await exec(cmd, ["-q", "-e", scode]);
}

module.exports = r;