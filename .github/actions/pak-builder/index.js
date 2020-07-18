
const core = require('@actions/core')

const rversions = [ "3.3", "3.4", "3.5", "3.6", "4.0", "devel" ]

async function run() {
    try {
        if (process.platform === 'win32') {
            var build_windows = require('./lib/build-windows');
            build_windows(rversions);
        } else if (process.platform === 'darwin') {
            var build_macos = require('./lib/build-macos');
            build_macos(rversions);
        } else if (process.platform === 'linux') {
            var build_linux = require('./lib/build-linux');
            build_linux(rversions);
        } else {
            throw new Error('Unsupported OS, only Windows, Linux and macOS are supported');
        }
        console.log("::endgroup::")
    } catch (error) {
        console.log(error);
        core.setFailed(error.message);
    }
}

run()
