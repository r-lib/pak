
const execa = require('execa');

const exec = require('./exec');
const r = require('./r');
const get_workdir = require('./workdir');
const { promisify } = require('util');
const copy_file = promisify(require('fs').copyFile);

// There is apparently no way in brew to install or update a package
// automatically, so we have to do this hack. The final 'brew list'
// will fail if any of the packages are not present.

async function install_libcurl() {
    try { await exec('brew', 'update'); } catch (err) { }
    process.env['HOMEBREW_NO_AUTO_UPDATE'] = '1';
    try { await exec('brew', ['install', 'curl']);     } catch(err) { }
    try { await exec('brew', ['upgrade', 'curl']);     } catch(err) { }
    try { await exec('brew', ['install', 'brotli']);   } catch(err) { }
    try { await exec('brew', ['upgrade', 'brotli']);   } catch(err) { }
    try { await exec('brew', ['install', 'c-ares']);   } catch(err) { }
    try { await exec('brew', ['upgrade', 'c-ares']);   } catch(err) { }
    try { await exec('brew', ['install', 'nghttp2']);  } catch(err) { }
    try { await exec('brew', ['upgrade', 'nghttp2']);  } catch(err) { }
    await execa('brew', ['list', 'curl', 'brotli', 'c-ares', 'nghttp2', 'libidn2']);
    await patch_libcurl();
    await recompile_libcurl();
    await get_curl_package();
}

async function patch_libcurl() {
    console.log('Patching libcurl');
    const out = await execa('brew', ['edit', 'curl'], { 'env': { 'EDITOR': 'true' }});
    const formula = out.stdout.replace(/^Editing /, '');
    const formuladir = path.dirname(formula);
    const patch = path.join(__dirname, '/curl.rb.patch');
    const wd = process.cwd()
    try {
        process.chdir(formuladir);
        await exec('git', ['checkout', '--', 'curl.rb']);
        await exec('patch', ['-i', patch]);
    } finally {
        process.chdir(wd);
    }
}

async function recompile_libcurl () {
    await exec('brew', ['reinstall', 'curl', '-s']);
}

async function get_curl_package () {
    const wd = process.cwd();
    try {
        const workdir = await get_workdir();
        process.chdir(workdir);
        await r(undefined, 'download_curl()');
        const makefile = path.join(__dirname, 'Makevars-curl.in');
        await copy_file(makefile, 'curl/src/Makevars.in');
    } finally {
        process.chdir(wd);
    }
}

install_libcurl.patch_libcurl     = patch_libcurl;
install_libcurl.recompile_libcurl = recompile_libcurl;
install_libcurl.get_curl_package  = get_curl_package;

module.exports = install_libcurl;
