
const execa = require('execa');
const installer = require('install-rstats');
const install_libcurl = require('./lib/install-libcurl');
const clone_pak = require('./lib/clone-pak');
const build_pak = require('./lib/build-pak');

async function install_libcurl() {
    await execa('brew', ['install', 'curl']);
    await execa('brew', ['install', 'brotli']);
    await execa('brew', ['install', 'rtmpdump']);
    await execa('brew', ['install', 'c-ares']);
    await execa('brew', ['install', 'nghttp2']);
}

async function build_macos(rversions) {
    console.log('::group::Installing R versions: ' + rversions.join(', '));
    await installer.install(rversions);
    console.log('::endgroup::')

    console.log('::group::Installing static libcurl from brew');
    await install_libcurl();
    console.log('::endgroup::')

    console.log('::group::Getting pak from GitHub');
    await clone_pak();
    console.log('::endgroup::')

    rversions.forEach(ver => {
        console.log('::group::Bulding pak for R ' + ver);
        await build_pak(ver);
        console.log('::endgroup::');
    });
}

module.exports = build_macos;
