
const execa = require('execa');

async function exec(file, args, options) {
    options = options || {};
    options['stdout'] = 'inherit';
    options['stderr'] = 'inherit';
    await execa(file, args, options);
}

module.exports = exec;
