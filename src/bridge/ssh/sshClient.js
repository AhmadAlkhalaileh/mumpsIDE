let SSHClient = undefined;
let sshLoadError = null;
function ensureSshClient() {
  if (SSHClient !== undefined) return SSHClient;
  try {
    SSHClient = require('ssh2').Client;
  } catch (e) {
    SSHClient = null;
    sshLoadError = e;
  }
  return SSHClient;
}

module.exports = {
  ensureSshClient
};

Object.defineProperty(module.exports, 'sshLoadError', {
  enumerable: true,
  get: () => sshLoadError
});
