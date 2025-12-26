const { exec } = require('child_process');
const { connectionConfig } = require('../config/connectionConfig');
const { hasActiveSshSession } = require('../state/sessions');
const { wrapDockerCmd } = require('./process');

function runHostCommand(cmd) {
  return new Promise((resolve) => {
    const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
    if (useDocker) {
      const cfg = connectionConfig.docker;
      const escaped = cmd.replace(/'/g, `'\\''`);
      const full = wrapDockerCmd(`docker exec ${cfg.containerId} bash -lc '${escaped}'`);
      exec(full, { timeout: 30000, maxBuffer: 10 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
        resolve({ ok: true, stdout, stderr });
      });
    } else {
      const cfg = connectionConfig.ssh;
      const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';
      const escaped = cmd.replace(/"/g, '\\"');
      const full = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${escaped}"`;
      exec(full, { timeout: 30000, maxBuffer: 10 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
        resolve({ ok: true, stdout, stderr });
      });
    }
  });
}

module.exports = {
  runHostCommand
};
