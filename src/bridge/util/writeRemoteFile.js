const { exec } = require('child_process');
const { connectionConfig } = require('../config/connectionConfig');
const { hasActiveSshSession } = require('../state/sessions');
const { wrapDockerCmd } = require('./process');

function writeRemoteFile(remotePath, content) {
  return new Promise((resolve) => {
    try {
      const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();

      let cmdProc;

      if (useDocker) {
        const cfg = connectionConfig.docker;
        if (!cfg || !cfg.containerId) {
          return resolve({ ok: false, error: 'Docker container ID not configured' });
        }
        const b64 = Buffer.from(content, 'utf8').toString('base64');
        const fullCmd = wrapDockerCmd(`docker exec -i ${cfg.containerId} bash -c "mkdir -p $(dirname ${remotePath}) && base64 -d > ${remotePath}"`);

        cmdProc = exec(fullCmd, (err, stdout, stderr) => {
          if (err) resolve({ ok: false, error: err.message, stderr });
          else resolve({ ok: true });
        });

        if (!cmdProc || !cmdProc.stdin) {
          return resolve({ ok: false, error: 'Failed to create stdin stream for Docker exec' });
        }

        cmdProc.stdin.on('error', (e) => {
          console.error('writeRemoteFile STDIN error (Docker):', e);
        });
        cmdProc.stdin.write(b64);
        cmdProc.stdin.end();

      } else {
        const cfg = connectionConfig.ssh;
        if (!cfg || !cfg.host) {
          return resolve({ ok: false, error: 'SSH host not configured' });
        }
        const b64 = Buffer.from(content, 'utf8').toString('base64');
        const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';
        const remoteCmd = `mkdir -p $(dirname ${remotePath}) && base64 -d > ${remotePath}`;
        const fullCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${remoteCmd.replace(/"/g, '\\"')}"`;

        cmdProc = exec(fullCmd, (err, stdout, stderr) => {
          if (err) resolve({ ok: false, error: err.message, stderr });
          else resolve({ ok: true });
        });

        if (!cmdProc || !cmdProc.stdin) {
          return resolve({ ok: false, error: 'Failed to create stdin stream for SSH exec' });
        }

        cmdProc.stdin.on('error', (e) => {
          console.error('writeRemoteFile STDIN error (SSH):', e);
        });
        cmdProc.stdin.write(b64);
        cmdProc.stdin.end();
      }
    } catch (err) {
      console.error('[writeRemoteFile] CRITICAL ERROR:', err);
      resolve({ ok: false, error: 'writeRemoteFile crashed: ' + err.message });
    }
  });
}

module.exports = {
  writeRemoteFile
};

