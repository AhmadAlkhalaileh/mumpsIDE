const { exec } = require('child_process');
const { log: dbgLog } = require('../../../utils/debug-log');
const { connectionConfig } = require('../config/connectionConfig');
const { hasActiveSshSession } = require('../state/sessions');
const { wrapDockerCmd } = require('../util/process');

// Discover gld path from the container/host
async function discoverGldPath() {
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;

  // If gldPath is already configured, use it
  if (cfg.gldPath) {
    return cfg.gldPath;
  }

  // Try to discover from common locations
  const discoveryCmd = "test -f /var/worldvista/prod/hakeem/globals/mumps.gld && echo '/var/worldvista/prod/hakeem/globals/mumps.gld' || (find /var/worldvista -name 'mumps.gld' 2>/dev/null | head -1)";

  return new Promise((resolve) => {
    if (useDocker) {
      const escaped = discoveryCmd.replace(/'/g, `'\\''`);
      const fullCmd = wrapDockerCmd(`docker exec ${cfg.containerId} bash -c '${escaped}'`);
      exec(fullCmd, { timeout: 10000 }, (err, stdout) => {
        const gldPath = (stdout || '').trim();
        dbgLog('[DEBUG] discoverGldPath result:', gldPath || '(empty)');
        resolve(gldPath || '/var/worldvista/prod/hakeem/globals/mumps.gld'); // Fallback
      });
    } else {
      const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';
      const fullCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${discoveryCmd.replace(/"/g, '\\"')}"`;
      exec(fullCmd, { timeout: 10000 }, (err, stdout) => {
        const gldPath = (stdout || '').trim();
        dbgLog('[DEBUG] discoverGldPath result:', gldPath || '(empty)');
        resolve(gldPath || '/var/worldvista/prod/hakeem/globals/mumps.gld'); // Fallback
      });
    }
  });
}

module.exports = {
  discoverGldPath
};
