const { exec } = require('child_process');
const { log: dbgLog } = require('../../../utils/debug-log');
const { connectionConfig } = require('../config/connectionConfig');
const { hasActiveSshSession } = require('../state/sessions');
const { wrapDockerCmd } = require('../util/process');
const {
  discoverVistaProfilePaths,
  applyVistaProfilePathsToConfig
} = require('../vista/vistaProfilePaths');

// Discover gld path from the container/host
async function discoverGldPath() {
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;

  // If gldPath is already configured, use it
  if (cfg.gldPath) {
    return cfg.gldPath;
  }

  // Prefer vista-profile when available (more accurate than envKey defaults)
  try {
    const discovered = await discoverVistaProfilePaths({ timeoutMs: 4000 });
    if (discovered?.ok && discovered.gldPath) {
      applyVistaProfilePathsToConfig(cfg, discovered, { override: true });
      return cfg.gldPath;
    }
  } catch (_) { }

  // Fallback: search for a global directory file anywhere under /var/worldvista.
  // (Avoid env-specific hardcoding like /var/worldvista/prod/<env>/globals/mumps.gld.)
  const discoveryCmd = "find /var/worldvista -name 'mumps.gld' -type f 2>/dev/null | head -1";

  return new Promise((resolve) => {
    if (useDocker) {
      const escaped = discoveryCmd.replace(/'/g, `'\\''`);
      const fullCmd = wrapDockerCmd(`docker exec ${cfg.containerId} bash -c '${escaped}'`);
      exec(fullCmd, { timeout: 10000 }, (err, stdout) => {
        const gldPath = (stdout || '').trim();
        dbgLog('[DEBUG] discoverGldPath result:', gldPath || '(empty)');
        resolve(gldPath || '');
      });
    } else {
      const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';
      const fullCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${discoveryCmd.replace(/"/g, '\\"')}"`;
      exec(fullCmd, { timeout: 10000 }, (err, stdout) => {
        const gldPath = (stdout || '').trim();
        dbgLog('[DEBUG] discoverGldPath result:', gldPath || '(empty)');
        resolve(gldPath || '');
      });
    }
  });
}

module.exports = {
  discoverGldPath
};
