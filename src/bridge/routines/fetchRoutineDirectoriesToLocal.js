const fs = require('fs');
const { exec } = require('child_process');
const { shellQuote } = require('../config/paths');
const { connectionConfig } = require('../config/connectionConfig');
const { hasActiveSshSession } = require('../state/sessions');
const { wrapDockerCmd } = require('../util/process');

function getRoutineDirs() {
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;
  const dirs = [];
  const base = (cfg.routinesPath || '').trim();
  if (base) dirs.push(base);
  const rpc = (cfg.rpcRoutinesPath || '').trim();
  if (rpc) {
    rpc.split(/\s+/).filter(Boolean).forEach(p => {
      if (!dirs.includes(p)) dirs.push(p);
    });
  }

  // Universal mode fallback: use /workspace if no paths are configured
  if (dirs.length === 0) {
    dirs.push('/workspace');
  }

  return dirs;
}

function fetchRoutineDirectoriesToLocal(targetRoot) {
  const routineDirs = getRoutineDirs();
  if (!routineDirs.length) return Promise.resolve({ ok: false, error: 'No routines path configured' });

  fs.mkdirSync(targetRoot, { recursive: true });
  const target = targetRoot.replace(/'/g, `'\\''`);
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;
  const basePath = (cfg.basePath || '').trim();
  const allInBase = basePath && routineDirs.every(d => d.startsWith(`${basePath}/`));

  // Preserve folder structure: fetch as "localr routines" to maintain directory hierarchy
  const relPaths = allInBase
    ? routineDirs.map(d => d.slice(basePath.length + 1)).join(' ')
    : '';

  // Build tar command that preserves directory structure
  // This will create targetRoot/localr/*.m and targetRoot/routines/*.m
  const tarCmdInner = allInBase
    ? `cd ${shellQuote(basePath)} && tar -cf - ${relPaths}`
    : `tar -cf - ${routineDirs.map(d => `"${d.replace(/"/g, '\\"')}"`).join(' ')}`;

  return new Promise((resolve) => {
    if (useDocker) {
      const dockerCmd = `docker exec ${cfg.containerId} bash -lc "${tarCmdInner.replace(/"/g, '\\"')}"`;
      const full = `${wrapDockerCmd(dockerCmd)} | tar -xf - -C '${target}'`;
      exec(full, { timeout: 60000, maxBuffer: 20 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
        resolve({ ok: true });
      });
    } else {
      const sshPass = cfg.password ? `sshpass -p '${(cfg.password || '').replace(/'/g, `'\\''`)}' ` : '';
      const sshCmd = `${sshPass}ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${tarCmdInner.replace(/"/g, '\\"')}" | tar -xf - -C '${target}'`;
      exec(sshCmd, { timeout: 60000, maxBuffer: 20 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
        resolve({ ok: true });
      });
    }
  });
}

module.exports = {
  getRoutineDirs,
  fetchRoutineDirectoriesToLocal
};
