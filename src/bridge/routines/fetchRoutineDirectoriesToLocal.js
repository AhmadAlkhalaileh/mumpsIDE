const fs = require('fs');
const { exec } = require('child_process');
const { shellQuote } = require('../config/paths');
const { connectionConfig } = require('../config/connectionConfig');
const { hasActiveSshSession } = require('../state/sessions');
const { wrapDockerCmd } = require('../util/process');
const { discoverVistaProfilePaths, applyVistaProfilePathsToConfig } = require('../vista/vistaProfilePaths');

function getRoutineDirs() {
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;
  const dirs = [];

  // Strategy 1: Use routinesPath (primary path - usually localr)
  const base = (cfg.routinesPath || '').trim();
  if (base && base.startsWith('/')) {
    dirs.push(base);
  }

  // Strategy 2: Parse rpcRoutinesPath (space-separated list: "localr routines")
  const rpc = (cfg.rpcRoutinesPath || '').trim();
  if (rpc) {
    const rpcDirs = rpc.split(/\s+/).filter(Boolean).filter(p => p.startsWith('/'));
    rpcDirs.forEach(p => {
      if (!dirs.includes(p)) {
        dirs.push(p);
      }
    });
  }

  // Strategy 3: Construct from basePath if available
  if (dirs.length === 0 && cfg.basePath) {
    const basePath = String(cfg.basePath).replace(/\/+$/, '');
    const localrCandidate = `${basePath}/localr`;
    const routinesCandidate = `${basePath}/routines`;

    // Note: We can't check if these exist here (would require async),
    // but they'll be validated by the caller
    dirs.push(localrCandidate, routinesCandidate);
  }

  // Universal mode fallback: use /workspace ONLY if no paths are configured
  if (dirs.length === 0) {
    dirs.push('/workspace');
  }

  return dirs;
}

async function ensureVistaPathsDiscovered() {
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;

  const hasConfiguredDirs = !!(String(cfg?.routinesPath || '').trim() || String(cfg?.rpcRoutinesPath || '').trim());

  if (hasConfiguredDirs) {
    return;
  }

  if (useDocker) {
    if (!cfg?.containerId) return;
  } else {
    if (!cfg?.host || !cfg?.username) return;
  }

  try {
    const discovered = await discoverVistaProfilePaths({ timeoutMs: 4000 });
    if (discovered?.ok) {
      applyVistaProfilePathsToConfig(cfg, discovered, { override: true });
    }
  } catch (err) {
    console.error('[ensureVistaPathsDiscovered] Discovery error:', err?.message || err);
  }
}

async function fetchRoutineDirectoriesToLocal(targetRoot) {
  await ensureVistaPathsDiscovered();
  const routineDirs = getRoutineDirs();

  if (!routineDirs.length) {
    return Promise.resolve({ ok: false, error: 'No routines path configured' });
  }

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
        if (err) {
          console.error('[fetchRoutineDirectoriesToLocal] Docker fetch failed:', err.message);
          return resolve({ ok: false, error: err.message, stdout, stderr });
        }
        resolve({ ok: true });
      });
    } else {
      const sshPass = cfg.password ? `sshpass -p '${(cfg.password || '').replace(/'/g, `'\\''`)}' ` : '';
      const sshCmd = `${sshPass}ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${tarCmdInner.replace(/"/g, '\\"')}" | tar -xf - -C '${target}'`;

      exec(sshCmd, { timeout: 60000, maxBuffer: 20 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) {
          console.error('[fetchRoutineDirectoriesToLocal] SSH fetch failed:', err.message);
          return resolve({ ok: false, error: err.message, stdout, stderr });
        }
        resolve({ ok: true });
      });
    }
  });
}

module.exports = {
  getRoutineDirs,
  fetchRoutineDirectoriesToLocal
};
