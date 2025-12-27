const { connectionConfig } = require('../config/connectionConfig');
const { hasActiveSshSession } = require('../state/sessions');
const { getRoutineDirs } = require('../routines/fetchRoutineDirectoriesToLocal');
const { runHostCommand } = require('../util/runHostCommand');
const { discoverVistaProfilePaths, applyVistaProfilePathsToConfig } = require('../vista/vistaProfilePaths');
const { buildSourceMapFromCode } = require('./sourceMapUtils');

const sourceMapCache = {};

async function ensureVistaPathsDiscovered() {
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;
  const hasConfiguredDirs = !!(String(cfg?.routinesPath || '').trim() || String(cfg?.rpcRoutinesPath || '').trim());
  if (hasConfiguredDirs) return;

  if (useDocker) {
    if (!cfg?.containerId) return;
  } else {
    if (!cfg?.host || !cfg?.username) return;
  }

  try {
    const discovered = await discoverVistaProfilePaths({ timeoutMs: 4000 });
    applyVistaProfilePathsToConfig(cfg, discovered, { override: true });
  } catch (_) { }
}

async function loadRoutineSourceMap(routine, inlineSourceMap = null) {
  if (!routine) return null;
  const key = routine.toUpperCase();
  if (sourceMapCache[key]) return sourceMapCache[key];
  if (inlineSourceMap) {
    sourceMapCache[key] = inlineSourceMap;
    return inlineSourceMap;
  }
  await ensureVistaPathsDiscovered();
  const routineDirs = getRoutineDirs();
  for (const dir of routineDirs) {
    const cmd = `cat ${dir}/${key}.m 2>/dev/null`;
    const res = await runHostCommand(cmd);
    if (res.ok && res.stdout) {
      const smap = buildSourceMapFromCode(key, res.stdout);
      sourceMapCache[key] = smap;
      return smap;
    }
  }
  return null;
}

module.exports = {
  sourceMapCache,
  loadRoutineSourceMap
};
