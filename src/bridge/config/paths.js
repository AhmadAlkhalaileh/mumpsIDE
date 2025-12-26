const DEFAULT_ENV_KEY = 'cc';
const DOCKER_DEFAULT_ENV_KEY = 'hakeem';

function shellQuote(str = '') {
  return `'${str.replace(/'/g, `'\\''`)}'`;
}

// envKey is the username suffix after the last dash (user-cc => cc); fallback keeps cc when parsing fails.
function deriveEnvKeyFromUsername(username, fallback = DEFAULT_ENV_KEY) {
  const raw = (username || '').trim();
  if (!raw) return fallback;
  const lastDash = raw.lastIndexOf('-');
  if (lastDash === -1) return fallback;
  const key = raw.slice(lastDash + 1).trim();
  return key || fallback;
}

function buildEnvPaths(envKey = DEFAULT_ENV_KEY) {
  const key = (envKey || DEFAULT_ENV_KEY).trim() || DEFAULT_ENV_KEY;
  const basePath = `/var/worldvista/prod/${key}`;
  return {
    envKey: key,
    gldPath: `${basePath}/globals/mumps.gld`,
    routinesPath: `${basePath}/localr`,
    rpcRoutinesPath: `${basePath}/localr ${basePath}/routines`,
    basePath
  };
}

// Default connection config (from web backend)
function buildSshPaths(envKey = DEFAULT_ENV_KEY, ydbPath = '/opt/fis-gtm/YDB136') {
  return {
    ydbPath,
    ...buildEnvPaths(envKey)
  };
}

function buildDockerPaths(envKey = DOCKER_DEFAULT_ENV_KEY, ydbPath = null) {
  // In universal mode (no ydbPath), don't set any YottaDB-specific paths
  if (!ydbPath) {
    return {
      ydbPath: null,
      gldPath: null,
      routinesPath: null,
      rpcRoutinesPath: null,
      basePath: null,
      envKey: envKey || DOCKER_DEFAULT_ENV_KEY
    };
  }
  // In configured mode, build all the paths
  const basePaths = buildEnvPaths(envKey);
  return { ydbPath, ...basePaths };
}

function normalizeRoutineName(name = '') {
  const trimmed = (name || '').trim();
  if (!trimmed) return '';
  return trimmed.replace(/\.m$/i, '').toUpperCase();
}

module.exports = {
  DEFAULT_ENV_KEY,
  DOCKER_DEFAULT_ENV_KEY,
  shellQuote,
  deriveEnvKeyFromUsername,
  buildEnvPaths,
  buildSshPaths,
  buildDockerPaths,
  normalizeRoutineName
};
