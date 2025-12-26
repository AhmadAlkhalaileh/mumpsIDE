const {
  DEFAULT_ENV_KEY,
  DOCKER_DEFAULT_ENV_KEY,
  deriveEnvKeyFromUsername,
  buildSshPaths,
  buildDockerPaths
} = require('./paths');

let connectionConfig = null;
function setConnectionConfig(cfg) {
  connectionConfig = cfg;
}

function mergeSshConfig(cfg = {}) {
  const fallbackEnvKey = cfg.envKey || cfg.baseKey || cfg.namespace || connectionConfig.ssh?.envKey || DEFAULT_ENV_KEY;
  const envKey = deriveEnvKeyFromUsername(cfg.username, fallbackEnvKey);
  const ydbPath = cfg.ydbPath || connectionConfig.ssh?.ydbPath || '/opt/fis-gtm/YDB136';
  const paths = buildSshPaths(envKey, ydbPath);
  return {
    ...connectionConfig.ssh,
    ...paths,
    ...cfg,
    envKey
  };
}

function mergeDockerConfig(cfg = {}) {
  // Use envKey from config if provided, otherwise use default
  const envKey = cfg.envKey || connectionConfig.docker?.envKey || DOCKER_DEFAULT_ENV_KEY;
  // ydbPath is optional - if not provided, Docker works in universal mode
  const ydbPath = cfg.ydbPath !== undefined ? cfg.ydbPath : connectionConfig.docker?.ydbPath;
  const paths = buildDockerPaths(envKey, ydbPath);
  return {
    ...connectionConfig.docker,
    ...paths,
    ...cfg,
    envKey
  };
}

module.exports = {
  setConnectionConfig,
  mergeSshConfig,
  mergeDockerConfig
};
