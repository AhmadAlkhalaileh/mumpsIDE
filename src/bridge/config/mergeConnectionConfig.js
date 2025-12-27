const {
  deriveEnvKeyFromUsername
} = require('./paths');

let connectionConfig = null;
function setConnectionConfig(cfg) {
  connectionConfig = cfg;
}

function mergeSshConfig(cfg = {}) {
  const hasEnvKey = Object.prototype.hasOwnProperty.call(cfg, 'envKey');
  const explicitEnvKey = hasEnvKey ? (String(cfg.envKey || '').trim() || null) : null;

  const fallbackEnvKey = String(cfg.baseKey || cfg.namespace || connectionConfig.ssh?.envKey || '').trim();
  const derivedEnvKey = deriveEnvKeyFromUsername(cfg.username || connectionConfig.ssh?.username || '', fallbackEnvKey);
  const envKey = explicitEnvKey || (derivedEnvKey ? derivedEnvKey : null);

  const ydbPath = String(cfg.ydbPath || connectionConfig.ssh?.ydbPath || '/opt/fis-gtm/YDB136').trim();
  return {
    ...connectionConfig.ssh,
    ...cfg,
    envKey,
    ydbPath: ydbPath || null
  };
}

function mergeDockerConfig(cfg = {}) {
  const hasEnvKey = Object.prototype.hasOwnProperty.call(cfg, 'envKey');
  const envKey = hasEnvKey
    ? (String(cfg.envKey || '').trim() || null)
    : (String(connectionConfig.docker?.envKey || '').trim() || null);

  const hasYdbPath = Object.prototype.hasOwnProperty.call(cfg, 'ydbPath');
  const rawYdbPath = hasYdbPath ? cfg.ydbPath : connectionConfig.docker?.ydbPath;
  const ydbPath = rawYdbPath ? String(rawYdbPath).trim() : null;
  return {
    ...connectionConfig.docker,
    ...cfg,
    envKey,
    ydbPath
  };
}

module.exports = {
  setConnectionConfig,
  mergeSshConfig,
  mergeDockerConfig
};
