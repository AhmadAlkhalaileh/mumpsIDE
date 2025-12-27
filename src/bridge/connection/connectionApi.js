const { exec } = require('child_process');

const { shellQuote } = require('../config/paths');

const sshClient = require('../ssh/sshClient');
const { ensureSshClient } = sshClient;

const { mergeSshConfig, mergeDockerConfig } = require('../config/mergeConnectionConfig');

const { connectionConfig } = require('../config/connectionConfig');

const { sshSessions, hasActiveSshSession } = require('../state/sessions');

const { detectYottaDBPath } = require('../ssh/detectYottaDBPath');

const { wrapDockerCmd } = require('../util/process');

const {
  discoverVistaProfilePaths,
  applyVistaProfilePathsToConfig
} = require('../vista/vistaProfilePaths');

module.exports = {
  async setConnection(type, cfg = {}) {
    try {
      let discovered = null;
      if (type === 'docker') {
        connectionConfig.type = 'docker';
        connectionConfig.docker = mergeDockerConfig(cfg.docker || {});

        // Auto-discover paths from /var/worldvista/prod/common/vista-profile when possible.
        try {
          if (connectionConfig.docker?.containerId) {
            discovered = await discoverVistaProfilePaths({ timeoutMs: 4000 });
            applyVistaProfilePathsToConfig(connectionConfig.docker, discovered, { override: true });

            // Preserve any explicit user overrides.
            const user = cfg.docker || {};
            if (user.gldPath) connectionConfig.docker.gldPath = user.gldPath;
            if (user.basePath) connectionConfig.docker.basePath = user.basePath;
            if (user.routinesPath) connectionConfig.docker.routinesPath = user.routinesPath;
            if (user.rpcRoutinesPath) connectionConfig.docker.rpcRoutinesPath = user.rpcRoutinesPath;
            if (user.envKey && !(discovered?.ok && discovered.envKey)) connectionConfig.docker.envKey = user.envKey;
          }
        } catch (err) {
          console.error('[setConnection] Path discovery failed:', err);
        }
      } else if (type === 'ssh') {
        connectionConfig.type = 'ssh';
        connectionConfig.ssh = mergeSshConfig(cfg.ssh || {});

        try {
          if (connectionConfig.ssh?.host && connectionConfig.ssh?.username) {
            discovered = await discoverVistaProfilePaths({ timeoutMs: 4000 });
            applyVistaProfilePathsToConfig(connectionConfig.ssh, discovered, { override: true });

            const user = cfg.ssh || {};
            if (user.gldPath) connectionConfig.ssh.gldPath = user.gldPath;
            if (user.basePath) connectionConfig.ssh.basePath = user.basePath;
            if (user.routinesPath) connectionConfig.ssh.routinesPath = user.routinesPath;
            if (user.rpcRoutinesPath) connectionConfig.ssh.rpcRoutinesPath = user.rpcRoutinesPath;
            if (user.envKey && !(discovered?.ok && discovered.envKey)) connectionConfig.ssh.envKey = user.envKey;
          }
        } catch (err) {
          console.error('[setConnection] Path discovery failed:', err);
        }
      }

      return { ok: true, type: connectionConfig.type, config: connectionConfig, discovered };
    } catch (error) {
      console.error('[setConnection] Error:', error);
      return { ok: false, error: error.message };
    }
  },

  getConnection() {
    const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
    const type = useDocker ? 'docker' : 'ssh';
    const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;

    const normalizeDirToken = (token) => {
      const raw = String(token || '').trim();
      if (!raw) return '';
      const withoutParen = raw.includes('(') ? raw.slice(0, raw.indexOf('(')) : raw;
      const cleaned = withoutParen.trim();
      if (!cleaned) return '';
      if (!cleaned.startsWith('/')) return '';
      if (/\.(so|o|obj)$/i.test(cleaned)) return '';
      if (cleaned.includes('$')) return '';
      return cleaned;
    };

    const collectDirs = (raw) => String(raw || '')
      .split(/\s+/)
      .map(normalizeDirToken)
      .filter(Boolean);

    const candidates = [
      ...collectDirs(cfg.routinesPath),
      ...collectDirs(cfg.rpcRoutinesPath)
    ];

    const localrPath = candidates.find((d) => /\/localr\/?$/.test(d)) || (candidates[0] || null);
    const routinesPath = candidates.find((d) => /\/routines\/?$/.test(d)) || null;
    const basePath = cfg.basePath
      || (localrPath ? localrPath.replace(/\/localr\/?$/, '') : null)
      || (routinesPath ? routinesPath.replace(/\/routines\/?$/, '') : null)
      || null;

    const envKey = (() => {
      const explicit = String(cfg.envKey || '').trim();
      if (explicit) return explicit;
      const m = String(basePath || '').match(/^\/var\/worldvista\/prod\/([^/]+)$/);
      return m ? (m[1] || '') : '';
    })();

    return {
      ok: true,
      type,
      connectionId: type === 'docker' ? cfg.containerId : null,
      config: cfg,
      paths: {
        localrPath,
        routinesPath,
        gldPath: cfg.gldPath || null,
        basePath,
        envKey: envKey || null
      }
    };
  },

  async sshConnect(config) {
    const sshCtor = ensureSshClient();
    if (!sshCtor) {
      return { ok: false, error: `SSH not available (missing native module ssh2${sshClient.sshLoadError ? `: ${sshClient.sshLoadError.message}` : ''})` };
    }
    const cfg = config || {};
    const purpose = String(cfg.purpose || '').trim().toLowerCase();
    const skipYdbDetect = !!cfg.skipYdbDetect || purpose === 'compare-with-release' || purpose === 'release-compare' || purpose === 'release';
    const setAsActiveConnection = cfg.setAsActiveConnection !== false;
    return new Promise((resolve) => {
      const conn = new sshCtor();
      const id = `ssh_${Date.now()}_${Math.random().toString(16).slice(2)}`;

      conn.on('ready', async () => {
        sshSessions[id] = conn;

        if (setAsActiveConnection) {
          // Store connection details for execute/ debug over SSH
          connectionConfig.type = 'ssh';
          connectionConfig.ssh = mergeSshConfig(cfg);

          // Auto-detect YottaDB path unless explicitly skipped (used by Compare-with-Release)
          if (!skipYdbDetect && (!cfg.ydbPath || cfg.ydbPath === '/opt/fis-gtm/YDB136')) {
            const detectedPath = await detectYottaDBPath(conn);
            if (detectedPath) {
              connectionConfig.ssh.ydbPath = detectedPath;
              console.log(`[SSH] Auto-detected YottaDB at: ${detectedPath}`);
            } else {
              console.warn('[SSH] Warning: Could not auto-detect YottaDB path. Please configure it in Connection settings.');
            }
          }
        }

        resolve({ ok: true, sessionId: id, ydbPath: setAsActiveConnection ? connectionConfig.ssh.ydbPath : (cfg.ydbPath || null) });
      }).on('error', (err) => {
        resolve({ ok: false, error: err.message });
      }).connect({
        host: cfg.host,
        port: cfg.port || 22,
        username: cfg.username,
        password: cfg.password
      });
    });
  },

  async sshExec(sessionId, command) {
    const conn = sshSessions[sessionId];
    if (!conn) return { ok: false, error: 'SSH session not found' };

    return new Promise((resolve) => {
      conn.exec(command, (err, stream) => {
        if (err) return resolve({ ok: false, error: err.message });
        let stdout = '';
        let stderr = '';
        stream.on('close', () => {
          resolve({ ok: true, stdout, stderr });
        }).on('data', (data) => {
          stdout += data.toString();
        }).stderr.on('data', (data) => {
          stderr += data.toString();
        });
      });
    });
  },

  async sshDisconnect(sessionId) {
    const conn = sshSessions[sessionId];
    if (conn) {
      conn.end();
      delete sshSessions[sessionId];
    }
    return { ok: true };
  },

  /**
   * Create a directory in the current environment (Docker or SSH).
   * Works in both universal mode (no YottaDB) and configured mode.
   * @param {string} dirPath - The directory path to create (relative or absolute)
   * @returns {Promise<{ok: boolean, error?: string}>}
   */
  async createDirectoryInCurrentEnv(dirPath) {
    if (!dirPath || typeof dirPath !== 'string') {
      return { ok: false, error: 'Directory path is required' };
    }

    const cfg = connectionConfig.type === 'ssh' ? connectionConfig.ssh : connectionConfig.docker;
    const isDocker = connectionConfig.type === 'docker';
    const mkdirCmd = `mkdir -p ${shellQuote(dirPath)}`;

    return new Promise((resolve) => {
      try {
        if (isDocker) {
          if (!cfg.containerId) {
            return resolve({ ok: false, error: 'No Docker container selected' });
          }
          const dockerCmd = wrapDockerCmd(`docker exec ${cfg.containerId} bash -c ${shellQuote(mkdirCmd)}`);
          exec(dockerCmd, { timeout: 8000 }, (err, stdout, stderr) => {
            if (err) {
              resolve({ ok: false, error: err.message || stderr || 'Failed to create directory' });
            } else {
              resolve({ ok: true });
            }
          });
        } else {
          // SSH mode
          if (!cfg.host || !cfg.username) {
            return resolve({ ok: false, error: 'SSH not connected' });
          }
          const sshPass = cfg.password ? `sshpass -p '${cfg.password}' ` : '';
          const sshCmd = `${sshPass}ssh -o StrictHostKeyChecking=no -p ${cfg.port || 22} ${cfg.username}@${cfg.host} ${shellQuote(mkdirCmd)}`;
          exec(sshCmd, { timeout: 8000 }, (err, stdout, stderr) => {
            if (err) {
              resolve({ ok: false, error: err.message || stderr || 'Failed to create directory' });
            } else {
              resolve({ ok: true });
            }
          });
        }
      } catch (err) {
        resolve({ ok: false, error: err.message || 'Failed to create directory' });
      }
    });
  }
};
