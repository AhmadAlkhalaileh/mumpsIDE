const { exec } = require('child_process');

const {
  DEFAULT_ENV_KEY,
  DOCKER_DEFAULT_ENV_KEY,
  shellQuote
} = require('../config/paths');

const sshClient = require('../ssh/sshClient');
const { ensureSshClient } = sshClient;

const { mergeSshConfig, mergeDockerConfig } = require('../config/mergeConnectionConfig');

const { connectionConfig } = require('../config/connectionConfig');

const { sshSessions, hasActiveSshSession } = require('../state/sessions');

const { detectYottaDBPath } = require('../ssh/detectYottaDBPath');

const { wrapDockerCmd } = require('../util/process');

module.exports = {
  setConnection(type, cfg = {}) {
    if (type === 'docker') {
      connectionConfig.type = 'docker';
      connectionConfig.docker = mergeDockerConfig(cfg.docker || {});
    } else if (type === 'ssh') {
      connectionConfig.type = 'ssh';
      connectionConfig.ssh = mergeSshConfig(cfg.ssh || {});
    }
    return { ok: true, type: connectionConfig.type, config: connectionConfig };
  },

  getConnection() {
    const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
    const type = useDocker ? 'docker' : 'ssh';
    const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;

    // Handle null basePath (universal mode) - use default paths
    const basePath = cfg.basePath || `/var/worldvista/prod/${cfg.envKey || DOCKER_DEFAULT_ENV_KEY}`;

    return {
      ok: true,
      type,
      connectionId: type === 'docker' ? cfg.containerId : null,
      config: cfg,
      paths: {
        localrPath: `${basePath}/localr`,
        routinesPath: `${basePath}/routines`,
        basePath: basePath,
        envKey: cfg.envKey || (useDocker ? DOCKER_DEFAULT_ENV_KEY : DEFAULT_ENV_KEY)
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

