/**
 * SSH Service for Electron Main Process
 * Handles SSH connections for patch tracking
 * Uses ssh2 for SSH operations (same as bridge.js)
 */

const { Client: SSH2Client } = require('ssh2');
const crypto = require('crypto');

class SSHService {
    constructor() {
        this.connections = new Map(); // connectionId -> SSH instance
        this.config = new Map(); // connectionId -> config
    }

    /**
     * Generate unique connection ID
     */
    generateConnectionId() {
        return crypto.randomBytes(16).toString('hex');
    }

    /**
     * Connect to SSH server
     * @param {Object} options - {host, port, username, password}
     * @returns {Promise<string>} connectionId
     */
    async connect(options) {
        const { host, port = 22, username, password, privateKey, passphrase } = options;

        if (!host || !username) {
            throw new Error('Host and username are required');
        }

        if (!password && !privateKey) {
            throw new Error('Password or private key is required');
        }

        const ssh = new SSH2Client();
        const connectionId = this.generateConnectionId();

        return new Promise((resolve, reject) => {
            const connectOptions = {
                host,
                port,
                username
            };

            if (password) {
                connectOptions.password = password;
            }

            if (privateKey) {
                connectOptions.privateKey = privateKey;
                if (passphrase) {
                    connectOptions.passphrase = passphrase;
                }
            }

            ssh.on('ready', () => {
                this.connections.set(connectionId, ssh);
                this.config.set(connectionId, {
                    host,
                    port,
                    username,
                    connectedAt: new Date().toISOString()
                });

                console.log(`[SSH Service] Connected: ${connectionId} to ${host}:${port}`);
                resolve(connectionId);
            });

            ssh.on('error', (err) => {
                console.error('[SSH Service] Connection error:', err.message);
                reject(new Error(`SSH connection failed: ${err.message}`));
            });

            ssh.connect(connectOptions);
        });
    }

    /**
     * Execute command on SSH connection
     * @param {Object} options - {connectionId, command, timeout}
     * @returns {Promise<Object>} {stdout, stderr, code}
     */
    async execCommand(options) {
        const { connectionId, command, timeout = 30000 } = options;

        const ssh = this.connections.get(connectionId);
        if (!ssh) {
            throw new Error(`Connection not found: ${connectionId}`);
        }

        return new Promise((resolve, reject) => {
            let stdout = '';
            let stderr = '';
            let timedOut = false;

            const timeoutId = setTimeout(() => {
                timedOut = true;
                reject(new Error('Command timeout'));
            }, timeout);

            ssh.exec(command, (err, stream) => {
                if (err) {
                    clearTimeout(timeoutId);
                    return reject(new Error(`Command execution failed: ${err.message}`));
                }

                stream.on('close', (code, signal) => {
                    clearTimeout(timeoutId);
                    if (!timedOut) {
                        resolve({
                            stdout,
                            stderr,
                            code: code !== undefined ? code : (stderr ? 1 : 0)
                        });
                    }
                });

                stream.on('data', (data) => {
                    stdout += data.toString();
                });

                stream.stderr.on('data', (data) => {
                    stderr += data.toString();
                });
            });
        });
    }

    /**
     * Disconnect SSH connection
     * @param {Object} options - {connectionId}
     */
    async disconnect(options) {
        const { connectionId } = options;

        const ssh = this.connections.get(connectionId);
        if (!ssh) {
            console.warn(`[SSH Service] Connection not found: ${connectionId}`);
            return;
        }

        try {
            ssh.end();
            this.connections.delete(connectionId);
            this.config.delete(connectionId);
            console.log(`[SSH Service] Disconnected: ${connectionId}`);
        } catch (error) {
            console.error('[SSH Service] Disconnect failed:', error.message);
        }
    }

    /**
     * Get connection status
     * @param {string} connectionId
     */
    isConnected(connectionId) {
        const ssh = this.connections.get(connectionId);
        return ssh !== undefined;
    }

    /**
     * Get active connections count
     */
    getActiveConnectionsCount() {
        return this.connections.size;
    }

    /**
     * Get all active connection IDs
     */
    getActiveConnections() {
        return Array.from(this.config.entries()).map(([id, config]) => ({
            id,
            ...config
        }));
    }

    /**
     * Cleanup all connections
     */
    async cleanup() {
        const ids = Array.from(this.connections.keys());
        for (const id of ids) {
            await this.disconnect({ connectionId: id });
        }
    }
}

// Export singleton instance
const sshService = new SSHService();

// Cleanup on process exit
process.on('exit', () => {
    sshService.cleanup();
});

module.exports = sshService;
