/**
 * SSH IPC Handlers for Electron
 * Connects renderer process to SSH service in main process
 */

const { ipcMain } = require('electron');
const sshService = require('../services/sshService');
const keytar = require('keytar'); // OS keychain for secure password storage

/**
 * Register SSH IPC handlers
 */
function registerSSHHandlers() {
    console.log('[IPC] Registering SSH handlers');

    // SSH: Connect
    ipcMain.handle('ssh:connect', async (event, options) => {
        try {
            const connectionId = await sshService.connect(options);
            return connectionId;
        } catch (error) {
            console.error('[IPC] SSH connect error:', error);
            throw error;
        }
    });

    // SSH: Execute command
    ipcMain.handle('ssh:exec', async (event, options) => {
        try {
            const result = await sshService.execCommand(options);
            return result;
        } catch (error) {
            console.error('[IPC] SSH exec error:', error);
            throw error;
        }
    });

    // SSH: Disconnect
    ipcMain.handle('ssh:disconnect', async (event, options) => {
        try {
            await sshService.disconnect(options);
            return { success: true };
        } catch (error) {
            console.error('[IPC] SSH disconnect error:', error);
            throw error;
        }
    });

    // SSH: Get status
    ipcMain.handle('ssh:status', async (event, options) => {
        try {
            const { connectionId } = options;
            const isConnected = sshService.isConnected(connectionId);
            return { connected: isConnected };
        } catch (error) {
            console.error('[IPC] SSH status error:', error);
            throw error;
        }
    });

    // SSH: Get active connections
    ipcMain.handle('ssh:list', async () => {
        try {
            const connections = sshService.getActiveConnections();
            return connections;
        } catch (error) {
            console.error('[IPC] SSH list error:', error);
            throw error;
        }
    });

    console.log('[IPC] SSH handlers registered');
}

/**
 * Register Keychain IPC handlers
 */
function registerKeychainHandlers() {
    console.log('[IPC] Registering Keychain handlers');

    // Keychain: Set password
    ipcMain.handle('keychain:setPassword', async (event, options) => {
        try {
            const { service, account, password } = options;

            if (!service || !account || !password) {
                throw new Error('Service, account, and password are required');
            }

            await keytar.setPassword(service, account, password);
            console.log(`[Keychain] Password set for ${service}:${account}`);
            return { success: true };

        } catch (error) {
            console.error('[IPC] Keychain set password error:', error);
            throw error;
        }
    });

    // Keychain: Get password
    ipcMain.handle('keychain:getPassword', async (event, options) => {
        try {
            const { service, account } = options;

            if (!service || !account) {
                throw new Error('Service and account are required');
            }

            const password = await keytar.getPassword(service, account);
            return password; // null if not found

        } catch (error) {
            console.error('[IPC] Keychain get password error:', error);
            throw error;
        }
    });

    // Keychain: Delete password
    ipcMain.handle('keychain:deletePassword', async (event, options) => {
        try {
            const { service, account } = options;

            if (!service || !account) {
                throw new Error('Service and account are required');
            }

            const success = await keytar.deletePassword(service, account);
            console.log(`[Keychain] Password deleted for ${service}:${account}`);
            return { success };

        } catch (error) {
            console.error('[IPC] Keychain delete password error:', error);
            throw error;
        }
    });

    // Keychain: Find passwords
    ipcMain.handle('keychain:findPasswords', async (event, options) => {
        try {
            const { service } = options;

            if (!service) {
                throw new Error('Service is required');
            }

            const credentials = await keytar.findCredentials(service);
            // Return without passwords for security
            return credentials.map(c => ({ account: c.account }));

        } catch (error) {
            console.error('[IPC] Keychain find passwords error:', error);
            throw error;
        }
    });

    console.log('[IPC] Keychain handlers registered');
}

/**
 * Cleanup handlers
 */
function cleanup() {
    // Remove all handlers
    ipcMain.removeHandler('ssh:connect');
    ipcMain.removeHandler('ssh:exec');
    ipcMain.removeHandler('ssh:disconnect');
    ipcMain.removeHandler('ssh:status');
    ipcMain.removeHandler('ssh:list');
    ipcMain.removeHandler('keychain:setPassword');
    ipcMain.removeHandler('keychain:getPassword');
    ipcMain.removeHandler('keychain:deletePassword');
    ipcMain.removeHandler('keychain:findPasswords');

    console.log('[IPC] Handlers cleaned up');
}

module.exports = {
    registerSSHHandlers,
    registerKeychainHandlers,
    cleanup
};
