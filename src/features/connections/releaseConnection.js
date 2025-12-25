/**
 * Release Connection Settings
 * Manages SSH connection to release server for Compare with Release feature
 */

(() => {
    function createReleaseConnectionSettings({ deps } = {}) {
        const settingsService = deps?.settingsService || window.AhmadIDEModules?.services?.settingsService;
        const primitives = deps?.primitives || window.AhmadIDEModules?.ui?.primitives;

        if (!settingsService || !primitives) {
            throw new Error('ReleaseConnection requires settingsService and primitives');
        }

        /**
         * Load connection settings
         */
        const loadConnection = () => {
            try {
                const connections = JSON.parse(localStorage.getItem('ahmadIDE:connectionProfiles') || '{}');
                return connections.release || null;
            } catch (_) {
                return null;
            }
        };

        /**
         * Save connection settings
         */
        const saveConnection = (config) => {
            try {
                const connections = JSON.parse(localStorage.getItem('ahmadIDE:connectionProfiles') || '{}');
                connections.release = {
                    id: 'release',
                    name: config.name || 'Release Server',
                    type: 'ssh',
                    host: config.host,
                    port: config.port || 22,
                    username: config.username,
                    createdAt: config.createdAt || new Date().toISOString(),
                    updatedAt: new Date().toISOString()
                };
                localStorage.setItem('ahmadIDE:connectionProfiles', JSON.stringify(connections));

                // Store password securely via bridge
                if (config.password) {
                    storePasswordSecurely(config.password);
                }

                return true;
            } catch (error) {
                console.error('[Release Connection] Failed to save:', error);
                return false;
            }
        };

        /**
         * Delete connection settings
         */
        const deleteConnection = () => {
            try {
                const connections = JSON.parse(localStorage.getItem('ahmadIDE:connectionProfiles') || '{}');
                delete connections.release;
                localStorage.setItem('ahmadIDE:connectionProfiles', JSON.stringify(connections));

                // Delete password from keychain
                deletePasswordSecurely();
                return true;
            } catch (error) {
                console.error('[Release Connection] Failed to delete:', error);
                return false;
            }
        };

        /**
         * Test connection
         */
        const testConnection = async (config) => {
            try {
                const response = await window.bridge?.invoke('ssh:connect', {
                    host: config.host,
                    port: config.port || 22,
                    username: config.username,
                    password: config.password || await getPasswordSecurely(),
                    purpose: 'compare-with-release',
                    skipYdbDetect: true,
                    setAsActiveConnection: false
                });

                if (!response || !response.ok || !response.sessionId) {
                    throw new Error(response?.error || 'Failed to establish connection');
                }

                const sessionId = response.sessionId;

                // Test with simple command
                const result = await window.bridge?.invoke('ssh:exec', {
                    sessionId,
                    command: 'echo "test"'
                });

                // Disconnect
                await window.bridge?.invoke('ssh:disconnect', { sessionId });

                if (result?.ok) {
                    return { success: true, message: 'Connection successful' };
                } else {
                    return { success: false, message: result?.error || result?.stderr || 'Connection test failed' };
                }
            } catch (error) {
                return { success: false, message: error.message || 'Connection failed' };
            }
        };

        /**
         * Store password securely in OS keychain
         */
        const storePasswordSecurely = async (password) => {
            try {
                await window.bridge?.invoke('keychain:setPassword', {
                    service: 'AhmadIDE',
                    account: 'release-connection',
                    password
                });
            } catch (error) {
                console.error('[Release Connection] Failed to store password:', error);
            }
        };

        /**
         * Get password securely from OS keychain
         */
        const getPasswordSecurely = async () => {
            try {
                return await window.bridge?.invoke('keychain:getPassword', {
                    service: 'AhmadIDE',
                    account: 'release-connection'
                });
            } catch (error) {
                console.error('[Release Connection] Failed to get password:', error);
                return null;
            }
        };

        /**
         * Delete password from OS keychain
         */
        const deletePasswordSecurely = async () => {
            try {
                await window.bridge?.invoke('keychain:deletePassword', {
                    service: 'AhmadIDE',
                    account: 'release-connection'
                });
            } catch (error) {
                console.error('[Release Connection] Failed to delete password:', error);
            }
        };

        /**
         * Render connection form
         */
        const renderConnectionForm = (container, existingConfig = null) => {
            const { createInput, createButton } = primitives;

            const config = existingConfig || loadConnection() || {
                host: '',
                port: 22,
                username: '',
                password: ''
            };

            const form = document.createElement('div');
            form.className = 'release-connection-form';
            form.innerHTML = `
                <div class="ps-form-section">
                    <div class="ps-form-header">
                        <h3>Release Connection (SSH)</h3>
                        <p class="ps-text-secondary">Configure SSH connection to release server for routine comparison</p>
                    </div>

                    <div class="ps-form-group">
                        <label for="releaseHost" class="ps-form-label">Host / IP Address</label>
                        <input type="text" id="releaseHost" class="ps-input" placeholder="192.168.1.100" value="${escapeHtml(config.host || '')}" required>
                    </div>

                    <div class="ps-form-group">
                        <label for="releasePort" class="ps-form-label">Port</label>
                        <input type="number" id="releasePort" class="ps-input" placeholder="22" value="${config.port || 22}" min="1" max="65535" required>
                    </div>

                    <div class="ps-form-group">
                        <label for="releaseUsername" class="ps-form-label">Username</label>
                        <input type="text" id="releaseUsername" class="ps-input" placeholder="username" value="${escapeHtml(config.username || '')}" required>
                    </div>

                    <div class="ps-form-group">
                        <label for="releasePassword" class="ps-form-label">Password</label>
                        <input type="password" id="releasePassword" class="ps-input" placeholder="••••••••" autocomplete="new-password">
                        <small class="ps-form-help">Password is stored securely in your system keychain</small>
                    </div>

                    <div class="ps-form-actions">
                        <button type="button" id="releaseTestBtn" class="ps-btn ps-btn-secondary">
                            <svg width="16" height="16" fill="currentColor" style="margin-right: 4px;">
                                <path d="M8 1a7 7 0 1 0 0 14A7 7 0 0 0 8 1z"/>
                            </svg>
                            Test Connection
                        </button>
                        <button type="button" id="releaseSaveBtn" class="ps-btn ps-btn-primary">
                            Save
                        </button>
                        ${existingConfig ? `
                            <button type="button" id="releaseDeleteBtn" class="ps-btn ps-btn-danger">
                                Delete
                            </button>
                        ` : ''}
                    </div>

                    <div id="releaseConnectionStatus" class="ps-connection-status hidden"></div>
                </div>
            `;

            container.appendChild(form);

            // Wire up events
            const testBtn = form.querySelector('#releaseTestBtn');
            const saveBtn = form.querySelector('#releaseSaveBtn');
            const deleteBtn = form.querySelector('#releaseDeleteBtn');
            const statusDiv = form.querySelector('#releaseConnectionStatus');

            testBtn?.addEventListener('click', async () => {
                const formConfig = getFormValues(form);
                if (!validateForm(formConfig, statusDiv)) {
                    return;
                }

                testBtn.disabled = true;
                testBtn.textContent = 'Testing...';
                statusDiv.className = 'ps-connection-status ps-status-info';
                statusDiv.textContent = 'Connecting...';
                statusDiv.classList.remove('hidden');

                const result = await testConnection(formConfig);

                if (result.success) {
                    statusDiv.className = 'ps-connection-status ps-status-success';
                    statusDiv.textContent = '✓ ' + result.message;
                } else {
                    statusDiv.className = 'ps-connection-status ps-status-error';
                    statusDiv.textContent = '✗ ' + result.message;
                }

                testBtn.disabled = false;
                testBtn.innerHTML = `
                    <svg width="16" height="16" fill="currentColor" style="margin-right: 4px;">
                        <path d="M8 1a7 7 0 1 0 0 14A7 7 0 0 0 8 1z"/>
                    </svg>
                    Test Connection
                `;
            });

            saveBtn?.addEventListener('click', () => {
                const formConfig = getFormValues(form);
                if (!validateForm(formConfig, statusDiv)) {
                    return;
                }

                const success = saveConnection(formConfig);

                if (success) {
                    statusDiv.className = 'ps-connection-status ps-status-success';
                    statusDiv.textContent = '✓ Connection settings saved';
                    statusDiv.classList.remove('hidden');

                    // Trigger refresh of connections list
                    window.dispatchEvent(new CustomEvent('ahmadIDE:connections-updated'));
                } else {
                    statusDiv.className = 'ps-connection-status ps-status-error';
                    statusDiv.textContent = '✗ Failed to save connection settings';
                    statusDiv.classList.remove('hidden');
                }
            });

            deleteBtn?.addEventListener('click', () => {
                if (confirm('Are you sure you want to delete this connection?')) {
                    const success = deleteConnection();

                    if (success) {
                        statusDiv.className = 'ps-connection-status ps-status-success';
                        statusDiv.textContent = '✓ Connection deleted';
                        statusDiv.classList.remove('hidden');

                        // Trigger refresh
                        window.dispatchEvent(new CustomEvent('ahmadIDE:connections-updated'));

                        // Clear form
                        setTimeout(() => {
                            container.innerHTML = '';
                            renderConnectionForm(container, null);
                        }, 1500);
                    } else {
                        statusDiv.className = 'ps-connection-status ps-status-error';
                        statusDiv.textContent = '✗ Failed to delete connection';
                        statusDiv.classList.remove('hidden');
                    }
                }
            });
        };

        /**
         * Get form values
         */
        const getFormValues = (form) => {
            return {
                host: form.querySelector('#releaseHost')?.value?.trim(),
                port: parseInt(form.querySelector('#releasePort')?.value) || 22,
                username: form.querySelector('#releaseUsername')?.value?.trim(),
                password: form.querySelector('#releasePassword')?.value
            };
        };

        /**
         * Validate form
         */
        const validateForm = (config, statusDiv) => {
            if (!config.host) {
                statusDiv.className = 'ps-connection-status ps-status-error';
                statusDiv.textContent = '✗ Please enter a host address';
                statusDiv.classList.remove('hidden');
                return false;
            }

            if (!config.username) {
                statusDiv.className = 'ps-connection-status ps-status-error';
                statusDiv.textContent = '✗ Please enter a username';
                statusDiv.classList.remove('hidden');
                return false;
            }

            if (config.port < 1 || config.port > 65535) {
                statusDiv.className = 'ps-connection-status ps-status-error';
                statusDiv.textContent = '✗ Port must be between 1 and 65535';
                statusDiv.classList.remove('hidden');
                return false;
            }

            return true;
        };

        /**
         * Escape HTML
         */
        const escapeHtml = (str) => {
            const div = document.createElement('div');
            div.textContent = str;
            return div.innerHTML;
        };

        return {
            loadConnection,
            saveConnection,
            deleteConnection,
            testConnection,
            renderConnectionForm,
            renderConfigForm: renderConnectionForm, // Alias for compatibility
            getPasswordSecurely,
            storePasswordSecurely,
            deletePasswordSecurely
        };
    }

    // Export
    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.releaseConnection = createReleaseConnectionSettings();
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { createReleaseConnectionSettings };
    }
})();
