/**
 * Patch Registry
 * Manages patch metadata and change tracking database
 */

const fs = require('fs').promises;
const path = require('path');
const crypto = require('crypto');

class PatchRegistry {
    constructor() {
        this.registryDir = path.join(__dirname, '../../../.patch-tracking');
        this.patchesFile = path.join(this.registryDir, 'patches.json');
        this.changesFile = path.join(this.registryDir, 'changes.json');
        this.environmentsFile = path.join(this.registryDir, 'environments.json');
    }

    /**
     * Initialize registry files
     */
    async init() {
        try {
            await fs.mkdir(this.registryDir, { recursive: true });

            // Initialize patches.json if not exists
            try {
                await fs.access(this.patchesFile);
            } catch {
                await fs.writeFile(this.patchesFile, JSON.stringify({ patches: [] }, null, 2));
            }

            // Initialize changes.json if not exists
            try {
                await fs.access(this.changesFile);
            } catch {
                await fs.writeFile(this.changesFile, JSON.stringify({ changes: [] }, null, 2));
            }

            // Initialize environments.json if not exists
            try {
                await fs.access(this.environmentsFile);
            } catch {
                await fs.writeFile(this.environmentsFile, JSON.stringify({ environments: [] }, null, 2));
            }

            console.log('[Patch Registry] Initialized');
        } catch (error) {
            console.error('[Patch Registry] Init error:', error);
            throw error;
        }
    }

    /**
     * Add patch to registry
     * @param {Object} patchMetadata - Parsed patch metadata
     * @returns {Promise<string>} Patch ID
     */
    async addPatch(patchMetadata) {
        try {
            const patches = await this.getAllPatches();

            // Check if patch already exists
            const existingIndex = patches.findIndex(p => p.patchId === patchMetadata.patchId);

            const patchRecord = {
                ...patchMetadata,
                registeredAt: new Date().toISOString(),
                status: 'pending', // pending, committed, error
                id: crypto.randomBytes(8).toString('hex')
            };

            if (existingIndex >= 0) {
                // Update existing patch
                patches[existingIndex] = { ...patches[existingIndex], ...patchRecord };
                console.log('[Patch Registry] Updated patch:', patchMetadata.patchId);
            } else {
                // Add new patch
                patches.push(patchRecord);
                console.log('[Patch Registry] Added patch:', patchMetadata.patchId);
            }

            await fs.writeFile(this.patchesFile, JSON.stringify({ patches }, null, 2));
            return patchRecord.id;

        } catch (error) {
            console.error('[Patch Registry] Add patch error:', error);
            throw error;
        }
    }

    /**
     * Get all patches
     * @returns {Promise<Array>} List of patches
     */
    async getAllPatches() {
        try {
            const data = await fs.readFile(this.patchesFile, 'utf8');
            const json = JSON.parse(data);
            return json.patches || [];
        } catch (error) {
            console.error('[Patch Registry] Get patches error:', error);
            return [];
        }
    }

    /**
     * Get patch by ID
     * @param {string} patchId - Patch identifier (e.g., "UJO*3.0*28")
     * @returns {Promise<Object|null>} Patch record
     */
    async getPatch(patchId) {
        const patches = await this.getAllPatches();
        return patches.find(p => p.patchId === patchId) || null;
    }

    /**
     * Record detected changes
     * @param {string} environment - Environment name
     * @param {Array} changes - List of changes
     * @returns {Promise<string>} Change record ID
     */
    async recordChanges(environment, changes) {
        try {
            const allChanges = await this.getAllChanges();

            const changeRecord = {
                id: crypto.randomBytes(8).toString('hex'),
                environment,
                detectedAt: new Date().toISOString(),
                changes,
                status: 'pending' // pending, committed, ignored
            };

            allChanges.push(changeRecord);

            await fs.writeFile(this.changesFile, JSON.stringify({ changes: allChanges }, null, 2));
            console.log('[Patch Registry] Recorded changes for:', environment);

            return changeRecord.id;

        } catch (error) {
            console.error('[Patch Registry] Record changes error:', error);
            throw error;
        }
    }

    /**
     * Get all change records
     * @returns {Promise<Array>} List of change records
     */
    async getAllChanges() {
        try {
            const data = await fs.readFile(this.changesFile, 'utf8');
            const json = JSON.parse(data);
            return json.changes || [];
        } catch (error) {
            console.error('[Patch Registry] Get changes error:', error);
            return [];
        }
    }

    /**
     * Register environment
     * @param {Object} env - Environment config
     */
    async registerEnvironment(env) {
        try {
            const data = await fs.readFile(this.environmentsFile, 'utf8');
            const json = JSON.parse(data);
            const environments = json.environments || [];

            // Check if environment exists
            const existingIndex = environments.findIndex(e => e.name === env.name);

            const envRecord = {
                ...env,
                lastScanned: new Date().toISOString()
            };

            if (existingIndex >= 0) {
                environments[existingIndex] = envRecord;
            } else {
                environments.push(envRecord);
            }

            await fs.writeFile(this.environmentsFile, JSON.stringify({ environments }, null, 2));
            console.log('[Patch Registry] Registered environment:', env.name);

        } catch (error) {
            console.error('[Patch Registry] Register environment error:', error);
            throw error;
        }
    }

    /**
     * Get all environments
     * @returns {Promise<Array>} List of environments
     */
    async getAllEnvironments() {
        try {
            const data = await fs.readFile(this.environmentsFile, 'utf8');
            const json = JSON.parse(data);
            return json.environments || [];
        } catch (error) {
            console.error('[Patch Registry] Get environments error:', error);
            return [];
        }
    }

    /**
     * Update patch status
     * @param {string} patchId - Patch ID
     * @param {string} status - New status
     * @param {Object} metadata - Additional metadata
     */
    async updatePatchStatus(patchId, status, metadata = {}) {
        try {
            const patches = await this.getAllPatches();
            const patchIndex = patches.findIndex(p => p.patchId === patchId);

            if (patchIndex >= 0) {
                patches[patchIndex].status = status;
                patches[patchIndex].updatedAt = new Date().toISOString();
                patches[patchIndex] = { ...patches[patchIndex], ...metadata };

                await fs.writeFile(this.patchesFile, JSON.stringify({ patches }, null, 2));
                console.log('[Patch Registry] Updated patch status:', patchId, status);
            }

        } catch (error) {
            console.error('[Patch Registry] Update status error:', error);
            throw error;
        }
    }

    /**
     * Link patch to changes
     * @param {string} patchId - Patch ID
     * @param {string} changeId - Change record ID
     */
    async linkPatchToChanges(patchId, changeId) {
        try {
            const patches = await this.getAllPatches();
            const patchIndex = patches.findIndex(p => p.patchId === patchId);

            if (patchIndex >= 0) {
                if (!patches[patchIndex].linkedChanges) {
                    patches[patchIndex].linkedChanges = [];
                }
                if (!patches[patchIndex].linkedChanges.includes(changeId)) {
                    patches[patchIndex].linkedChanges.push(changeId);
                }

                await fs.writeFile(this.patchesFile, JSON.stringify({ patches }, null, 2));
                console.log('[Patch Registry] Linked patch to changes:', patchId, changeId);
            }

        } catch (error) {
            console.error('[Patch Registry] Link error:', error);
            throw error;
        }
    }

    /**
     * Get statistics
     * @returns {Promise<Object>} Registry statistics
     */
    async getStatistics() {
        const patches = await this.getAllPatches();
        const changes = await this.getAllChanges();
        const environments = await this.getAllEnvironments();

        return {
            totalPatches: patches.length,
            pendingPatches: patches.filter(p => p.status === 'pending').length,
            committedPatches: patches.filter(p => p.status === 'committed').length,
            totalChanges: changes.length,
            pendingChanges: changes.filter(c => c.status === 'pending').length,
            environments: environments.length
        };
    }
}

// Export singleton instance
module.exports = new PatchRegistry();
