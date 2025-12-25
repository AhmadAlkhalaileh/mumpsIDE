/**
 * Patch Tracker Service
 * Main orchestrator for Vista patch tracking system
 */

const kidsParser = require('./kidsParser');
const patchRegistry = require('./patchRegistry');
const dockerScanner = require('./dockerScanner');
const changeDetector = require('./changeDetector');
const gitlabIntegration = require('./gitlabIntegration');
const sshService = require('../sshService');

class PatchTrackerService {
    constructor() {
        this.initialized = false;
        this.activeConnection = null;
        this.currentWorkflow = null;
    }

    /**
     * Initialize patch tracking service
     */
    async init(repoPath = null) {
        if (this.initialized) {
            console.log('[Patch Tracker] Already initialized');
            return;
        }

        console.log('[Patch Tracker] Initializing...');

        try {
            // Initialize registry
            await patchRegistry.init();

            // Initialize scanner and detector
            dockerScanner.init(sshService);
            changeDetector.init(dockerScanner);

            // Initialize GitLab integration with repo path
            if (repoPath) {
                await gitlabIntegration.init(repoPath);
                console.log('[Patch Tracker] GitLab integration initialized with:', repoPath);
            } else {
                // Try to use default paths
                const defaultPaths = [
                    require('os').homedir() + '/Desktop/vista-routines',
                    require('os').homedir() + '/vista-routines',
                    process.cwd() + '/vista-routines'
                ];

                const fs = require('fs');
                for (const path of defaultPaths) {
                    if (fs.existsSync(path + '/.git')) {
                        await gitlabIntegration.init(path);
                        console.log('[Patch Tracker] GitLab integration auto-detected repo:', path);
                        break;
                    }
                }
            }

            this.initialized = true;
            console.log('[Patch Tracker] Initialization complete');

        } catch (error) {
            console.error('[Patch Tracker] Initialization failed:', error);
            throw error;
        }
    }

    /**
     * Set Git repository path
     * @param {string} repoPath - Local Git repository path
     */
    async setRepoPath(repoPath) {
        console.log('[Patch Tracker] Setting repository path:', repoPath);

        const fs = require('fs');
        const path = require('path');
        const os = require('os');

        // Expand ~ to home directory
        let expandedPath = repoPath;
        if (repoPath.startsWith('~/')) {
            expandedPath = path.join(os.homedir(), repoPath.slice(2));
        } else if (repoPath === '~') {
            expandedPath = os.homedir();
        }

        console.log('[Patch Tracker] Expanded path:', expandedPath);

        if (!fs.existsSync(expandedPath)) {
            throw new Error(`Repository path does not exist: ${expandedPath}`);
        }

        if (!fs.existsSync(path.join(expandedPath, '.git'))) {
            throw new Error(`Not a Git repository: ${expandedPath} (no .git directory found)`);
        }

        await gitlabIntegration.init(expandedPath);
        console.log('[Patch Tracker] GitLab integration initialized');

        return { success: true, repoPath: expandedPath };
    }

    /**
     * Upload and parse KIDS patch file
     * @param {string} filePath - Path to KIDS file
     * @returns {Promise<Object>} Parsed patch metadata
     */
    async uploadPatch(filePath) {
        console.log('[Patch Tracker] Uploading patch:', filePath);

        try {
            const fs = require('fs').promises;
            const content = await fs.readFile(filePath, 'utf8');

            // Validate KIDS format
            if (!kidsParser.isValidKIDS(content)) {
                throw new Error('Invalid KIDS file format');
            }

            // Parse patch
            const patchMetadata = kidsParser.parse(content);

            // Add to registry
            const patchId = await patchRegistry.addPatch(patchMetadata);

            console.log('[Patch Tracker] Patch uploaded:', patchMetadata.patchId);

            return {
                success: true,
                patchId: patchMetadata.patchId,
                metadata: patchMetadata
            };

        } catch (error) {
            console.error('[Patch Tracker] Upload failed:', error);
            throw error;
        }
    }

    /**
     * Scan Docker environment for changes
     * @param {Object} options - Scan options
     * @returns {Promise<Object>} Scan and change detection results
     */
    async scanEnvironment(options = {}) {
        if (!this.initialized) {
            await this.init();
        }

        const {
            connectionId,
            envName = 'docker',
            localrPath = '/var/worldvista/prod/hakeem/localr',
            routinesPath = '/var/worldvista/prod/hakeem/routines',
            patchId = null // NEW: Optional patch ID to scan only its routines
        } = options;

        if (!connectionId) {
            throw new Error('SSH connection ID required');
        }

        console.log('[Patch Tracker] Scanning environment:', envName);

        // Store connection ID for later use in commit
        this.activeConnection = connectionId;

        try {
            // Get routine names from patch if provided
            let routineNames = null;
            console.log('[Patch Tracker] patchId:', patchId);
            if (patchId) {
                const patchMetadata = await patchRegistry.getPatch(patchId);
                console.log('[Patch Tracker] patchMetadata:', patchMetadata);
                if (patchMetadata && patchMetadata.routines && patchMetadata.routines.length > 0) {
                    routineNames = patchMetadata.routines;
                    console.log(`[Patch Tracker] Scanning ${routineNames.length} routines from patch ${patchId}:`, routineNames.join(', '));
                } else {
                    console.warn(`[Patch Tracker] Patch ${patchId} has no routines - scanning all`);
                    if (patchMetadata) {
                        console.warn('[Patch Tracker] Patch metadata routines:', patchMetadata.routines);
                    }
                }
            } else {
                console.warn('[Patch Tracker] No patchId provided - scanning all routines');
            }

            // Scan environment (will use fast path if routineNames provided)
            const scanResult = await dockerScanner.scanEnvironment(connectionId, {
                envName,
                localrPath,
                routinesPath,
                routineNames // Pass routine names for targeted scan
            });

            // Detect changes
            const changeResult = await changeDetector.detectChanges(connectionId, scanResult);

            // Register environment
            await patchRegistry.registerEnvironment({
                name: envName,
                paths: { localr: localrPath, routines: routinesPath },
                connectionId
            });

            // Record changes
            const changeId = await patchRegistry.recordChanges(envName, changeResult);

            console.log('[Patch Tracker] Scan complete:', {
                modified: changeResult.summary.modified,
                added: changeResult.summary.added,
                removed: changeResult.summary.removed
            });

            return {
                success: true,
                scanResult,
                changeResult,
                changeId
            };

        } catch (error) {
            console.error('[Patch Tracker] Scan failed:', error);
            throw error;
        }
    }

    /**
     * Correlate patch with detected changes
     * @param {string} patchId - Patch identifier
     * @param {string} changeId - Change record ID
     * @returns {Promise<Object>} Correlation result
     */
    async correlatePatchWithChanges(patchId, changeId) {
        console.log('[Patch Tracker] Correlating patch with changes:', patchId);

        try {
            // Get patch metadata
            const patchMetadata = await patchRegistry.getPatch(patchId);
            if (!patchMetadata) {
                throw new Error(`Patch not found: ${patchId}`);
            }

            // Get change record
            const allChanges = await patchRegistry.getAllChanges();
            const changeRecord = allChanges.find(c => c.id === changeId);
            if (!changeRecord) {
                throw new Error(`Change record not found: ${changeId}`);
            }

            // Correlate
            const correlation = changeDetector.correlateWithPatch(
                changeRecord.changes,
                patchMetadata
            );

            // Validate
            const validation = changeDetector.validateChanges(correlation);

            // Link patch to changes
            await patchRegistry.linkPatchToChanges(patchId, changeId);

            console.log('[Patch Tracker] Correlation complete:', {
                matchRate: `${correlation.matchRate.toFixed(1)}%`,
                canCommit: validation.canCommit
            });

            return {
                success: true,
                correlation,
                validation,
                patchMetadata,
                changeRecord
            };

        } catch (error) {
            console.error('[Patch Tracker] Correlation failed:', error);
            throw error;
        }
    }

    /**
     * Prepare commit for user approval (Option C workflow)
     * @param {string} patchId - Patch identifier
     * @param {Object} correlation - Correlation result
     * @returns {Promise<Object>} Prepared commit
     */
    async prepareCommit(patchId, correlation) {
        console.log('[Patch Tracker] Preparing commit for approval:', patchId);

        try {
            const patchMetadata = await patchRegistry.getPatch(patchId);
            if (!patchMetadata) {
                throw new Error(`Patch not found: ${patchId}`);
            }

            // Prepare commit
            const preparedCommit = await gitlabIntegration.prepareCommit({
                patchMetadata,
                correlation: correlation.correlation,
                changeResult: correlation.changeRecord.changes
            });

            // Generate preview
            const preview = gitlabIntegration.generateCommitPreview(preparedCommit);

            // Store workflow state
            this.currentWorkflow = {
                patchId,
                preparedCommit,
                correlation,
                preparedAt: new Date().toISOString()
            };

            console.log('[Patch Tracker] Commit prepared - awaiting approval');

            return {
                success: true,
                preparedCommit,
                preview,
                requiresApproval: true
            };

        } catch (error) {
            console.error('[Patch Tracker] Prepare commit failed:', error);
            throw error;
        }
    }

    /**
     * Execute commit (AFTER user approval)
     * @returns {Promise<Object>} Commit result
     */
    async executeCommit() {
        if (!this.currentWorkflow) {
            throw new Error('No commit workflow in progress');
        }

        console.log('[Patch Tracker] Executing approved commit:', this.currentWorkflow.patchId);

        try {
            const { preparedCommit, patchId, correlation } = this.currentWorkflow;

            // Get Docker connection info from correlation
            const changeRecord = correlation.changeRecord;
            const connectionId = this.activeConnection; // Store this during scan
            const changeResult = changeRecord?.changes || changeRecord;

            console.log('[Patch Tracker] Using connection:', connectionId);
            console.log('[Patch Tracker] Change result:', changeResult ? 'available' : 'none');

            // Execute commit (will download files from Docker first)
            const result = await gitlabIntegration.executeCommit(
                preparedCommit,
                connectionId,
                changeResult
            );

            // Update patch status
            await patchRegistry.updatePatchStatus(patchId, 'committed', {
                committedAt: result.committedAt,
                commitResult: result
            });

            // Clear workflow
            this.currentWorkflow = null;

            console.log('[Patch Tracker] Commit executed successfully');

            return {
                success: true,
                result
            };

        } catch (error) {
            console.error('[Patch Tracker] Execute commit failed:', error);

            // Update patch status to error
            if (this.currentWorkflow) {
                await patchRegistry.updatePatchStatus(
                    this.currentWorkflow.patchId,
                    'error',
                    { error: error.message }
                );
            }

            throw error;
        }
    }

    /**
     * Cancel current commit workflow
     */
    async cancelCommit() {
        if (!this.currentWorkflow) {
            console.log('[Patch Tracker] No workflow to cancel');
            return { success: true };
        }

        console.log('[Patch Tracker] Canceling commit workflow:', this.currentWorkflow.patchId);

        this.currentWorkflow = null;

        return { success: true, message: 'Commit workflow canceled' };
    }

    /**
     * Get tracking statistics
     * @returns {Promise<Object>} Statistics
     */
    async getStatistics() {
        const stats = await patchRegistry.getStatistics();
        const repoStatus = await gitlabIntegration.getRepoStatus();

        return {
            ...stats,
            gitRepo: repoStatus,
            hasActiveWorkflow: !!this.currentWorkflow
        };
    }

    /**
     * Get all patches with their status
     * @returns {Promise<Array>} Patches list
     */
    async getAllPatches() {
        return await patchRegistry.getAllPatches();
    }

    /**
     * Get all changes
     * @returns {Promise<Array>} Changes list
     */
    async getAllChanges() {
        return await patchRegistry.getAllChanges();
    }

    /**
     * Get current workflow status
     * @returns {Object|null} Current workflow or null
     */
    getCurrentWorkflow() {
        return this.currentWorkflow;
    }
}

// Export singleton instance
module.exports = new PatchTrackerService();
