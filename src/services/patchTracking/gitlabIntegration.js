/**
 * GitLab Integration
 * Handles Git operations with user approval workflow (Option C)
 */

const { exec } = require('child_process');
const util = require('util');
const execAsync = util.promisify(exec);

class GitLabIntegration {
    constructor() {
        this.repoPath = null;
        this.repoUrl = 'https://gitlab.ehs.com.jo/Ahmad.AlKhalaileh/docker';
        this.approvalRequired = true;
    }

    /**
     * Initialize GitLab integration
     * @param {string} repoPath - Local repository path
     */
    async init(repoPath) {
        this.repoPath = repoPath;
        console.log('[GitLab] Initialized with repo:', repoPath);

        // Verify git is available
        try {
            await execAsync('git --version');
        } catch (error) {
            throw new Error('Git is not installed or not available');
        }
    }

    /**
     * Prepare commit with patch metadata (REQUIRES USER APPROVAL)
     * @param {Object} options - Commit options
     * @returns {Promise<Object>} Prepared commit data
     */
    async prepareCommit(options) {
        const {
            patchMetadata,
            correlation,
            changeResult
        } = options;

        console.log('[GitLab] Preparing commit for patch:', patchMetadata.patchId);

        // Build commit message
        const commitMessage = this.buildCommitMessage(patchMetadata, correlation);

        // Build file list
        const files = correlation.matched.map(change => ({
            path: `localr/${change.routine}.m`,
            routine: change.routine,
            status: 'modified'
        }));

        // Build git commands (for user review)
        const gitCommands = this.buildGitCommands(files, commitMessage, patchMetadata);

        const preparedCommit = {
            patchId: patchMetadata.patchId,
            message: commitMessage,
            files,
            gitCommands,
            approvalStatus: 'pending',
            preparedAt: new Date().toISOString(),
            stats: {
                filesModified: files.length,
                totalChanges: correlation.matched.length
            }
        };

        console.log('[GitLab] Commit prepared - awaiting user approval');

        return preparedCommit;
    }

    /**
     * Build commit message from patch metadata
     * @param {Object} patchMetadata - Patch metadata
     * @param {Object} correlation - Correlation result
     * @returns {string} Formatted commit message
     */
    buildCommitMessage(patchMetadata, correlation) {
        const { patchId, title, author, createdDate, gitlabIssues, description } = patchMetadata;

        let message = `Applied patch ${patchId}: ${title}\n\n`;

        // Add description
        if (description && description.length > 0) {
            const desc = description
                .filter(line => line.trim())
                .join('\n')
                .substring(0, 500); // Limit description length
            message += `${desc}\n\n`;
        }

        // Add changed routines (modified + added)
        const modifiedCount = correlation.matched.filter(c => c.status === 'modified').length;
        const addedCount = correlation.matched.filter(c => c.status === 'added').length;

        if (modifiedCount > 0 && addedCount > 0) {
            message += `Modified Routines (${modifiedCount}):\n`;
            correlation.matched.filter(c => c.status === 'modified').forEach(change => {
                message += `- ${change.routine}.m\n`;
            });
            message += `\nAdded Routines (${addedCount}):\n`;
            correlation.matched.filter(c => c.status === 'added').forEach(change => {
                message += `- ${change.routine}.m\n`;
            });
        } else if (addedCount > 0) {
            message += `Added Routines (${addedCount}):\n`;
            correlation.matched.forEach(change => {
                message += `- ${change.routine}.m\n`;
            });
        } else {
            message += `Modified Routines (${modifiedCount}):\n`;
            correlation.matched.forEach(change => {
                message += `- ${change.routine}.m\n`;
            });
        }
        message += '\n';

        // Add metadata
        message += `Patch Details:\n`;
        message += `- Patch Author: ${author || 'Unknown'}\n`;
        message += `- Patch Date: ${createdDate || 'Unknown'}\n`;

        // Add GitLab issues
        if (gitlabIssues && gitlabIssues.length > 0) {
            message += `- Issues: ${gitlabIssues.map(i => `#${i.number}`).join(', ')}\n`;
            gitlabIssues.forEach(issue => {
                message += `  ${issue.url}\n`;
            });
        }

        message += '\n';
        message += '🤖 Auto-tracked by Ahmad IDE\n';

        // Add co-author if available
        if (author) {
            const email = `${author.toLowerCase().replace(/\s+/g, '.')}@ehs.com.jo`;
            message += `Co-Authored-By: ${author} <${email}>`;
        }

        return message;
    }

    /**
     * Build git commands for user review
     * @param {Array} files - List of files to commit
     * @param {string} message - Commit message
     * @param {Object} patchMetadata - Patch metadata
     * @returns {Object} Git commands
     */
    buildGitCommands(files, message, patchMetadata) {
        const fileList = files.map(f => f.path).join(' ');
        const tag = patchMetadata.patchId.replace(/\*/g, '-');

        return {
            add: `git add ${fileList}`,
            commit: `git commit -m "${message.replace(/"/g, '\\"')}"`,
            tag: `git tag ${tag}`,
            push: `git push origin HEAD --tags`, // Use HEAD to push current branch
            full: [
                `git add ${fileList}`,
                `git commit -m "${message.replace(/"/g, '\\"')}"`,
                `git tag ${tag}`,
                `git push origin HEAD --tags`
            ].join(' && ')
        };
    }

    /**
     * Download changed routines from Docker to Git repo
     * @param {Object} preparedCommit - Prepared commit data
     * @param {string} connectionId - Docker container ID
     * @param {Object} changeResult - Change detection result with file paths
     */
    async downloadChangedRoutines(preparedCommit, connectionId, changeResult) {
        if (!this.repoPath) {
            throw new Error('Repository path not initialized');
        }

        console.log('[GitLab] ========================================');
        console.log('[GitLab] Starting routine download from Docker');
        console.log('[GitLab] ========================================');

        const fs = require('fs');
        const path = require('path');
        const { exec } = require('child_process');
        const util = require('util');
        const execAsync = util.promisify(exec);
        const { connectionConfig } = require('../../bridge/config/connectionConfig');
        const { hasActiveSshSession } = require('../../bridge/state/sessions');
        const { getRoutineDirs } = require('../../bridge/routines/fetchRoutineDirectoriesToLocal');

        // Get current paths from connectionConfig (updated by scan)
        const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
        const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;

        console.log('[GitLab] Connection config:', {
            type: connectionConfig.type,
            routinesPath: cfg?.routinesPath,
            rpcRoutinesPath: cfg?.rpcRoutinesPath,
            basePath: cfg?.basePath
        });

        // Get all routine directories (localr, routines, etc.)
        const routineDirs = getRoutineDirs();
        console.log('[GitLab] Available routine directories:', routineDirs);

        // Find localr path (primary directory for changes)
        const localrPath = routineDirs.find(d => /\/localr\/?$/.test(d)) ||
                          cfg.routinesPath ||
                          routineDirs[0] ||
                          null;

        console.log('[GitLab] Using localr path:', localrPath);

        if (!localrPath) {
            throw new Error('Cannot determine localr path - no paths configured. Run scan first to discover paths from vista-profile.');
        }

        // Create localr and routines directories in repo
        const localrDir = path.join(this.repoPath, 'localr');
        const routinesDir = path.join(this.repoPath, 'routines');

        if (!fs.existsSync(localrDir)) {
            fs.mkdirSync(localrDir, { recursive: true });
            console.log('[GitLab] Created localr directory:', localrDir);
        }
        if (!fs.existsSync(routinesDir)) {
            fs.mkdirSync(routinesDir, { recursive: true });
            console.log('[GitLab] Created routines directory:', routinesDir);
        }

        // Download each modified/added file from Docker
        const filesToDownload = [
            ...(changeResult.changes.modified || []),
            ...(changeResult.changes.addedToLocalr || [])
        ];

        console.log(`[GitLab] Files to download: ${filesToDownload.length}`);
        console.log('[GitLab] Files:', filesToDownload.map(f => f.routine).join(', '));

        let successCount = 0;
        let failCount = 0;

        for (const change of filesToDownload) {
            try {
                const localPath = path.join(localrDir, `${change.routine}.m`);

                // Determine Docker path with multiple fallback strategies
                let dockerPath = change.localrPath; // From scan result

                if (!dockerPath || !dockerPath.startsWith('/')) {
                    // Fallback 1: Use localrPath from config
                    dockerPath = `${localrPath}/${change.routine}.m`;
                    console.log(`[GitLab] Using config path for ${change.routine}: ${dockerPath}`);
                }

                // Verify path looks valid
                if (!dockerPath || !dockerPath.startsWith('/')) {
                    console.error(`[GitLab] Invalid Docker path for ${change.routine}: ${dockerPath}`);
                    failCount++;
                    continue;
                }

                // Use docker cp to copy file from container
                const cpCmd = `docker cp ${connectionId}:${dockerPath} "${localPath}"`;
                console.log(`[GitLab] Executing: ${cpCmd}`);

                const result = await execAsync(cpCmd);

                if (fs.existsSync(localPath)) {
                    const stats = fs.statSync(localPath);
                    console.log(`[GitLab] ✓ Downloaded ${change.routine} (${stats.size} bytes)`);
                    successCount++;
                } else {
                    console.error(`[GitLab] ✗ File not created: ${change.routine}`);
                    failCount++;
                }

            } catch (error) {
                console.error(`[GitLab] ✗ Failed to download ${change.routine}:`, error.message);
                console.error(`[GitLab]   Error details:`, error.stderr || error.stdout);
                failCount++;
            }
        }

        console.log('[GitLab] ========================================');
        console.log(`[GitLab] Download complete: ${successCount} success, ${failCount} failed`);
        console.log('[GitLab] ========================================');

        if (failCount > 0 && successCount === 0) {
            throw new Error(`Failed to download any routines. Check that:\n1. Paths are correct (localr: ${localrPath})\n2. Files exist in Docker container\n3. Docker container is running`);
        }
    }

    /**
     * Execute commit (AFTER USER APPROVAL)
     * @param {Object} preparedCommit - Prepared commit data
     * @param {string} connectionId - Docker container ID (optional, for downloading files)
     * @param {Object} changeResult - Change detection result (optional, for downloading files)
     * @returns {Promise<Object>} Commit result
     */
    async executeCommit(preparedCommit, connectionId = null, changeResult = null) {
        if (!this.repoPath) {
            throw new Error('Repository path not initialized');
        }

        console.log('[GitLab] Executing commit for:', preparedCommit.patchId);

        // Check if there are any files to commit
        if (!preparedCommit.files || preparedCommit.files.length === 0) {
            throw new Error(`No files to commit for patch ${preparedCommit.patchId}. The scan found 0 changed routines. Make sure:\n1. The Docker container has routines in localr/\n2. The routines from the patch exist in the container\n3. The scan detected changes correctly`);
        }

        try {
            const cwd = this.repoPath;

            // Download changed files from Docker if connection info provided
            if (connectionId && changeResult) {
                await this.downloadChangedRoutines(preparedCommit, connectionId, changeResult);
            }

            // Add files
            const addCmd = preparedCommit.gitCommands.add;
            console.log('[GitLab] Running:', addCmd);
            await execAsync(addCmd, { cwd });

            // Commit
            const commitCmd = preparedCommit.gitCommands.commit;
            console.log('[GitLab] Running: git commit');
            let commitResult;
            let hasNewCommit = true;

            try {
                commitResult = await execAsync(commitCmd, { cwd });
            } catch (error) {
                // Check if error is "nothing to commit" (files already committed)
                if (error.stdout && error.stdout.includes('nothing to commit')) {
                    console.log('[GitLab] Files already committed - no changes detected');
                    commitResult = { stdout: 'No changes - files already up to date' };
                    hasNewCommit = false;
                } else {
                    throw error; // Re-throw if it's a different error
                }
            }

            let pushResult = { stdout: '' };

            // Only tag and push if there was a new commit
            if (hasNewCommit) {
                // Tag
                const tagCmd = preparedCommit.gitCommands.tag;
                console.log('[GitLab] Running:', tagCmd);
                try {
                    await execAsync(tagCmd, { cwd });
                } catch (error) {
                    // Tag might already exist, that's OK
                    if (error.stderr && error.stderr.includes('already exists')) {
                        console.log('[GitLab] Tag already exists');
                    } else {
                        throw error;
                    }
                }

                // Push
                const pushCmd = preparedCommit.gitCommands.push;
                console.log('[GitLab] Running:', pushCmd);
                pushResult = await execAsync(pushCmd, { cwd });
            } else {
                console.log('[GitLab] Skipping tag and push - no new changes');
            }

            const result = {
                success: true,
                patchId: preparedCommit.patchId,
                committedAt: new Date().toISOString(),
                commitOutput: commitResult.stdout,
                pushOutput: pushResult.stdout
            };

            console.log('[GitLab] Commit executed successfully');

            return result;

        } catch (error) {
            console.error('[GitLab] Commit execution failed:', error);
            throw new Error(`Git commit failed: ${error.message}`);
        }
    }

    /**
     * Get repository status
     * @returns {Promise<Object>} Repository status
     */
    async getRepoStatus() {
        if (!this.repoPath) {
            return { available: false, error: 'Repository path not initialized' };
        }

        try {
            const { stdout: status } = await execAsync('git status --short', { cwd: this.repoPath });
            const { stdout: branch } = await execAsync('git branch --show-current', { cwd: this.repoPath });
            const { stdout: remote } = await execAsync('git remote get-url origin', { cwd: this.repoPath });

            return {
                available: true,
                branch: branch.trim(),
                remote: remote.trim(),
                hasChanges: status.trim().length > 0,
                status: status.trim()
            };

        } catch (error) {
            return {
                available: false,
                error: error.message
            };
        }
    }

    /**
     * Generate commit preview for user approval
     * @param {Object} preparedCommit - Prepared commit data
     * @returns {string} Human-readable commit preview
     */
    generateCommitPreview(preparedCommit) {
        let preview = '═══════════════════════════════════════════════════\n';
        preview += '  PATCH COMMIT PREVIEW - REVIEW BEFORE APPROVAL\n';
        preview += '═══════════════════════════════════════════════════\n\n';

        preview += `Patch: ${preparedCommit.patchId}\n`;
        preview += `Files: ${preparedCommit.stats.filesModified} modified\n`;
        preview += `Prepared: ${new Date(preparedCommit.preparedAt).toLocaleString()}\n\n`;

        preview += '───────────────────────────────────────────────────\n';
        preview += 'COMMIT MESSAGE:\n';
        preview += '───────────────────────────────────────────────────\n';
        preview += preparedCommit.message + '\n\n';

        preview += '───────────────────────────────────────────────────\n';
        preview += 'FILES TO BE COMMITTED:\n';
        preview += '───────────────────────────────────────────────────\n';
        preparedCommit.files.forEach(file => {
            preview += `  ${file.status.toUpperCase().padEnd(10)} ${file.path}\n`;
        });
        preview += '\n';

        preview += '───────────────────────────────────────────────────\n';
        preview += 'GIT COMMANDS (for review):\n';
        preview += '───────────────────────────────────────────────────\n';
        preview += preparedCommit.gitCommands.full + '\n\n';

        preview += '═══════════════════════════════════════════════════\n';
        preview += '  ACTION REQUIRED: Approve to execute commit\n';
        preview += '═══════════════════════════════════════════════════\n';

        return preview;
    }

    /**
     * Clone or validate repository
     * @param {string} targetPath - Target path for repository
     * @returns {Promise<boolean>} Success status
     */
    async setupRepository(targetPath) {
        try {
            // Check if directory exists and is a git repo
            try {
                await execAsync('git rev-parse --git-dir', { cwd: targetPath });
                console.log('[GitLab] Repository already exists at:', targetPath);
                return true;
            } catch {
                // Not a git repo, need to clone
                console.log('[GitLab] Cloning repository to:', targetPath);
                await execAsync(`git clone ${this.repoUrl} ${targetPath}`);
                return true;
            }

        } catch (error) {
            console.error('[GitLab] Repository setup failed:', error);
            return false;
        }
    }
}

// Export singleton instance
module.exports = new GitLabIntegration();
