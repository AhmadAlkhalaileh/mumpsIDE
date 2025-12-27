/**
 * Docker Environment Scanner
 * Scans Vista Docker containers for routine changes
 */

const crypto = require('crypto');
const { exec } = require('child_process');
const util = require('util');
const execAsync = util.promisify(exec);
const { extractVistaPathsFromProfileText, applyVistaProfilePathsToConfig } = require('../../bridge/vista/vistaProfilePaths');
const { connectionConfig } = require('../../bridge/config/connectionConfig');
const { hasActiveSshSession } = require('../../bridge/state/sessions');

class DockerScanner {
    constructor() {
        this.sshService = null;
    }

    /**
     * Initialize scanner with SSH service
     * @param {Object} sshService - SSH service instance
     */
    init(sshService) {
        this.sshService = sshService;
    }

    /**
     * Check if connection ID is a Docker container ID (hexadecimal)
     */
    isDockerContainer(connectionId) {
        // Docker IDs are typically 12+ hex characters
        return /^[0-9a-f]{12,}$/i.test(connectionId);
    }

    /**
     * Execute command on Docker container or SSH connection
     */
    async execCommand(connectionId, command, timeout = 30000) {
        if (this.isDockerContainer(connectionId)) {
            // Execute on Docker container
            try {
                const dockerCommand = `docker exec ${connectionId} ${command}`;
                console.log('[Docker Scanner] Executing:', dockerCommand);

                const { stdout, stderr } = await execAsync(dockerCommand, {
                    timeout,
                    maxBuffer: 10 * 1024 * 1024 // 10MB buffer
                });

                return {
                    stdout: stdout || '',
                    stderr: stderr || '',
                    code: 0
                };
            } catch (error) {
                return {
                    stdout: error.stdout || '',
                    stderr: error.stderr || error.message,
                    code: error.code || 1
                };
            }
        } else {
            // Execute on SSH connection
            if (!this.sshService) {
                throw new Error('SSH service not initialized');
            }
            return await this.sshService.execCommand({ connectionId, command, timeout });
        }
    }

    /**
     * Scan Docker environment
     * @param {string} connectionId - Docker container ID or SSH connection ID
     * @param {Object} config - Scan configuration
     * @returns {Promise<Object>} Scan results
     */
    async scanEnvironment(connectionId, config = {}) {

        const {
            localrPath = null,
            routinesPath = null,
            envName = 'docker',
            routineNames = null // NEW: Optional list of specific routines to scan
        } = config;

        console.log('[Docker Scanner] Starting scan:', envName);
        console.log('[Docker Scanner] Paths:', { localrPath, routinesPath });

        try {
            // Auto-discover paths when not provided or when invalid/empty.
            let actualLocalrPath = String(localrPath || '').trim();
            let actualRoutinesPath = String(routinesPath || '').trim();
            let discoveryResult = null;

            let needsDiscovery = !actualLocalrPath || !actualRoutinesPath;

            if (!needsDiscovery) {
                const testLocalr = await this.execCommand(connectionId, `test -d "${actualLocalrPath}" && echo "exists" || echo "missing"`, 5000);
                const testRoutines = await this.execCommand(connectionId, `test -d "${actualRoutinesPath}" && echo "exists" || echo "missing"`, 5000);

                const localrExists = testLocalr.stdout.trim() === 'exists';
                const routinesExists = testRoutines.stdout.trim() === 'exists';
                console.log('[Docker Scanner] Directory existence check:', { localrExists, routinesExists });

                needsDiscovery = !localrExists || !routinesExists;

                if (localrExists && routinesExists) {
                    console.log('[Docker Scanner] Directories exist, checking if empty...');
                    const countCmd = `ls "${actualLocalrPath}"/*.m 2>/dev/null | wc -l`;
                    const countResult = await this.execCommand(connectionId, countCmd, 5000);
                    const fileCount = parseInt(countResult.stdout.trim()) || 0;
                    console.log('[Docker Scanner] File count in localr:', fileCount);
                    if (fileCount === 0) needsDiscovery = true;
                }
            }

            console.log('[Docker Scanner] needsDiscovery:', needsDiscovery);

            if (needsDiscovery) {
                console.warn('[Docker Scanner] Auto-discovering Vista paths...');

                const fromProfile = await this.discoverPathsFromVistaProfile(connectionId);
                if (fromProfile?.localrPath) {
                    actualLocalrPath = fromProfile.localrPath;
                    discoveryResult = fromProfile;
                }
                if (fromProfile?.routinesPath) actualRoutinesPath = fromProfile.routinesPath;

                if (!actualLocalrPath || !actualRoutinesPath) {
                    const discovered = await this.discoverPaths(connectionId);
                    if (discovered?.localrPath) actualLocalrPath = discovered.localrPath;
                    if (discovered?.routinesPath) actualRoutinesPath = discovered.routinesPath;
                    // Merge discovery results, preferring fromProfile if available
                    if (discovered) {
                        discoveryResult = {
                            ...discovered,
                            ...(fromProfile || {}),
                            localrPath: actualLocalrPath,
                            routinesPath: actualRoutinesPath
                        };
                    }
                }

                console.log('[Docker Scanner] Using paths:', { actualLocalrPath, actualRoutinesPath });
            } else {
                // Even if paths were provided, try to get metadata from vista-profile
                console.log('[Docker Scanner] Paths provided, enriching with metadata...');
                const fromProfile = await this.discoverPathsFromVistaProfile(connectionId);
                if (fromProfile) {
                    discoveryResult = {
                        ...fromProfile,
                        localrPath: actualLocalrPath,
                        routinesPath: actualRoutinesPath
                    };
                }
            }

            // UPDATE: Propagate discovered paths to connectionConfig so other code can use them
            if (actualLocalrPath || actualRoutinesPath) {
                const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
                const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;

                const pathsToApply = {
                    ok: true,
                    localrPath: actualLocalrPath,
                    routinesPath: actualRoutinesPath,
                    basePath: discoveryResult?.basePath || null,
                    gldPath: discoveryResult?.gldPath || null,
                    envKey: discoveryResult?.envKey || null
                };

                applyVistaProfilePathsToConfig(cfg, pathsToApply, { override: true });
                console.log('[Docker Scanner] Updated connectionConfig with paths:', pathsToApply);
            }

            let localrFiles, routinesFiles;

            if (routineNames && routineNames.length > 0) {
                // FAST PATH: Only scan specific routines from patch
                console.log(`[Docker Scanner] Scanning ${routineNames.length} specific routines from patch`);
                const results = await this.scanSpecificRoutines(connectionId, actualLocalrPath, actualRoutinesPath, routineNames);
                localrFiles = results.localr;
                routinesFiles = results.routines;
            } else {
                // SLOW PATH: Scan all routines (fallback)
                console.log('[Docker Scanner] Scanning all routines (no patch filter)');
                localrFiles = await this.listRoutines(connectionId, actualLocalrPath);
                routinesFiles = await this.listRoutines(connectionId, actualRoutinesPath);
            }

            console.log(`[Docker Scanner] Found ${localrFiles.length} routines in localr, ${routinesFiles.length} in routines`);

            // Build result with full path metadata
            const result = {
                environment: envName,
                scannedAt: new Date().toISOString(),
                paths: {
                    localr: actualLocalrPath,
                    routines: actualRoutinesPath,
                    basePath: discoveryResult?.basePath || null,
                    gldPath: discoveryResult?.gldPath || null,
                    envKey: discoveryResult?.envKey || null
                },
                localr: localrFiles,
                routines: routinesFiles,
                stats: {
                    localrCount: localrFiles.length,
                    routinesCount: routinesFiles.length
                }
            };

            return result;

        } catch (error) {
            console.error('[Docker Scanner] Scan error:', error);
            throw new Error(`Docker scan failed: ${error.message}`);
        }
    }

    /**
     * Scan SPECIFIC routines (from patch) - MUCH FASTER!
     * @param {string} connectionId - Docker container ID or SSH connection ID
     * @param {string} localrPath - Path to localr directory
     * @param {string} routinesPath - Path to routines directory
     * @param {Array<string>} routineNames - List of routine names to scan (e.g., ["XUINIT", "XUPAR"])
     * @returns {Promise<Object>} {localr: [...], routines: [...]}
     */
    async scanSpecificRoutines(connectionId, localrPath, routinesPath, routineNames) {
        console.log(`[Docker Scanner] Scanning ${routineNames.length} specific routines:`, routineNames.join(', '));

        const localrFiles = [];
        const routinesFiles = [];

        // Build commands to check specific files
        const fileList = routineNames.map(name => `${name}.m`).join(' ');

        // Check localr files with checksums (one command for all files)
        try {
            const localrCmd = `sh -c 'cd ${localrPath} && md5sum ${fileList} 2>/dev/null || true'`;
            const localrResult = await this.execCommand(connectionId, localrCmd, 30000);

            if (localrResult.stdout) {
                const lines = localrResult.stdout.split('\n').filter(l => l.trim());
                for (const line of lines) {
                    const parts = line.trim().split(/\s+/);
                    if (parts.length >= 2) {
                        const checksum = parts[0];
                        const basename = parts[1];
                        const routineName = basename.replace('.m', '');

                        localrFiles.push({
                            name: routineName,
                            basename: basename,
                            fullPath: `${localrPath}/${basename}`,
                            checksum: checksum
                        });
                    }
                }
            }
        } catch (error) {
            console.warn('[Docker Scanner] localr scan error:', error.message);
        }

        // Check routines files with checksums (one command for all files)
        try {
            const routinesCmd = `sh -c 'cd ${routinesPath} && md5sum ${fileList} 2>/dev/null || true'`;
            const routinesResult = await this.execCommand(connectionId, routinesCmd, 30000);

            if (routinesResult.stdout) {
                const lines = routinesResult.stdout.split('\n').filter(l => l.trim());
                for (const line of lines) {
                    const parts = line.trim().split(/\s+/);
                    if (parts.length >= 2) {
                        const checksum = parts[0];
                        const basename = parts[1];
                        const routineName = basename.replace('.m', '');

                        routinesFiles.push({
                            name: routineName,
                            basename: basename,
                            fullPath: `${routinesPath}/${basename}`,
                            checksum: checksum
                        });
                    }
                }
            }
        } catch (error) {
            console.warn('[Docker Scanner] routines scan error:', error.message);
        }

        console.log(`[Docker Scanner] Found ${localrFiles.length} in localr, ${routinesFiles.length} in routines`);

        return { localr: localrFiles, routines: routinesFiles };
    }

    /**
     * List routine files in a directory WITH checksums (much faster!)
     * @param {string} connectionId - Docker container ID or SSH connection ID
     * @param {string} path - Directory path
     * @returns {Promise<Array>} List of routine files with checksums
     */
    async listRoutines(connectionId, path) {
        try {
            // First check if directory exists
            const testCmd = `test -d "${path}" && echo "exists" || echo "missing"`;
            const testResult = await this.execCommand(connectionId, testCmd, 5000);

            if (testResult.stdout.trim() === 'missing') {
                console.warn(`[Docker Scanner] Directory not found: ${path}`);
                return []; // Return empty array if directory doesn't exist
            }

            // Get all .m files with checksums
            // Simpler approach: list files, then get checksums
            const listCmd = `ls "${path}"/*.m 2>/dev/null || true`;
            const listResult = await this.execCommand(connectionId, listCmd, 10000);

            if (!listResult.stdout || listResult.stdout.trim() === '') {
                console.log(`[Docker Scanner] No .m files found in ${path}`);
                return [];
            }

            const files = listResult.stdout.split('\n').filter(f => f.trim() && f.endsWith('.m'));
            if (files.length === 0) {
                console.log(`[Docker Scanner] No .m files found in ${path}`);
                return [];
            }

            console.log(`[Docker Scanner] Found ${files.length} .m files, calculating checksums...`);

            // Get checksums for all files
            const checksumCmd = `md5sum ${files.map(f => `"${f}"`).join(' ')}`;
            const result = await this.execCommand(connectionId, checksumCmd, 60000);

            if (result.code !== 0) {
                console.error(`[Docker Scanner] Checksum command failed for ${path}`);
                console.error(`[Docker Scanner] Exit code: ${result.code}`);
                console.error(`[Docker Scanner] Stderr: ${result.stderr}`);
                console.error(`[Docker Scanner] Stdout: ${result.stdout}`);
                // Don't throw - return empty array
                return [];
            }

            const routineFiles = result.stdout
                .split('\n')
                .filter(line => line.trim())
                .map(line => {
                    // Parse: "checksum  /path/to/file.m"
                    const parts = line.trim().split(/\s+/);
                    if (parts.length < 2) return null;

                    const checksum = parts[0];
                    const filePath = parts.slice(1).join(' '); // Handle spaces in path
                    const basename = filePath.split('/').pop();
                    const routineName = basename.replace('.m', '');

                    return {
                        name: routineName,
                        basename: basename,
                        fullPath: filePath.trim(),
                        checksum: checksum // MD5 checksum from remote
                    };
                })
                .filter(f => f !== null);

            console.log(`[Docker Scanner] Processed ${routineFiles.length} routines from ${path}`);
            return routineFiles;

        } catch (error) {
            console.error('[Docker Scanner] List routines error:', error);
            throw error;
        }
    }

    /**
     * Read routine file content
     * @param {string} connectionId - Docker container ID or SSH connection ID
     * @param {string} filePath - Full file path
     * @returns {Promise<string>} File content
     */
    async readRoutine(connectionId, filePath) {
        try {
            const command = `cat "${filePath}"`;
            const result = await this.execCommand(connectionId, command, 10000);

            if (result.code !== 0) {
                throw new Error(result.stderr || 'Failed to read routine');
            }

            return result.stdout;

        } catch (error) {
            console.error('[Docker Scanner] Read routine error:', error);
            throw error;
        }
    }

    /**
     * Get routine metadata (checksum, size, modified date)
     * @param {string} connectionId - Docker container ID or SSH connection ID
     * @param {string} filePath - Full file path
     * @returns {Promise<Object>} Routine metadata
     */
    async getRoutineMetadata(connectionId, filePath) {
        try {
            const command = `stat -c '%Y %s' "${filePath}"`;
            const result = await this.execCommand(connectionId, command, 5000);

            if (result.code !== 0) {
                throw new Error(result.stderr || 'Failed to get metadata');
            }

            const [modifiedTime, size] = result.stdout.trim().split(' ');

            // Read content to calculate checksum
            const content = await this.readRoutine(connectionId, filePath);
            const checksum = crypto.createHash('md5').update(content).digest('hex');

            return {
                modifiedTime: parseInt(modifiedTime) * 1000, // Convert to milliseconds
                size: parseInt(size),
                checksum,
                lines: content.split('\n').length
            };

        } catch (error) {
            console.error('[Docker Scanner] Get metadata error:', error);
            throw error;
        }
    }

    /**
     * Compare two routines using checksums (FAST - no file reading!)
     * @param {string} connectionId - Docker container ID or SSH connection ID
     * @param {string} localrPath - Path to localr routine
     * @param {string} routinesPath - Path to routines routine
     * @param {string} localrChecksum - Pre-computed checksum from listRoutines
     * @param {string} routinesChecksum - Pre-computed checksum from listRoutines
     * @returns {Promise<Object>} Comparison result
     */
    async compareRoutines(connectionId, localrPath, routinesPath, localrChecksum = null, routinesChecksum = null) {
        try {
            // If checksums provided, use them (FAST path - no file reading!)
            if (localrChecksum && routinesChecksum) {
                const isDifferent = localrChecksum !== routinesChecksum;

                // Get file sizes without reading content
                const sizeCmd = `stat -c '%s' "${localrPath}" "${routinesPath}"`;
                const sizeResult = await this.execCommand(connectionId, sizeCmd, 5000);
                const sizes = sizeResult.stdout.trim().split('\n').map(s => parseInt(s) || 0);

                return {
                    isDifferent,
                    localr: {
                        checksum: localrChecksum,
                        lines: 0, // Unknown without reading
                        size: sizes[0] || 0
                    },
                    routines: {
                        checksum: routinesChecksum,
                        lines: 0, // Unknown without reading
                        size: sizes[1] || 0
                    },
                    content: null // Don't read content unless needed
                };
            }

            // Fallback: read files and calculate checksums (SLOW path)
            const [localrContent, routinesContent] = await Promise.all([
                this.readRoutine(connectionId, localrPath),
                this.readRoutine(connectionId, routinesPath)
            ]);

            const localrChecksumCalc = crypto.createHash('md5').update(localrContent).digest('hex');
            const routinesChecksumCalc = crypto.createHash('md5').update(routinesContent).digest('hex');

            const isDifferent = localrChecksumCalc !== routinesChecksumCalc;

            return {
                isDifferent,
                localr: {
                    checksum: localrChecksumCalc,
                    lines: localrContent.split('\n').length,
                    size: localrContent.length
                },
                routines: {
                    checksum: routinesChecksumCalc,
                    lines: routinesContent.split('\n').length,
                    size: routinesContent.length
                },
                content: isDifferent ? {
                    localr: localrContent,
                    routines: routinesContent
                } : null
            };

        } catch (error) {
            console.error('[Docker Scanner] Compare routines error:', error);
            throw error;
        }
    }

    /**
     * Test connection to Docker container or SSH
     * @param {string} connectionId - Docker container ID or SSH connection ID
     * @returns {Promise<boolean>} Connection test result
     */
    async testConnection(connectionId) {
        try {
            const result = await this.execCommand(connectionId, 'echo "test"', 5000);

            return result.code === 0;

        } catch (error) {
            console.error('[Docker Scanner] Connection test failed:', error);
            return false;
        }
    }

    /**
     * Auto-discover Vista routine directories in container
     * @param {string} connectionId - Docker container ID or SSH connection ID
     * @returns {Promise<Object>} Discovered paths or null
     */
    async discoverPaths(connectionId) {
        try {
            console.log('[Docker Scanner] Auto-discovering Vista paths...');

            // Try to find common Vista base paths
            // Use sh -c to handle complex find command with parentheses
            const searchCmd = `sh -c 'find /var/worldvista -maxdepth 3 -type d \\( -name "localr" -o -name "routines" \\) 2>/dev/null | head -20'`;
            console.log('[Docker Scanner] Discovery command:', searchCmd);
            const result = await this.execCommand(connectionId, searchCmd, 15000);

            if (result.code !== 0 || !result.stdout) {
                console.warn('[Docker Scanner] Path discovery failed - exit code:', result.code);
                console.warn('[Docker Scanner] Discovery stderr:', result.stderr);
                console.warn('[Docker Scanner] Discovery stdout:', result.stdout);
                return null;
            }

            const foundPaths = result.stdout.split('\n').filter(p => p.trim());
            console.log('[Docker Scanner] Found', foundPaths.length, 'paths:', foundPaths);

            // Group by base path
            const pathGroups = {};
            foundPaths.forEach(path => {
                const parts = path.split('/');
                const dirName = parts[parts.length - 1]; // localr or routines
                const basePath = parts.slice(0, -1).join('/'); // /var/worldvista/prod/<env>

                if (!pathGroups[basePath]) {
                    pathGroups[basePath] = {};
                }
                pathGroups[basePath][dirName] = path;
            });

            // Find first base path with both localr and routines
            for (const [basePath, dirs] of Object.entries(pathGroups)) {
                if (dirs.localr && dirs.routines) {
                    console.log('[Docker Scanner] Discovered paths:', dirs);
                    return {
                        basePath,
                        localrPath: dirs.localr,
                        routinesPath: dirs.routines,
                        envKey: basePath.split('/').pop()
                    };
                }
            }

            // If we have at least one path, return it
            if (Object.keys(pathGroups).length > 0) {
                const basePath = Object.keys(pathGroups)[0];
                const dirs = pathGroups[basePath];
                console.log('[Docker Scanner] Partial paths found:', dirs);
                return {
                    basePath,
                    localrPath: dirs.localr || null,
                    routinesPath: dirs.routines || null,
                    envKey: basePath.split('/').pop()
                };
            }

            return null;

        } catch (error) {
            console.error('[Docker Scanner] Path discovery error:', error);
            return null;
        }
    }

    /**
     * Auto-discover Vista paths using /var/worldvista/prod/common/vista-profile
     * @returns {Promise<Object|null>} Discovered paths or null
     */
    async discoverPathsFromVistaProfile(connectionId) {
        try {
            const cmd = `sh -c 'cat /var/worldvista/prod/common/vista-profile 2>/dev/null || true'`;
            const res = await this.execCommand(connectionId, cmd, 5000);
            const text = String(res?.stdout || '').trim();
            if (!text) return null;

            const parsed = extractVistaPathsFromProfileText(text);
            const localr = parsed?.localrPath || null;
            const routines = parsed?.routinesPath || null;
            if (!localr && !routines) return null;

            return {
                basePath: parsed?.basePath || null,
                localrPath: localr,
                routinesPath: routines,
                envKey: parsed?.envKey || null
            };
        } catch (error) {
            console.warn('[Docker Scanner] vista-profile discovery error:', error?.message || error);
            return null;
        }
    }
}

// Export singleton instance
module.exports = new DockerScanner();
