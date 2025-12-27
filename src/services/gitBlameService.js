/**
 * Git Blame Service
 * Provides line-by-line authorship information from Git repositories
 */

const { execFile } = require('child_process');
const util = require('util');
const execFileAsync = util.promisify(execFile);
const path = require('path');
const fs = require('fs');
const { fileURLToPath } = require('url');

class GitBlameService {
    constructor() {
        this.blameCache = new Map(); // Cache blame results
        this.repoRoots = new Map(); // Cache repo root paths
        this.gitRepoPath = null; // Path to Git repository (e.g., ~/Desktop/vista-routines)
        this.configuredRepoRoot = null; // Cached `git rev-parse --show-toplevel` for gitRepoPath
    }

    /**
     * Set Git repository path for Docker file mapping
     * @param {string} repoPath - Path to Git repository
     */
    setGitRepoPath(repoPath) {
        const os = require('os');

        // Expand ~ to home directory
        let expandedPath = String(repoPath || '').trim();
        if (expandedPath === '~') {
            expandedPath = os.homedir();
        } else if (expandedPath.startsWith('~/')) {
            expandedPath = path.join(os.homedir(), expandedPath.slice(2));
        }

        this.gitRepoPath = path.resolve(expandedPath);
        this.configuredRepoRoot = null;
        this.clearAllCaches();
    }

    /**
     * Normalize Monaco URI strings into filesystem paths when possible.
     * @param {string} filePath
     * @returns {string}
     */
    normalizeFilePath(filePath) {
        const raw = String(filePath || '').trim();
        if (!raw) return raw;

        if (raw.startsWith('file://')) {
            try {
                return fileURLToPath(raw);
            } catch (_) {
                return raw;
            }
        }

        return raw;
    }

    /**
     * Map Docker/remote file path to local Git repository file
     * @param {string} filePath - Original file path (might be Docker path)
     * @returns {string} Mapped file path in Git repository
     */
    mapToGitRepo(filePath) {
        if (!this.gitRepoPath) {
            return filePath; // No mapping configured
        }


        const raw = String(filePath || '').trim();
        if (!raw) return raw;

        // If the path exists locally, treat it as a real file path and don't remap.
        // (Prevents breaking blame for normal `file:///...` editor models.)
        try {
            if (path.isAbsolute(raw) && fs.existsSync(raw)) {
                return raw;
            }
        } catch (_) { }

        const ensureMExt = (name) => {
            const n = String(name || '').trim();
            if (!n) return n;
            return n.toLowerCase().endsWith('.m') ? n : `${n}.m`;
        };

        const stripQueryFragment = (val) => String(val || '').split(/[?#]/)[0];

        let routineName = null;
        let subdir = null; // localr or routines

        // Handle Monaco URI format: docker:/path
        if (raw.startsWith('docker:')) {
            // Remove 'docker:' prefix
            const uriPath = stripQueryFragment(raw).replace(/^docker:\/*/gi, '');

            // Check if it has localr or routines folder
            if (uriPath.includes('localr/')) {
                subdir = 'localr';
                const parts = uriPath.split('/');
                const routineBase = parts[parts.length - 1];
                routineName = ensureMExt(routineBase);
            } else if (uriPath.includes('routines/')) {
                subdir = 'routines';
                const parts = uriPath.split('/');
                const routineBase = parts[parts.length - 1];
                routineName = ensureMExt(routineBase);
            } else {
                // No folder specified, default to localr
                subdir = 'localr';
                const routineBase = uriPath.split('/').pop();
                routineName = ensureMExt(routineBase);
            }
        }
        // Handle regular file paths
        else if (raw.includes('/localr/')) {
            subdir = 'localr';
            routineName = ensureMExt(path.basename(stripQueryFragment(raw)));
        } else if (raw.includes('/routines/')) {
            subdir = 'routines';
            routineName = ensureMExt(path.basename(stripQueryFragment(raw)));
        } else if (raw.endsWith('.m')) {
            // Just a routine name like "LRAPD1.m"
            routineName = path.basename(stripQueryFragment(raw));
            subdir = 'localr'; // Default to localr
        } else if (!raw.includes('/') && !raw.includes('\\') && /^[A-Za-z%][A-Za-z0-9%]*$/.test(raw)) {
            // Bare routine name like "LRAPD1"
            routineName = ensureMExt(raw);
            subdir = 'localr';
        } else {
            // Not a routine file, return as-is
            return filePath;
        }

        // Build mapped path
        const primary = path.join(this.gitRepoPath, subdir, routineName);
        let mappedPath = primary;

        // Fallback: if the primary doesn't exist but the alternate does, use the alternate.
        try {
            if (!fs.existsSync(primary)) {
                const altSubdir = subdir === 'localr' ? 'routines' : 'localr';
                const alt = path.join(this.gitRepoPath, altSubdir, routineName);
                if (fs.existsSync(alt)) mappedPath = alt;
            }
        } catch (_) { }

        return mappedPath;
    }

    async getConfiguredRepoRoot() {
        if (!this.gitRepoPath) return null;
        if (this.configuredRepoRoot) return this.configuredRepoRoot;

        try {
            const cwd = this.gitRepoPath;
            if (!cwd || !fs.existsSync(cwd)) return null;
            const { stdout } = await execFileAsync('git', ['rev-parse', '--show-toplevel'], {
                cwd,
                timeout: 5000
            });
            const repoRoot = String(stdout || '').trim();
            this.configuredRepoRoot = repoRoot || null;
            return this.configuredRepoRoot;
        } catch (_) {
            this.configuredRepoRoot = null;
            return null;
        }
    }

    /**
     * Find Git repository root for a file
     * @param {string} filePath - Absolute path to file
     * @returns {Promise<string|null>} Repository root path or null
     */
    async findGitRepo(filePath) {
        // Check cache first
        if (this.repoRoots.has(filePath)) {
            return this.repoRoots.get(filePath);
        }

        try {
            let dir = path.dirname(filePath);
            while (dir && !fs.existsSync(dir)) {
                const parent = path.dirname(dir);
                if (!parent || parent === dir) break;
                dir = parent;
            }
            if (!dir || !fs.existsSync(dir)) {
                this.repoRoots.set(filePath, null);
                return null;
            }

            const { stdout } = await execFileAsync('git', ['rev-parse', '--show-toplevel'], {
                cwd: dir,
                timeout: 5000
            });

            const repoRoot = stdout.trim();
            this.repoRoots.set(filePath, repoRoot);
            return repoRoot;
        } catch (error) {
            this.repoRoots.set(filePath, null);
            return null;
        }
    }

    /**
     * Parse inline patch markers from file content (MUMPS format)
     * SMART: Detects exact change by looking at marker + scope + old/new code pattern
     * @param {string} fileContent - File content
     * @returns {Map} Map of lineNumber -> {patchId, patchAuthor, patchDate}
     */
    parseInlinePatchMarkers(fileContent) {
        const patchMap = new Map();
        if (!fileContent) return patchMap;

        const lines = fileContent.split('\n');

        // Regex to match: ;DEPT/CODE ;PATCH*ID ; DATE ;description
        // Can have leading whitespace or dots (MUMPS indentation)
        const patchMarkerRegex = /;([A-Z]+)\/([A-Z]+)\s*;([A-Z]+\*[\d.]+\*\d+)\s*;\s*([A-Z]{3}\s+\d+,\d{4})/i;

        // Map codes to full names
        const authorMap = {
            'OSS': 'Osama Swies',
            'AKH': 'Ahmad Alkhalaileh',
            'EHS': 'EHS'
        };

        for (let i = 0; i < lines.length; i++) {
            const lineNumber = i + 1;
            const line = lines[i];

            // Check for patch marker
            const match = line.match(patchMarkerRegex);
            if (match) {
                const authorCode = match[2];
                const patchId = match[3];
                const patchDate = match[4];
                const patchAuthor = authorMap[authorCode] || authorCode;

                const patchInfo = { patchId, patchAuthor, patchDate };

                // Get indentation prefix of marker line (e.g., "\t       " or "       ." or "\t       .")
                const markerPrefix = line.substring(0, line.indexOf(';'));

                // Calculate indentation level (treating tabs as 8 spaces for comparison)
                const markerIndentLevel = markerPrefix.replace(/\t/g, '        ').length;

                // Mark the marker line itself
                patchMap.set(lineNumber, { ...patchInfo });

                // Look ahead for related lines
                // Strategy: Mark ONLY lines that match the pattern exactly
                const markedLines = [lineNumber]; // Start with marker line itself

                for (let j = 1; j <= 10 && (i + j) < lines.length; j++) {
                    const nextLine = lines[i + j];
                    const nextLineNum = lineNumber + j;

                    // Extract leading whitespace from next line
                    const nextLineMatch = nextLine.match(/^(\s*)/);
                    const nextLinePrefix = nextLineMatch ? nextLineMatch[1] : '';
                    const nextLineIndentLevel = nextLinePrefix.replace(/\t/g, '        ').length;

                    const afterPrefix = nextLine.substring(nextLinePrefix.length);
                    let trimmedAfterPrefix = afterPrefix.trim();

                    // Stop conditions:
                    if (!trimmedAfterPrefix) {
                        // Empty line - stop
                        break;
                    }

                    // Handle MUMPS dotted syntax - skip leading dots for comment detection
                    const withoutDots = trimmedAfterPrefix.replace(/^\.+\s*/, '');
                    const isComment = withoutDots.startsWith(';');

                    if (isComment) {
                        // It's a comment line - must have same indentation as marker
                        if (Math.abs(nextLineIndentLevel - markerIndentLevel) > 1) {
                            // Different indentation - stop here
                            break;
                        }

                        const commentContent = withoutDots.substring(1).trim();

                        // Check if it's a scope/update comment OR old code comment
                        const isScopeComment = /^(Scope|Update|Change|Modified|Added|Deleted)/i.test(commentContent);
                        // Match MUMPS commands: optional dot, optional spaces, then command followed by space/non-letter/end
                        // List includes both full names (WRITE, TCOMMIT) and abbreviations (W, TC)
                        // Sorted by length (longest first) to match WRITE before W
                        const isOldCode = /^\.?\s*(TCOMMIT|XECUTE|TSTART|WRITE|CLOSE|MERGE|KILL|QUIT|GOTO|HALT|READ|OPEN|LOCK|VIEW|ELSE|SET|FOR|NEW|USE|JOB|DO|IF|TC|SELECT|S|W|D|I|F|K|Q|N|X|G|H|R|U|C|O|L|M|V|J|E|T)(\s|[^A-Z]|$)/i.test(commentContent);

                        if (isScopeComment || isOldCode) {
                            markedLines.push(nextLineNum);
                        } else {
                            // Other comment type - stop
                            break;
                        }
                    } else {
                        // It's a code line - check if it's a MUMPS command
                        // Allow less indentation (code line may be dedented compared to comments)
                        // but must be at least at the base level (tab indent = 8)
                        const isCodeLine = /^\.?\s*(TCOMMIT|XECUTE|TSTART|WRITE|CLOSE|MERGE|KILL|QUIT|GOTO|HALT|READ|OPEN|LOCK|VIEW|ELSE|SET|FOR|NEW|USE|JOB|DO|IF|TC|SELECT|S|W|D|I|F|K|Q|N|X|G|H|R|U|C|O|L|M|V|J|E|T)(\s|[^A-Z]|$)/i.test(trimmedAfterPrefix);

                        if (isCodeLine && nextLineIndentLevel >= 8 && nextLineIndentLevel <= markerIndentLevel) {
                            // Valid code line within expected indentation range - mark it and stop
                            markedLines.push(nextLineNum);
                            break;
                        } else {
                            // Not a valid code line or wrong indentation - stop without marking
                            break;
                        }
                    }
                }

                // Apply patch info to all marked lines
                for (const ln of markedLines) {
                    patchMap.set(ln, { ...patchInfo });
                }
            }
        }

        return patchMap;
    }

    /**
     * Get Git blame information for a file
     * @param {string} filePath - Absolute path to file
     * @returns {Promise<Array>} Array of blame info per line
     */
    async getBlame(filePath) {
        const normalizedPath = this.normalizeFilePath(filePath);

        // Map to Git repository if configured (for Docker files)
        const mappedPath = this.mapToGitRepo(normalizedPath);

        // Check if file is in a Git repo
        let repoRoot = null;
        const looksRemote =
            String(filePath || '').startsWith('docker:') ||
            (String(normalizedPath || '').startsWith('/') && !fs.existsSync(normalizedPath));

        if (looksRemote && !this.gitRepoPath) {
            return null;
        }

        if (this.gitRepoPath && looksRemote) {
            repoRoot = await this.getConfiguredRepoRoot();
        }
        if (!repoRoot) {
            repoRoot = await this.findGitRepo(mappedPath);
        }
        if (!repoRoot) {
            return null;
        }

        // Check cache
        const cacheKey = filePath;
        if (this.blameCache.has(cacheKey)) {
            const cached = this.blameCache.get(cacheKey);
            // Verify cache has patchAuthor field (new format)
            const hasNewFormat = cached.length > 0 && cached.some(b => 'patchAuthor' in b);
            if (hasNewFormat) {
                return cached;
            } else {
                this.blameCache.delete(cacheKey);
            }
        }

        try {
            const relPath = path.relative(repoRoot, mappedPath);
            if (!relPath || relPath.startsWith('..') || path.isAbsolute(relPath)) {
                throw new Error(`File is not under repo root: ${mappedPath}`);
            }

            // Run git blame with porcelain format for easier parsing
            // --line-porcelain gives full commit info per line
            const { stdout } = await execFileAsync('git', ['blame', '--line-porcelain', '--', relPath], {
                cwd: repoRoot,
                timeout: 30000,
                maxBuffer: 10 * 1024 * 1024 // 10MB
            });

            const blameData = this.parseBlameOutput(stdout);

            // Enrich with commit messages to extract patch IDs
            await this.enrichWithCommitMessages(blameData, repoRoot);

            // Parse inline patch markers from file content (OVERRIDES Git commit markers)
            try {
                const fileContent = fs.readFileSync(mappedPath, 'utf8');
                const inlinePatchMap = this.parseInlinePatchMarkers(fileContent);

                // Apply inline markers - they take priority over Git commit markers
                for (const blame of blameData) {
                    const inlineInfo = inlinePatchMap.get(blame.lineNumber);
                    if (inlineInfo) {
                        blame.patchId = inlineInfo.patchId;
                        blame.patchAuthor = inlineInfo.patchAuthor;
                        blame.patchDate = inlineInfo.patchDate || null;
                        blame.inlineMarker = true; // Flag to indicate this came from inline marker
                    }
                }
            } catch (_) { }

            // Cache the result
            this.blameCache.set(cacheKey, blameData);
            return blameData;

        } catch (error) {
            // If git blame fails, check if this is a NEW file that exists locally
            // In that case, we can still provide patch information if available
            console.log(`[Git Blame] git blame failed for ${mappedPath}, checking if new file:`, error.message);

            try {
                // Check if file exists locally (new file not yet committed)
                if (fs.existsSync(mappedPath)) {
                    console.log('[Git Blame] File exists but not in Git - creating synthetic blame for new file');

                    // Read file to get line count and parse inline markers
                    const fileContent = fs.readFileSync(mappedPath, 'utf8');
                    const lines = fileContent.split('\n');
                    const inlinePatchMap = this.parseInlinePatchMarkers(fileContent);

                    // Create synthetic blame data for new file
                    const syntheticBlame = lines.map((line, idx) => {
                        const lineNumber = idx + 1;
                        const inlineInfo = inlinePatchMap.get(lineNumber);

                        return {
                            lineNumber,
                            hash: '0000000000000000000000000000000000000000', // Uncommitted
                            hashShort: '0000000',
                            author: inlineInfo?.patchAuthor || 'Not Committed',
                            authorMail: '',
                            date: inlineInfo?.patchDate ? new Date(inlineInfo.patchDate) : new Date(),
                            summary: 'New file - not yet committed to Git',
                            patchId: inlineInfo?.patchId || null,
                            patchAuthor: inlineInfo?.patchAuthor || null,
                            patchDate: inlineInfo?.patchDate || null,
                            inlineMarker: !!inlineInfo,
                            isNewFile: true // Flag to indicate this is a new file
                        };
                    });

                    // Cache and return
                    this.blameCache.set(cacheKey, syntheticBlame);
                    return syntheticBlame;
                }
            } catch (readError) {
                console.error('[Git Blame] Failed to create synthetic blame:', readError);
            }

            return null;
        }
    }

    /**
     * Parse git blame --line-porcelain output
     * @param {string} output - Raw git blame output
     * @returns {Array} Parsed blame data
     */
    parseBlameOutput(output) {
        const lines = output.split('\n');
        const blameData = [];
        let currentCommit = null;
        let currentLine = null;

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];

            // Commit hash line: "hash lineNum finalLineNum numLines"
            if (line.match(/^[0-9a-f]{40}\s+\d+\s+\d+/)) {
                const parts = line.split(/\s+/);
                currentCommit = {
                    hash: parts[0],
                    originalLine: parseInt(parts[1]),
                    finalLine: parseInt(parts[2]),
                    numLines: parseInt(parts[3] || 1)
                };
            }
            // Author name
            else if (line.startsWith('author ')) {
                if (currentCommit) {
                    currentCommit.author = line.substring(7);
                }
            }
            // Author email
            else if (line.startsWith('author-mail ')) {
                if (currentCommit) {
                    currentCommit.authorEmail = line.substring(12).replace(/[<>]/g, '');
                }
            }
            // Author time
            else if (line.startsWith('author-time ')) {
                if (currentCommit) {
                    currentCommit.authorTime = parseInt(line.substring(12));
                    currentCommit.authorDate = new Date(currentCommit.authorTime * 1000);
                }
            }
            // Summary (commit message first line)
            else if (line.startsWith('summary ')) {
                if (currentCommit) {
                    currentCommit.summary = line.substring(8);
                }
            }
            // The actual source line (starts with tab)
            else if (line.startsWith('\t')) {
                if (currentCommit) {
                    blameData.push({
                        lineNumber: currentCommit.finalLine,
                        hash: currentCommit.hash,
                        hashShort: currentCommit.hash.substring(0, 8),
                        author: currentCommit.author,
                        authorEmail: currentCommit.authorEmail,
                        date: currentCommit.authorDate,
                        summary: currentCommit.summary,
                        patchId: null, // Will be filled by enrichWithCommitMessages
                        patchAuthor: null, // Will be filled by enrichWithCommitMessages (from commit message)
                        content: line.substring(1) // Remove leading tab
                    });
                }
            }
        }

        return blameData;
    }

    /**
     * Enrich blame data with commit messages to extract patch IDs
     * ONLY applies patch author to lines that were ACTUALLY CHANGED in the patch commit
     * @param {Array} blameData - Blame data to enrich
     * @param {string} repoRoot - Repository root path
     */
    async enrichWithCommitMessages(blameData, repoRoot) {
        // Get unique commit hashes
        const uniqueHashes = [...new Set(blameData.map(b => b.hash))];

        // Fetch full commit messages for each unique hash
        const commitInfo = new Map();

        for (const hash of uniqueHashes) {
            try {
                const { stdout } = await execFileAsync('git', ['log', '-1', '--format=%B', hash], {
                    cwd: repoRoot,
                    timeout: 5000
                });

                const message = stdout.trim();
                const patchId = this.extractPatchId(message);
                let patchAuthor = this.extractPatchAuthor(message);

                // Fallback to Co-Authored-By if no patch author found
                if (!patchAuthor) {
                    const coAuthor = this.extractCoAuthor(message);
                    patchAuthor = coAuthor ? coAuthor.name : null;
                }

                commitInfo.set(hash, { patchId, patchAuthor });
            } catch (_) {
                commitInfo.set(hash, { patchId: null, patchAuthor: null });
            }
        }

        // For commits with patch metadata, check which lines were ACTUALLY modified
        // Only those lines should show the patch author
        const linesModifiedInCommit = new Map(); // hash -> Set of line numbers

        for (const hash of uniqueHashes) {
            const info = commitInfo.get(hash);
            if (!info || !info.patchId || !info.patchAuthor) continue;

            // Get which lines this commit actually modified
            try {
                // Use git show with --numstat or git diff
                const { stdout } = await execFileAsync('git', ['show', '--format=', '--unified=0', hash], {
                    cwd: repoRoot,
                    timeout: 5000
                });

                const modifiedLines = new Set();
                const lines = stdout.split('\n');
                let inHunk = false;
                let currentLine = 0;

                for (const line of lines) {
                    // Look for hunk headers like @@ -5,1 +5,1 @@
                    const hunkMatch = line.match(/^@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@/);
                    if (hunkMatch) {
                        currentLine = parseInt(hunkMatch[1], 10);
                        const count = parseInt(hunkMatch[2] || '1', 10);
                        inHunk = true;
                        continue;
                    }

                    if (inHunk) {
                        if (line.startsWith('+') && !line.startsWith('+++')) {
                            // This line was added/modified
                            modifiedLines.add(currentLine);
                            currentLine++;
                        } else if (line.startsWith(' ')) {
                            // Context line
                            currentLine++;
                        } else if (line.startsWith('-') && !line.startsWith('---')) {
                            // Line removed, don't increment
                        } else if (line.startsWith('diff ') || line.startsWith('index ')) {
                            inHunk = false;
                        }
                    }
                }

                if (modifiedLines.size > 0) {
                    linesModifiedInCommit.set(hash, modifiedLines);
                }
            } catch (_) { }
        }

        // Update blame data with patch IDs/authors
        // ONLY apply patch author if the line was actually modified in that commit
        for (const blame of blameData) {
            const info = commitInfo.get(blame.hash) || {};

            // Check if this line was actually modified in the commit
            const modifiedLines = linesModifiedInCommit.get(blame.hash);
            const wasModifiedInCommit = modifiedLines ? modifiedLines.has(blame.lineNumber) : false;

            if (info.patchId && info.patchAuthor && wasModifiedInCommit) {
                // Line was actually changed in patch commit -> show patch author
                blame.patchId = info.patchId;
                blame.patchAuthor = info.patchAuthor;
            } else if (info.patchId && !wasModifiedInCommit) {
                // Commit has patch metadata but this line wasn't modified -> don't show patch info
                blame.patchId = null;
                blame.patchAuthor = null;
            } else {
                // No patch info
                blame.patchId = info.patchId || null;
                blame.patchAuthor = info.patchAuthor || null;
            }
        }
    }

    /**
     * Extract patch ID from commit message
     * @param {string} message - Commit message
     * @returns {string|null} Patch ID or null
     */
    extractPatchId(message) {
        // Look for patterns like:
        // "Applied patch UJO*3.0*28"
        // "UJO*3.0*28"
        // "Patch: UJO*3.0*28"

        const patterns = [
            /Applied patch ([A-Z]+\*[\d.]+\*\d+)/i,
            /Patch[:\s]+([A-Z]+\*[\d.]+\*\d+)/i,
            /\b([A-Z]{2,}\*[\d.]+\*\d+)\b/
        ];

        for (const pattern of patterns) {
            const match = message.match(pattern);
            if (match) {
                return match[1];
            }
        }

        return null;
    }

    /**
     * Extract patch author from commit message
     * @param {string} message - Commit message
     * @returns {string|null} Patch author or null
     */
    extractPatchAuthor(message) {
        const text = String(message || '');

        const patterns = [
            /Patch Author:\s*([^\n]+)/im,              // "Patch Author: Name"
            /Patch-Author:\s*([^\n]+)/im,              // "Patch-Author: Name"
            /-\s*Patch Author:\s*([^\n]+)/im,          // "- Patch Author: Name"
            /-\s*Author:\s*([^\n]+)/im,                // "- Author: Name"
            /Author:\s*([^\n]+)/im,                    // "Author: Name"
            /Created (?:On|By) [^\n]+ By ([^\n]+)/im   // "Created On ... By Name"
        ];

        for (const pattern of patterns) {
            const match = text.match(pattern);
            if (match) {
                const value = String(match[1] || '').trim();
                if (value) return value;
            }
        }

        return null;
    }

    /**
     * Extract Co-Authored-By information from commit message
     * @param {string} message - Commit message
     * @returns {Object|null} {name, email} or null
     */
    extractCoAuthor(message) {
        // Look for: Co-Authored-By: Osama Swies <osama.swies@ehs.com.jo>
        const pattern = /Co-Authored-By:\s*([^<]+)\s*<([^>]+)>/i;
        const match = message.match(pattern);

        if (match) {
            return {
                name: match[1].trim(),
                email: match[2].trim()
            };
        }

        return null;
    }

    /**
     * Get blame for a specific line number
     * @param {string} filePath - File path
     * @param {number} lineNumber - Line number (1-based)
     * @returns {Promise<Object|null>} Blame info for the line
     */
    async getBlameForLine(filePath, lineNumber) {
        const blameData = await this.getBlame(filePath);
        if (!blameData) return null;

        const blame = blameData.find(b => b.lineNumber === lineNumber) || null;
        return blame;
    }

    /**
     * Clear cache for a file (call when file is modified)
     * @param {string} filePath - File path
     */
    clearCache(filePath) {
        this.blameCache.delete(filePath);
    }

    /**
     * Clear all caches
     */
    clearAllCaches() {
        this.blameCache.clear();
        this.repoRoots.clear();
    }

    /**
     * Format blame info for display
     * @param {Object} blame - Blame data
     * @returns {string} Formatted string
     */
    formatBlame(blame) {
        if (!blame) return 'Not committed';

        const author = blame.patchAuthor || blame.author || 'Unknown';
        const date = blame.date ? blame.date.toLocaleDateString() : 'Unknown date';
        const patch = blame.patchId ? ` • Patch: ${blame.patchId}` : '';

        return `${author} • ${date}${patch}`;
    }

    /**
     * Format blame info for inline annotation (short version)
     * @param {Object} blame - Blame data
     * @returns {string} Formatted string
     */
    formatBlameInline(blame) {
        if (!blame) return '';

        const authorSrc = blame.patchAuthor || blame.author;
        const author = authorSrc ? String(authorSrc).split(' ')[0] : 'Unknown'; // First name only
        const patch = blame.patchId || 'No patch';

        return `${author} • ${patch}`;
    }
}

// Export singleton instance
module.exports = new GitBlameService();
