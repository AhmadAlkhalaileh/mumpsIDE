/**
 * MUMPS Rename Transaction Manager
 * Atomic file updates with rollback for cross-routine rename
 */

(() => {
    function createMumpsRenameTransaction({ deps } = {}) {
        const writeFile = deps?.writeFile || (async (path, content) => {
            if (window.ahmadIDE?.writeFile) {
                await window.ahmadIDE.writeFile(path, content);
                return true;
            }
            throw new Error('File write API not available');
        });

        const readFile = deps?.readFile || (async (path) => {
            if (window.ahmadIDE?.readFile) {
                const result = await window.ahmadIDE.readFile(path);
                return result?.content || '';
            }
            throw new Error('File read API not available');
        });

        /**
         * Apply edits to file content
         * @param {string} content - Original file content
         * @param {Array} edits - Array of Monaco edit objects
         * @returns {string} Updated content
         */
        function applyEditsToContent(content, edits) {
            const lines = String(content || '').split('\n');

            // Group edits by line
            const perLine = new Map();
            edits.forEach(edit => {
                const range = edit.textEdit?.range;
                if (!range || range.startLineNumber !== range.endLineNumber) return;
                const ln = range.startLineNumber;
                if (!perLine.has(ln)) perLine.set(ln, []);
                perLine.get(ln).push(edit);
            });

            // Apply edits line by line
            for (const [lineNumber, lineEdits] of perLine.entries()) {
                if (lineNumber < 1 || lineNumber > lines.length) continue;

                const lineIdx = lineNumber - 1;
                const original = lines[lineIdx];

                // Sort edits by column (descending) to apply right-to-left
                const sorted = lineEdits.slice().sort((a, b) => {
                    return b.textEdit.range.startColumn - a.textEdit.range.startColumn;
                });

                let updated = original;
                for (const edit of sorted) {
                    const range = edit.textEdit.range;
                    const start = Math.max(0, range.startColumn - 1);
                    const end = Math.max(start, range.endColumn - 1);
                    const text = String(edit.textEdit.text ?? '');

                    updated = updated.slice(0, start) + text + updated.slice(end);
                }

                lines[lineIdx] = updated;
            }

            return lines.join('\n');
        }

        /**
         * Apply rename transaction atomically
         * @param {Map} fileEdits - Map of filePath -> {edits, model, filePath}
         * @param {Object} options - Transaction options
         * @returns {Object} Transaction result
         */
        async function applyTransaction(fileEdits, options = {}) {
            const { onProgress = null, dryRun = false } = options;

            const backup = new Map(); // filePath -> originalContent
            const applied = new Set(); // filePaths successfully written
            const errors = [];

            try {
                const files = Array.from(fileEdits.entries());
                const total = files.length;

                // Phase 1: Read and backup all files
                if (onProgress) onProgress({ phase: 'backup', current: 0, total });

                for (let i = 0; i < files.length; i++) {
                    const [filePath, fileData] = files[i];
                    try {
                        const content = await readFile(filePath);
                        backup.set(filePath, content);

                        if (onProgress) onProgress({ phase: 'backup', current: i + 1, total });
                    } catch (err) {
                        errors.push({ filePath, phase: 'backup', error: err.message });
                        throw new Error(`Failed to backup ${filePath}: ${err.message}`);
                    }
                }

                if (dryRun) {
                    return { success: true, filesAffected: files.length, dryRun: true };
                }

                // Phase 2: Apply edits and write files
                if (onProgress) onProgress({ phase: 'apply', current: 0, total });

                for (let i = 0; i < files.length; i++) {
                    const [filePath, fileData] = files[i];
                    try {
                        const originalContent = backup.get(filePath);
                        const updatedContent = applyEditsToContent(originalContent, fileData.edits);

                        await writeFile(filePath, updatedContent);
                        applied.add(filePath);

                        // If file has an open Monaco model, update it
                        if (fileData.model) {
                            try {
                                fileData.model.setValue(updatedContent);
                            } catch (_) {
                                // Model may not be available or writable
                            }
                        }

                        if (onProgress) onProgress({ phase: 'apply', current: i + 1, total });
                    } catch (err) {
                        errors.push({ filePath, phase: 'apply', error: err.message });
                        throw new Error(`Failed to write ${filePath}: ${err.message}`);
                    }
                }

                // Success!
                return {
                    success: true,
                    filesAffected: applied.size,
                    filePaths: Array.from(applied)
                };

            } catch (err) {
                // Phase 3: Rollback on error
                console.error('[Rename Transaction] Error, rolling back:', err);

                if (onProgress) onProgress({ phase: 'rollback', current: 0, total: applied.size });

                const rollbackErrors = [];
                let rollbackCount = 0;

                for (const filePath of applied) {
                    try {
                        const originalContent = backup.get(filePath);
                        if (originalContent != null) {
                            await writeFile(filePath, originalContent);
                            rollbackCount++;
                        }
                    } catch (rollbackErr) {
                        rollbackErrors.push({ filePath, error: rollbackErr.message });
                    }
                }

                if (onProgress) onProgress({ phase: 'rollback', current: rollbackCount, total: applied.size });

                return {
                    success: false,
                    error: err.message,
                    errors,
                    rollbackErrors,
                    filesRolledBack: rollbackCount,
                    filesAffected: 0
                };
            }
        }

        /**
         * Preview transaction (compute what would change without writing)
         * @param {Map} fileEdits - Map of filePath -> {edits, model, filePath}
         * @returns {Object} Preview result
         */
        async function previewTransaction(fileEdits) {
            const previews = [];

            for (const [filePath, fileData] of fileEdits.entries()) {
                try {
                    const content = await readFile(filePath);
                    const updated = applyEditsToContent(content, fileData.edits);
                    const linesChanged = countChangedLines(content, updated);

                    previews.push({
                        filePath,
                        linesChanged,
                        editsCount: fileData.edits.length,
                        sizeChange: updated.length - content.length
                    });
                } catch (err) {
                    previews.push({
                        filePath,
                        error: err.message
                    });
                }
            }

            return {
                filesAffected: previews.length,
                totalEdits: previews.reduce((sum, p) => sum + (p.editsCount || 0), 0),
                totalLinesChanged: previews.reduce((sum, p) => sum + (p.linesChanged || 0), 0),
                previews
            };
        }

        /**
         * Count number of lines changed between two texts
         */
        function countChangedLines(original, updated) {
            const origLines = String(original || '').split('\n');
            const updatedLines = String(updated || '').split('\n');
            const maxLen = Math.max(origLines.length, updatedLines.length);

            let changed = 0;
            for (let i = 0; i < maxLen; i++) {
                if (origLines[i] !== updatedLines[i]) {
                    changed++;
                }
            }
            return changed;
        }

        return {
            applyTransaction,
            previewTransaction,
            applyEditsToContent
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
        window.AhmadIDEModules.mumps.createMumpsRenameTransaction = createMumpsRenameTransaction;
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { createMumpsRenameTransaction };
    }
})();
