/**
 * MUMPS Smart Rename Engine
 * Cross-routine tag rename with safety detection and risk scoring
 */

(() => {
    function createMumpsSmartRename({ deps } = {}) {
        const getMonaco = deps?.getMonaco || (() => (typeof monaco !== 'undefined' ? monaco : null));
        const getSymbolIndexer = deps?.getSymbolIndexer || (() => null);
        const readFile = deps?.readFile || (async (path) => {
            if (window.ahmadIDE?.readFile) {
                const result = await window.ahmadIDE.readFile(path);
                return result?.content || '';
            }
            return '';
        });

        // Import helper functions from existing rename provider
        const isAlpha = (ch) => (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch === '%';
        const isDigit = (ch) => ch >= '0' && ch <= '9';
        const isNameChar = (ch) => isAlpha(ch) || isDigit(ch);

        const LABEL_RX = /^([A-Za-z%][A-Za-z0-9]*)(?=\s|;|\(|$)/;

        /**
         * Compute rename plan for cross-routine tag rename
         * @param {Object} params - Rename parameters
         * @returns {Object} Rename plan with edits, risk score, and metadata
         */
        async function computeSmartRename({
            model,
            oldTagName,
            newTagName,
            includeLocalChanges = true,
            includeCrossRoutineChanges = true,
            includeUnsafeChanges = false
        }) {
            const monacoRef = getMonaco();
            if (!monacoRef || !model) {
                return createEmptyPlan('Monaco or model not available');
            }

            const symbolIndexer = getSymbolIndexer();
            if (!symbolIndexer) {
                return createEmptyPlan('Symbol indexer not available');
            }

            const routineName = getModelRoutineName(model);
            const oldUpper = String(oldTagName).toUpperCase();
            const newUpper = String(newTagName).toUpperCase();

            // Collision detection
            const collisionCheck = checkCollisions(symbolIndexer, newTagName, routineName);
            if (collisionCheck.hasCollision) {
                return {
                    edits: [],
                    fileEdits: new Map(),
                    riskScore: 100,
                    riskLevel: 'high',
                    riskFactors: [`Tag "${newTagName}" already exists in routine "${collisionCheck.collisionRoutine}"`],
                    hasCollision: true,
                    collisionRoutine: collisionCheck.collisionRoutine,
                    totalChanges: 0,
                    filesAffected: 0
                };
            }

            // Get all references to the tag
            const allReferences = symbolIndexer.getTagReferences(oldTagName);

            // Categorize references
            const localRefs = allReferences.filter(ref =>
                String(ref.routine).toUpperCase() === String(routineName).toUpperCase()
            );
            const crossRoutineRefs = allReferences.filter(ref =>
                String(ref.routine).toUpperCase() !== String(routineName).toUpperCase() &&
                String(ref.targetRoutine).toUpperCase() === String(routineName).toUpperCase()
            );

            const safeRefs = crossRoutineRefs.filter(ref => ref.confidence === 'high');
            const unsafeRefs = crossRoutineRefs.filter(ref => ref.confidence !== 'high');

            // Collect file edits
            const fileEdits = new Map(); // filePath -> { edits: [], model: ... }

            // 1. Local changes in current file
            if (includeLocalChanges) {
                const localEdits = await computeLocalFileEdits(model, oldTagName, newTagName, monacoRef);
                if (localEdits.length > 0) {
                    const uri = model.uri;
                    fileEdits.set(uri.toString(), {
                        edits: localEdits,
                        model,
                        filePath: uri.fsPath || uri.path,
                        routine: routineName,
                        isLocal: true
                    });
                }
            }

            // 2. Cross-routine safe changes
            if (includeCrossRoutineChanges) {
                const refsToProcess = includeUnsafeChanges ? crossRoutineRefs : safeRefs;

                // Group refs by file
                const refsByFile = new Map();
                refsToProcess.forEach(ref => {
                    const fp = ref.filePath;
                    if (!refsByFile.has(fp)) refsByFile.set(fp, []);
                    refsByFile.get(fp).push(ref);
                });

                // Compute edits for each file
                for (const [filePath, refs] of refsByFile.entries()) {
                    const edits = await computeCrossRoutineFileEdits(
                        filePath,
                        refs,
                        oldTagName,
                        newTagName,
                        routineName,
                        monacoRef,
                        readFile
                    );

                    if (edits.length > 0) {
                        fileEdits.set(filePath, {
                            edits,
                            model: null, // Cross-routine files may not be open
                            filePath,
                            routine: refs[0]?.routine || '',
                            isLocal: false
                        });
                    }
                }
            }

            // Compute risk score
            const riskData = computeRiskScore({
                localRefs,
                safeRefs,
                unsafeRefs,
                filesAffected: fileEdits.size,
                hasUnsafeChanges: includeUnsafeChanges && unsafeRefs.length > 0
            });

            // Flatten edits for Monaco
            const allEdits = [];
            for (const fileData of fileEdits.values()) {
                allEdits.push(...fileData.edits);
            }

            return {
                edits: allEdits,
                fileEdits,
                riskScore: riskData.score,
                riskLevel: riskData.level,
                riskFactors: riskData.factors,
                hasCollision: false,
                totalChanges: allEdits.length,
                filesAffected: fileEdits.size,
                localChanges: localRefs.length,
                crossRoutineChanges: safeRefs.length,
                unsafeChanges: unsafeRefs.length
            };
        }

        /**
         * Check for name collisions
         */
        function checkCollisions(indexer, newTagName, currentRoutine) {
            const defs = indexer.getTagDefinitions(newTagName);
            for (const def of defs) {
                if (String(def.routine).toUpperCase() === String(currentRoutine).toUpperCase()) {
                    return { hasCollision: true, collisionRoutine: def.routine };
                }
            }
            return { hasCollision: false };
        }

        /**
         * Compute edits for local file (current routine)
         */
        async function computeLocalFileEdits(model, oldTagName, newTagName, monacoRef) {
            const edits = [];
            const routineName = getModelRoutineName(model);
            const oldUpper = String(oldTagName).toUpperCase();

            for (let lineNumber = 1; lineNumber <= model.getLineCount(); lineNumber++) {
                const line = model.getLineContent(lineNumber);

                // Check label definition
                const labelMatch = line.match(LABEL_RX);
                if (labelMatch && String(labelMatch[1]).toUpperCase() === oldUpper) {
                    edits.push({
                        resource: model.uri,
                        textEdit: {
                            range: new monacoRef.Range(lineNumber, 1, lineNumber, 1 + labelMatch[1].length),
                            text: newTagName
                        },
                        versionId: model.getVersionId()
                    });
                }

                // Find references in code (D TAG, G TAG, $$TAG)
                const codeEdits = findTagReferencesInLine(line, lineNumber, oldTagName, newTagName, monacoRef, model.uri, true);
                edits.push(...codeEdits);
            }

            return edits;
        }

        /**
         * Compute edits for cross-routine file
         */
        async function computeCrossRoutineFileEdits(filePath, refs, oldTagName, newTagName, targetRoutine, monacoRef, readFileFn) {
            const edits = [];
            const content = await readFileFn(filePath);
            if (!content) return edits;

            const lines = content.split('\n');
            const targetRoutineUpper = String(targetRoutine).toUpperCase();

            // Group refs by line for efficiency
            const refsByLine = new Map();
            refs.forEach(ref => {
                if (ref.line) {
                    if (!refsByLine.has(ref.line)) refsByLine.set(ref.line, []);
                    refsByLine.get(ref.line).push(ref);
                }
            });

            // Create a Monaco URI for this file
            const uri = monacoRef.Uri.file(filePath);

            for (const [lineNumber, lineRefs] of refsByLine.entries()) {
                if (lineNumber < 1 || lineNumber > lines.length) continue;
                const line = lines[lineNumber - 1];

                // Find TAG^ROUTINE patterns
                const codeEdits = findTagReferencesInLine(
                    line,
                    lineNumber,
                    oldTagName,
                    newTagName,
                    monacoRef,
                    uri,
                    false,
                    targetRoutineUpper
                );
                edits.push(...codeEdits);
            }

            return edits;
        }

        /**
         * Find and replace tag references in a line
         */
        function findTagReferencesInLine(line, lineNumber, oldTagName, newTagName, monacoRef, uri, isLocal, targetRoutine = null) {
            const edits = [];
            const oldUpper = String(oldTagName).toUpperCase();

            // Pattern: DO/GOTO/$ followed by TAG (with optional ^ROUTINE)
            // For local: D TAG, G TAG, $$TAG (no caret or same routine)
            // For cross-routine: D TAG^ROUTINE, G TAG^ROUTINE, $$TAG^ROUTINE

            const patterns = [
                // DO TAG^ROUTINE or DO TAG
                /\b(DO|D)\s+([A-Za-z%][A-Za-z0-9]*)(\^([A-Za-z%][A-Za-z0-9]*))?/gi,
                // GOTO TAG^ROUTINE or GOTO TAG
                /\b(GOTO|G)\s+([A-Za-z%][A-Za-z0-9]*)(\^([A-Za-z%][A-Za-z0-9]*))?/gi,
                // $$TAG^ROUTINE or $$TAG
                /\$\$([A-Za-z%][A-Za-z0-9]*)(\^([A-Za-z%][A-Za-z0-9]*))?/gi
            ];

            patterns.forEach(pattern => {
                let match;
                const regex = new RegExp(pattern);
                while ((match = regex.exec(line)) !== null) {
                    let tagName, routine;

                    if (match[0].startsWith('$$')) {
                        // $$TAG^ROUTINE
                        tagName = match[1];
                        routine = match[3];
                    } else {
                        // DO/GOTO TAG^ROUTINE
                        tagName = match[2];
                        routine = match[4];
                    }

                    if (!tagName) continue;

                    const tagUpper = String(tagName).toUpperCase();
                    if (tagUpper !== oldUpper) continue;

                    // Check if this is the right context (local vs cross-routine)
                    if (isLocal) {
                        // Only replace if no routine or same routine
                        if (routine) continue;
                    } else {
                        // Only replace if it references the target routine
                        if (!routine) continue;
                        const routineUpper = String(routine).toUpperCase();
                        if (targetRoutine && routineUpper !== targetRoutine) continue;
                    }

                    // Calculate position of tag name in match
                    const matchStart = match.index;
                    let tagStartInMatch;

                    if (match[0].startsWith('$$')) {
                        tagStartInMatch = 2; // Skip $$
                    } else {
                        tagStartInMatch = match[0].indexOf(tagName);
                    }

                    const tagStart = matchStart + tagStartInMatch;
                    const tagEnd = tagStart + tagName.length;

                    edits.push({
                        resource: uri,
                        textEdit: {
                            range: new monacoRef.Range(lineNumber, tagStart + 1, lineNumber, tagEnd + 1),
                            text: newTagName
                        }
                    });
                }
            });

            return edits;
        }

        /**
         * Compute risk score (0-100)
         */
        function computeRiskScore({ localRefs, safeRefs, unsafeRefs, filesAffected, hasUnsafeChanges }) {
            let score = 0;
            const factors = [];

            // Base risk: number of files affected
            if (filesAffected === 1) {
                score += 5;
            } else if (filesAffected <= 3) {
                score += 15;
                factors.push(`${filesAffected} files affected`);
            } else if (filesAffected <= 10) {
                score += 30;
                factors.push(`${filesAffected} files affected`);
            } else {
                score += 50;
                factors.push(`${filesAffected} files affected (high)`);
            }

            // Risk from cross-routine changes
            if (safeRefs.length > 0) {
                const refScore = Math.min(20, Math.floor(safeRefs.length / 5) * 5);
                score += refScore;
                if (safeRefs.length > 5) {
                    factors.push(`${safeRefs.length} cross-routine references`);
                }
            }

            // Risk from unsafe changes
            if (hasUnsafeChanges) {
                score += 40;
                factors.push(`${unsafeRefs.length} uncertain/unsafe references included`);
            } else if (unsafeRefs.length > 0) {
                score += 10;
                factors.push(`${unsafeRefs.length} uncertain references (not included)`);
            }

            // Determine level
            let level;
            if (score <= 20) {
                level = 'low';
            } else if (score <= 60) {
                level = 'medium';
            } else {
                level = 'high';
            }

            if (factors.length === 0 && localRefs.length > 0) {
                factors.push('Local changes only');
            }

            return {
                score: Math.min(100, score),
                level,
                factors
            };
        }

        /**
         * Get routine name from model
         */
        function getModelRoutineName(model) {
            try {
                const uri = model?.uri;
                const p = uri?.path || uri?.fsPath || uri?.toString?.() || '';
                const base = String(p).split(/[\\/]/).pop() || '';
                const noExt = base.replace(/\.[^.]+$/g, '');
                return String(noExt || '').toUpperCase();
            } catch (_) {
                return '';
            }
        }

        /**
         * Create empty plan
         */
        function createEmptyPlan(reason) {
            return {
                edits: [],
                fileEdits: new Map(),
                riskScore: 0,
                riskLevel: 'low',
                riskFactors: [reason],
                hasCollision: false,
                totalChanges: 0,
                filesAffected: 0
            };
        }

        return {
            computeSmartRename
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
        window.AhmadIDEModules.mumps.createMumpsSmartRename = createMumpsSmartRename;
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { createMumpsSmartRename };
    }
})();
