(() => {
    const normalizeNewlines = (text) => String(text || '').replace(/\r\n/g, '\n');

    /**
     * Parse `find` output into a list of .m routine paths.
     * Filters common noise lines (e.g., permission errors).
     */
    function parseFindOutput(output) {
        return normalizeNewlines(output)
            .split('\n')
            .map((line) => line.trim())
            .filter((line) => {
                if (!line) return false;
                if (!line.endsWith('.m')) return false;
                // ignore common find noise (in case stderr is merged into stdout)
                if (line.startsWith('find:')) return false;
                if (/permission denied/i.test(line)) return false;
                return true;
            });
    }

    /**
     * Simple, line-based diff suitable for routines.
     * Returns an array of changes with 1-based line numbers.
     */
    function calculateLineDiff(localContent, remoteContent) {
        const localLines = normalizeNewlines(localContent).split('\n');
        const remoteLines = normalizeNewlines(remoteContent).split('\n');

        const changes = [];
        const maxLines = Math.max(localLines.length, remoteLines.length);

        for (let i = 0; i < maxLines; i += 1) {
            const localLine = localLines[i] || '';
            const remoteLine = remoteLines[i] || '';
            if (localLine === remoteLine) continue;
            changes.push({
                lineNumber: i + 1,
                type: !remoteLine ? 'added' : !localLine ? 'removed' : 'modified',
                localLine,
                remoteLine,
                isWhitespaceOnly: localLine.trim() === remoteLine.trim()
            });
        }
        return changes;
    }

    /**
     * Convert diff changes into Problems-style list entries.
     */
    function mapChangesToProblems(changes) {
        const list = Array.isArray(changes) ? changes : [];
        return list.map((change) => {
            const type = String(change?.type || 'modified');
            const lineNumber = Number(change?.lineNumber || 0) || 0;
            const isWhitespaceOnly = !!change?.isWhitespaceOnly;
            const side = type === 'added' ? 'local' : type === 'removed' ? 'remote' : 'both';
            const severity = isWhitespaceOnly ? 'info' : 'warning';
            return {
                lineNumber,
                type,
                side,
                severity,
                isWhitespaceOnly,
                message: `${type.toUpperCase()}: Line ${lineNumber}`
            };
        });
    }

    const api = Object.freeze({
        parseFindOutput,
        calculateLineDiff,
        mapChangesToProblems
    });

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.extensions = window.AhmadIDEModules.extensions || {};
        window.AhmadIDEModules.extensions.compareWithReleaseUtils = api;
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = api;
    }
})();

