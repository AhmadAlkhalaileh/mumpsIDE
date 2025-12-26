module.exports = {
  selectEntryTag({ startLine, headerLines, userToTransformed, map, lines, nextExecutableLine, dbgLog, hasStartWrapper, firstTag, hasParams }) {
    let entryTag = '';
    // CRITICAL: If caller provided a starting line, adjust for the header and any inserted guard lines
    // This is where we determine WHERE to start debugging (NOT defaulting to line 1)
    if (Number.isInteger(startLine) && startLine > 0) {
      dbgLog(`[DEBUG] User requested start at line ${startLine}`);
      dbgLog('[runtime] User requested start line', { startLine });
      const mappedStart = (userToTransformed.get(startLine) || startLine) + headerLines;
      const adjustedLine = mappedStart;
      const targetLine = nextExecutableLine({ map, lines }, adjustedLine);
      dbgLog(`[DEBUG] Mapped user line ${startLine} -> payload line ${targetLine}`);
      dbgLog('[runtime] Mapped start line', { userLine: startLine, mappedStart, targetLine });

      if (targetLine < 1 || targetLine > lines.length) {
        dbgLog(`[DEBUG] WARNING: Target line ${targetLine} is out of range, falling back to first executable line`);
        dbgLog('[runtime] Start line out of range, using first executable', { targetLine, linesCount: lines.length });
        // Don't use line 1; find first executable line instead
        const firstExec = nextExecutableLine({ map, lines }, headerLines + 1);
        entryTag = (map[firstExec - 1]?.tag || '').toUpperCase();
      } else {
        const entry = map[targetLine - 1];
        entryTag = (entry?.tag || '').toUpperCase();
      }
    } else if (hasStartWrapper) {
      // Always use routine entry when we have a wrapper (no START tag anymore)
      entryTag = '';
      dbgLog('[DEBUG] No start line provided, using routine entry (wrapper handles first tag call)');
      dbgLog('[runtime] Using wrapper for entry', { firstTag, hasParams });
    } else {
      // No tags found, start at routine entry
      entryTag = '';
      dbgLog(`[DEBUG] No tags found, starting at routine entry`);
      dbgLog('[runtime] No tags, starting at routine entry');
    }

    // If the first tag is parameterized but we ended up targeting it directly (e.g. because a start line was provided),
    // force routine entry so the wrapper handles the call with dummy arguments.
    if (hasParams && firstTag && entryTag && entryTag === firstTag.toUpperCase()) {
      dbgLog('[DEBUG] First tag is parameterized; forcing routine entry to use wrapper');
      dbgLog('[runtime] Forcing routine entry for parameterized label', { entryTag, firstTag });
      entryTag = '';
    }

    // Validate entryTag exists; if missing, fall back to the first label or routine entry
    if (entryTag) {
      const hasEntry = map.some((m) => m.isLabel && (m.tag || '').toUpperCase() === entryTag);
      if (!hasEntry) {
        const firstLabel = (map.find((m) => m.isLabel && m.tag) || {}).tag || '';
        dbgLog(`[DEBUG] Entry tag "${entryTag}" not found; falling back to`, firstLabel || '(routine entry)');
        dbgLog('[runtime] Entry tag missing, applying fallback', { requested: entryTag, fallback: firstLabel || '(routine)' });
        entryTag = (firstLabel || '').toUpperCase();
      }
    }
    // As a final guard, if we still don't have a valid label in the payload, clear the entry tag so we start at routine entry
    if (entryTag) {
      const stillMissing = !map.some((m) => m.isLabel && (m.tag || '').toUpperCase() === entryTag);
      if (stillMissing) {
        dbgLog(`[DEBUG] Entry tag "${entryTag}" still not found after fallback; starting at routine entry`);
        dbgLog('[runtime] Entry tag missing after fallback', { entryTag });
        entryTag = '';
      }
    }

    return entryTag;
  }
};

