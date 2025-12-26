module.exports = {
  createTagOffsetForLine(map, lines, dbgLog) {
    return (payloadLine) => {
      const idx = payloadLine - 1;
      if (!Number.isInteger(idx) || idx < 0 || idx >= lines.length) {
        dbgLog('[DEBUG] tagOffsetForLine: payloadLine out of bounds', { payloadLine, idx, maxLines: lines.length });
        return null;
      }

      // Find the nearest preceding label by walking backward
      // Look for map entries where isLabel is true
      let labelLine = -1;
      let labelTag = '';
      let labelFullText = '';

      for (let i = idx; i >= 0; i -= 1) {
        const entry = map[i];
        if (entry && entry.isLabel) {
          labelLine = entry.line; // 1-indexed
          labelFullText = (entry.labelText || entry.tag || '');
          labelTag = labelFullText.split('(')[0].toUpperCase(); // Tag without params
          dbgLog('[DEBUG] tagOffsetForLine: Found label', labelTag, '(full:', labelFullText, ') at line', labelLine);
          break;
        }
      }

      if (labelLine < 0 || !labelTag) {
        // No label found - use routine entry point (TMPDBG)
        // Fallback: offset from line 1
        const offset = payloadLine - 1; // 0-based offset from routine start
        dbgLog('[DEBUG] tagOffsetForLine: No label found, using TMPDBG with offset', offset);
        return {
          tag: 'TMPDBG',
          offset: Math.max(0, offset),
          tagLine: 1
        };
      }

      // Calculate offset from label line
      let offset = payloadLine - labelLine;
      const rawOffset = offset; // keep the raw offset for potential fallback attempts

      // CRITICAL FIX: If the label line itself is non-executable (tag-only line),
      // MUMPS counts offsets from the NEXT line, so we subtract 1
      const labelEntry = map[labelLine - 1];
      const labelIsNonExecutable = labelEntry && labelEntry.isComment;

      if (labelIsNonExecutable) {
        offset -= 1;
        dbgLog('[DEBUG] tagOffsetForLine: Label line is non-executable, adjusting offset:', {
          labelTag,
          labelLine,
          targetLine: payloadLine,
          rawOffset: payloadLine - labelLine,
          adjustedOffset: offset
        });
      } else {
        dbgLog('[DEBUG] tagOffsetForLine: Label', labelTag, 'at line', labelLine, ', target line', payloadLine, ', offset:', offset);
      }

      // VALIDATION: Ensure offset is not negative
      if (offset < 0) {
        dbgLog('[DEBUG] tagOffsetForLine: WARNING Negative offset!', { payloadLine, labelLine, offset });
        return null;
      }

      // VALIDATION: Ensure the breakpoint makes sense
      // If label is non-executable, the first line after it is at offset 0
      const expectedTargetLine = labelIsNonExecutable ? (labelLine + 1 + offset) : (labelLine + offset);
      if (expectedTargetLine !== payloadLine) {
        dbgLog('[DEBUG] tagOffsetForLine: WARNING Calculated offset mismatch!', {
          tag: labelTag,
          tagLine: labelLine,
          offset,
          expectedTargetLine,
          actualPayloadLine: payloadLine,
          labelIsNonExecutable
        });
      }

      if (expectedTargetLine > lines.length) {
        dbgLog('[DEBUG] tagOffsetForLine: WARNING Target exceeds bounds!', {
          tag: labelTag,
          tagLine: labelLine,
          offset,
          targetLine: expectedTargetLine,
          maxLines: lines.length
        });
        return null;
      }

      return {
        tag: labelTag,
        offset,
        tagLine: labelLine,
        rawOffset,
        labelIsNonExecutable
      };
    };
  }
};

