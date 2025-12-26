const fs = require('fs');
const { normalizeRoutineName } = require('../config/paths');
const { connectionConfig } = require('../config/connectionConfig');

function defaultRoutinePath(routineName) {
  const cfg = connectionConfig.docker || {};
  const root = (cfg.routinesPath || cfg.rpcRoutinesPath || '').split(' ')[0] || '';
  const norm = normalizeRoutineName(routineName);
  if (!norm) return '';
  if (!root) return `${norm}.m`;
  return `${root}/${norm}.m`;
}

/**
 * Convert MUMPS position string to file/line coordinates
 * @param {string} positionString - MUMPS position format: TAG+OFFSET^ROUTINE or +OFFSET^ROUTINE
 * @returns {object} { routine, tag, offset, line }
 *   - routine: normalized routine name (uppercase, no .m extension)
 *   - tag: label/tag name (empty string if none)
 *   - offset: numeric offset from tag
 *   - line: **0-BASED** line number in the source file (matches mumps-debug-master)
 *
 * CRITICAL COORDINATE SYSTEM (MISMATCH #1 fix):
 * - This function returns **0-BASED** line numbers (line 0 = first line of file)
 * - MDEBUG server also uses **0-BASED** line numbers internally
 * - When sending breakpoints to server: use **1-BASED** (SETBP;file;1 for first line)
 * - When receiving positions from server: convert to **0-BASED** with this function
 * - Monaco editor uses **1-BASED** line numbers, so add 1 when displaying
 */
function convertMdebugPosition(positionString = '') {
  // positionString example: TAG+OFFSET^ROUTINE or +5^ROUTINE
  const parts = positionString.split('^');
  const left = parts[0] || '';
  const routine = normalizeRoutineName(parts[1] ? parts[1].split(' ', 1)[0] : '');
  let tag = '';
  let offset = 0;
  if (left.includes('+')) {
    tag = left.split('+')[0];
    offset = parseInt(left.split('+')[1] || '0', 10);
    if (!tag) offset = Math.max(0, offset - 1); // M adds 1 when no tag
  } else {
    tag = left;
  }

  const file = defaultRoutinePath(routine);
  let line = 0;  // 0-based line number
  try {
    const lines = fs.readFileSync(file, 'utf8').split('\n');
    if (tag) {
      const tagRe = new RegExp(`^${tag}([\\s(;:]|$)`);
      for (let i = 0; i < lines.length; i++) {
        if (tagRe.test(lines[i])) {
          line = i;  // Found tag at 0-based line i
          break;
        }
      }
    }
    line = line + offset;
    if (line < 0) line = 0;
    if (line >= lines.length) line = lines.length - 1;
  } catch (e) {
    // fall back to offset only
    line = offset;
  }

  // Return 0-based line number (matches mumps-debug-master behavior)
  return { routine, tag, offset, line };
}

module.exports = {
  defaultRoutinePath,
  convertMdebugPosition
};
