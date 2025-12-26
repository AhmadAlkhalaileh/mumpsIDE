function buildSourceMapFromCode(routineName, code = '') {
  const lines = (code || '').split('\n').map(l => l.replace(/\r/g, ''));
  let currentTag = '';
  let currentLabelText = '';
  const map = lines.map((raw, idx) => {
    const trimmed = raw.trim();
    const startsAtColumn1 = raw && !/^[\t ]/.test(raw);
    const tagMatch = startsAtColumn1 ? trimmed.match(/^([A-Za-z%][A-Za-z0-9]*)(\([^)]*\))?(.*)$/) : null;
    let isComment = trimmed.startsWith(';') || trimmed === '';

    if (tagMatch) {
      currentTag = tagMatch[1];
      currentLabelText = tagMatch[1] + (tagMatch[2] || '');
      // Check if there's code after the tag (not just a comment)
      const afterTag = (tagMatch[3] || '').trim();
      const hasCode = afterTag && !afterTag.startsWith(';');
      // Tag-only lines (no code) are non-executable
      if (!hasCode) {
        isComment = true;
      }
    }

    return {
      routine: routineName.toUpperCase(),
      line: idx + 1,
      isComment,
      tag: currentTag,
      labelText: currentLabelText || currentTag,
      isLabel: !!tagMatch && startsAtColumn1
    };
  });
  return { lines, map };
}

function nextExecutableLine(sourceMap, fromLine) {
  if (!sourceMap || !Array.isArray(sourceMap.map)) return fromLine;
  const total = sourceMap.map.length;
  let i = Math.max(0, fromLine - 1);
  while (i < total) {
    const entry = sourceMap.map[i];
    if (!entry.isComment) return entry.line;
    i += 1;
  }
  return fromLine;
}

function parseZPos(zpos) {
  if (!zpos || typeof zpos !== 'string') return {};
  const m = zpos.match(/^(?:([A-Za-z%][A-Za-z0-9]*))?(?:\+(\d+))?\^([A-Za-z%][A-Za-z0-9]+)/);
  if (!m) return {};
  return {
    tag: m[1] || '',
    offset: parseInt(m[2] || '0', 10),
    routine: m[3] ? m[3].toUpperCase() : ''
  };
}

function formatCallStackForClient(callStack = []) {
  return (callStack || []).map((frame) => {
    if (typeof frame === 'string') return frame;
    const routine = (frame?.routine || '').toUpperCase() || 'TMPDBG';
    const line = frame?.line || frame?.returnLine || 1;
    const tag = frame?.tag || frame?.returnTag;
    return tag ? `${routine}:${line} (${tag})` : `${routine}:${line}`;
  });
}

function payloadLineToUserLine(session, payloadLine) {
  if (!session || !payloadLine) return payloadLine;
  const map = Array.isArray(session.payloadToUser) ? session.payloadToUser : [];
  const idx = payloadLine - 1;
  if (idx >= 0 && idx < map.length && map[idx] !== undefined) return map[idx];
  const header = session.headerLines || 0;
  return payloadLine > header ? payloadLine - header : payloadLine;
}

module.exports = {
  buildSourceMapFromCode,
  nextExecutableLine,
  parseZPos,
  formatCallStackForClient,
  payloadLineToUserLine
};

