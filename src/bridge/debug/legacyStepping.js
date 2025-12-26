// --- Legacy stepping helpers (comment skipping) ---
function isSkippableDebugLine(line) {
  const trimmed = (line || '').replace(/^[\t ]+/, '');
  return trimmed === '' || trimmed.startsWith(';');
}

function advanceToNextExecutableLine(session) {
  if (!session || !Array.isArray(session.lines)) return;
  while (session.currentLine <= session.lines.length) {
    const line = session.lines[session.currentLine - 1] || '';
    if (!isSkippableDebugLine(line)) return;
    session.currentLine += 1;
  }
}

module.exports = {
  isSkippableDebugLine,
  advanceToNextExecutableLine
};
