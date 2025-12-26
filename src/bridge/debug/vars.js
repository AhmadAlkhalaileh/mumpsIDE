function decodeMString(val) {
  if (typeof val !== 'string') return val;
  // ZSHOW wraps strings in quotes and doubles inner quotes; normalize to how the user wrote it.
  let out = val.replace(/""/g, '"');

  // If there are backslash escapes (e.g., \" from JSON), try to unescape them safely.
  if (out.includes('\\')) {
    try {
      out = JSON.parse(`"${out.replace(/\\/g, '\\\\').replace(/"/g, '\\"')}"`);
    } catch (_) {
      // Fallback: minimally replace \" -> "
      out = out.replace(/\\"/g, '"');
    }
  }
  return out;
}

function normalizeDebuggerVars(rawVars = {}) {
  const locals = {};
  Object.entries(rawVars || {}).forEach(([rawKey, value]) => {
    if (!rawKey) return;
    const key = `${rawKey}`.toUpperCase();
    const decodedVal = decodeMString(value);
    const arrMatch = key.match(/^([A-Z%][A-Z0-9]*)(\(.+\))$/);
    if (arrMatch) {
      const base = arrMatch[1];
      const sub = arrMatch[2];
      if (!locals[base] || typeof locals[base] !== 'object' || !locals[base]._isArray) {
        locals[base] = { _isArray: true, _elements: {} };
      }
      locals[base]._elements[sub] = decodedVal;
    } else {
      locals[key] = decodedVal;
    }
  });
  return locals;
}

module.exports = {
  decodeMString,
  normalizeDebuggerVars
};

