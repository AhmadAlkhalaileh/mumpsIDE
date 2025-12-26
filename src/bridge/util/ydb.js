function sanitizeRoutineName(name = '') {
  const trimmed = name.trim();
  // MUMPS routines can start with %, Ø, or other special chars, including _
  // Allow: %NAME, _NAME, ØNAME, or regular NAME (alphanumeric, up to 31 chars)
  if (!/^[A-Za-z%_ØŒÆÐ][A-Za-z0-9%_ØŒÆÐ]{0,30}$/i.test(trimmed)) return null;
  return trimmed.toUpperCase();
}

function cleanOutput(raw = '') {
  return (raw || '')
    .replace(/YDB>/g, '')
    .split('\n')
    .filter(line =>
      !line.includes('mupip:') &&
      !line.includes('.bashrc') &&
      !line.toLowerCase().includes('permission denied') &&
      !line.includes('%YDB-E-NOTEXTRINSIC') &&
      !line.includes('At M source location') &&
      line.trim().length > 0
    )
    .map(line => line.trimEnd())
    .join('\n')
    .trim();
}

module.exports = {
  sanitizeRoutineName,
  cleanOutput
};
