function shellQuote(str = '') {
  return `'${str.replace(/'/g, `'\\''`)}'`;
}

// envKey is the username suffix after the last dash (user-cc => cc).
// If parsing fails, returns `fallback` (default: empty string).
function deriveEnvKeyFromUsername(username, fallback = '') {
  const raw = (username || '').trim();
  if (!raw) return fallback;
  const lastDash = raw.lastIndexOf('-');
  if (lastDash === -1) return fallback;
  const key = raw.slice(lastDash + 1).trim();
  return key || fallback;
}

function normalizeRoutineName(name = '') {
  const trimmed = (name || '').trim();
  if (!trimmed) return '';
  return trimmed.replace(/\.m$/i, '').toUpperCase();
}

module.exports = {
  shellQuote,
  deriveEnvKeyFromUsername,
  normalizeRoutineName
};
