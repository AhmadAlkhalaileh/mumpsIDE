const sanitizeDebugCode = (src = '') => {
  // Heuristic: keep label definitions flush left; indent known M commands.
  const mCommands = new Set([
    'B', 'BREAK', 'C', 'CLOSE', 'D', 'DO', 'E', 'ELSE', 'F', 'FOR', 'G', 'GOTO',
    'H', 'HALT', 'HANG', 'I', 'IF', 'J', 'JOB', 'K', 'KILL', 'L', 'LOCK',
    'M', 'MERGE', 'N', 'NEW', 'O', 'OPEN', 'Q', 'QUIT', 'R', 'READ',
    'S', 'SET', 'TCOMMIT', 'TROLLBACK', 'TSTART', 'U', 'USE', 'V', 'VIEW',
    'W', 'WRITE', 'X', 'XECUTE', 'ZBREAK', 'ZCONTINUE', 'ZSTEP', 'ZWRITE'
  ]);

  return (src || '').split('\n').map((line) => {
    const raw = line.replace(/\r/g, '');
    const trimmed = raw.trim();
    if (trimmed === '') return '';

    // If it looks like a label but is indented, normalize to column 1 so GT.M sees it.
    const firstToken = trimmed.split(/\s+/)[0];
    const tokenName = firstToken.replace(/\(.*/, '').toUpperCase();
    const isLabelCandidate = /^[A-Za-z%][A-Za-z0-9]*/.test(firstToken) && !mCommands.has(tokenName);
    const hasLeadingSpace = /^[\t ]/.test(raw);
    if (isLabelCandidate && hasLeadingSpace) {
      return trimmed;
    }

    if (/^[\t ]/.test(raw)) return raw;
    if (/^;/.test(trimmed)) return raw;

    const isLabel = isLabelCandidate;
    if (isLabel) return raw;

    return `\t${trimmed}`;
  }).join('\n');
};

// Prevent fall-through into parameterized labels by inserting a QUIT ahead of them when needed.
const addGuardQuits = (src = '') => {
  const lines = (src || '').split('\n');
  const out = [];
  const transformedToUser = [];
  const userToTransformed = new Map();
  let lastExecIdx = -1;
  const stopTokens = ['QUIT', 'HALT', 'HANG', 'GOTO', 'G'];

  lines.forEach((raw, idx) => {
    const userLine = idx + 1;
    const trimmed = raw.trim();
    const isComment = trimmed === '' || trimmed.startsWith(';');
    const isParamLabel = !/^[\t ]/.test(raw) && /^([A-Za-z%][A-Za-z0-9]*)\(/.test(trimmed);

    if (isParamLabel && out.length > 0) {
      const prevExec = out[lastExecIdx] || '';
      const prevTrim = prevExec.trim().toUpperCase();
      const endsWithStop = stopTokens.some(tok => prevTrim.startsWith(tok));
      if (!endsWithStop) {
        out.push('\tQUIT');
        transformedToUser.push(userLine);
        lastExecIdx = out.length - 1;
      }
    }

    out.push(raw);
    transformedToUser.push(userLine);
    if (!isComment) lastExecIdx = out.length - 1;
    userToTransformed.set(userLine, out.length); // 1-based line in transformed user code
  });

  return { code: out.join('\n'), transformedToUser, userToTransformed };
};

module.exports = {
  sanitizeDebugCode,
  addGuardQuits
};

