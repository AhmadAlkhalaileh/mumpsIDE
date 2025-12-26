const { exec } = require('child_process');
const fs = require('fs');

function resolveGitBinary() {
  try {
    const candidates = [];
    const snap = String(process.env.SNAP || '').trim();
    if (snap) {
      candidates.push(`${snap}/usr/bin/git`);
      candidates.push(`${snap}/bin/git`);
    }
    candidates.push('/usr/bin/git');
    candidates.push('/usr/local/bin/git');
    candidates.push('git');
    for (const c of candidates) {
      if (c === 'git') return 'git';
      try {
        if (fs.existsSync(c)) return c;
      } catch (_) { }
    }
  } catch (_) { }
  return 'git';
}

function rewriteGitCommand(cmd) {
  const raw = String(cmd || '');
  if (!/^\s*git(?=\s|$)/.test(raw)) return raw;
  const bin = resolveGitBinary();
  if (!bin || bin === 'git') return raw;
  const quoted = bin.includes(' ') ? `"${bin.replace(/"/g, '\\"')}"` : bin;
  return raw.replace(/^\s*git(?=\s|$)/, quoted);
}

function runLocalGitCommand(cmd) {
  return new Promise((resolve) => {
    const rewritten = rewriteGitCommand(cmd);
    const env = (() => {
      const next = { ...process.env };
      try {
        // Desktop-launched apps can get a truncated PATH; ensure common dirs exist.
        if (process.platform !== 'win32') {
          const snap = String(process.env.SNAP || '').trim();
          const snapPrefix = snap
            ? `${snap}/usr/local/sbin:${snap}/usr/local/bin:${snap}/usr/sbin:${snap}/usr/bin:${snap}/sbin:${snap}/bin`
            : '';
          const sysPrefix = '/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin';
          const prefix = snapPrefix ? `${snapPrefix}:${sysPrefix}` : sysPrefix;
          const cur = String(next.PATH || '');
          next.PATH = prefix + (cur ? ':' + cur : '');
        }
      } catch (_) { }
      return next;
    })();
    exec(rewritten, { timeout: 30000, maxBuffer: 10 * 1024 * 1024, env }, (err, stdout, stderr) => {
      if (err) {
        const stderrMsg = String(stderr || '').trim();
        const message = stderrMsg || String(err.message || 'Git command failed').trim();
        return resolve({ ok: false, error: message, stdout, stderr });
      }
      resolve({ ok: true, stdout, stderr });
    });
  });
}

module.exports = {
  resolveGitBinary,
  rewriteGitCommand,
  runLocalGitCommand
};
