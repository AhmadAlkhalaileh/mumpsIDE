const path = require('path');
const { exec } = require('child_process');
const { shellQuote } = require('../config/paths');
const { connectionConfig } = require('../config/connectionConfig');
const { hasActiveSshSession } = require('../state/sessions');
const { wrapDockerCmd } = require('../util/process');

const DEFAULT_VISTA_PROFILE_PATH = '/var/worldvista/prod/common/vista-profile';

function stripInlineShellComment(line) {
  const s = String(line || '');
  let inSingle = false;
  let inDouble = false;
  for (let i = 0; i < s.length; i++) {
    const ch = s[i];
    if (ch === "'" && !inDouble) inSingle = !inSingle;
    else if (ch === '"' && !inSingle) inDouble = !inDouble;
    else if (ch === '#' && !inSingle && !inDouble) {
      return s.slice(0, i);
    }
  }
  return s;
}

function parseShellAssignedValue(raw) {
  let s = String(raw || '').trim();
  if (!s) return '';

  // Drop trailing command separators
  s = s.replace(/[;\s]+$/g, '').trim();
  if (!s) return '';

  const q = s[0];
  if (q === '"' || q === "'") {
    // Extract until matching quote (best-effort; does not fully emulate shell escaping).
    let out = '';
    for (let i = 1; i < s.length; i++) {
      const ch = s[i];
      if (ch === q) return out;
      if (q === '"' && ch === '\\' && i + 1 < s.length) {
        out += s[i + 1];
        i++;
        continue;
      }
      out += ch;
    }
    return out;
  }

  // Unquoted: stop at first whitespace or semicolon.
  const m = s.match(/^([^;\s]+)/);
  return m ? m[1] : s;
}

function parseVistaProfileVars(profileText) {
  const vars = Object.create(null);
  const lines = String(profileText || '').split(/\r?\n/);
  for (const rawLine of lines) {
    const noComment = stripInlineShellComment(rawLine);
    const line = String(noComment || '').trim();
    if (!line) continue;
    if (line.startsWith('#')) continue;

    // "export VAR=VALUE" or "VAR=VALUE"
    const m = line.match(/^(?:export\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.+)$/);
    if (m) {
      const key = m[1];
      const valueRaw = m[2];
      const value = parseShellAssignedValue(valueRaw);
      if (value) vars[key] = value;
      continue;
    }

    // csh/tcsh: "setenv VAR VALUE"
    const csh = line.match(/^setenv\s+([A-Za-z_][A-Za-z0-9_]*)\s+(.+)$/);
    if (csh) {
      const key = csh[1];
      const valueRaw = csh[2];
      const value = parseShellAssignedValue(valueRaw);
      if (value) vars[key] = value;
      continue;
    }
  }
  return vars;
}

function expandShellVarRefs(input, vars) {
  const s = String(input || '');
  if (!s) return '';

  let out = '';
  for (let i = 0; i < s.length; i++) {
    const ch = s[i];
    if (ch !== '$') {
      out += ch;
      continue;
    }

    const next = s[i + 1];
    if (next === '{') {
      const end = s.indexOf('}', i + 2);
      if (end === -1) {
        out += ch;
        continue;
      }

      const inner = s.slice(i + 2, end);
      const idxDefault = inner.indexOf(':-');
      const varName = (idxDefault === -1 ? inner : inner.slice(0, idxDefault)).trim();
      const defValue = idxDefault === -1 ? '' : inner.slice(idxDefault + 2);

      if (/^[A-Za-z_][A-Za-z0-9_]*$/.test(varName)) {
        const resolved = String(vars?.[varName] || '');
        out += resolved ? resolved : defValue;
        i = end;
        continue;
      }

      out += s.slice(i, end + 1);
      i = end;
      continue;
    }

    if (next && /[A-Za-z_]/.test(next)) {
      let j = i + 1;
      while (j < s.length && /[A-Za-z0-9_]/.test(s[j])) j++;
      const varName = s.slice(i + 1, j);
      const resolved = String(vars?.[varName] || '');
      if (resolved) out += resolved;
      else out += `$${varName}`;
      i = j - 1;
      continue;
    }

    out += ch;
  }

  return out;
}

function expandVistaProfileVars(vars) {
  const out = Object.assign(Object.create(null), vars || {});
  for (let pass = 0; pass < 10; pass++) {
    let changed = false;
    for (const key of Object.keys(out)) {
      const current = String(out[key] || '');
      if (!current) continue;
      const next = expandShellVarRefs(current, out);
      if (next !== current) {
        out[key] = next;
        changed = true;
      }
    }
    if (!changed) break;
  }
  return out;
}

function splitTopLevelBySpaces(s) {
  const str = String(s || '');
  const out = [];
  let buf = '';
  let parenDepth = 0;
  for (let i = 0; i < str.length; i++) {
    const ch = str[i];
    if (ch === '(') parenDepth++;
    else if (ch === ')' && parenDepth > 0) parenDepth--;

    if (/\s/.test(ch) && parenDepth === 0) {
      if (buf.trim()) out.push(buf.trim());
      buf = '';
      continue;
    }
    buf += ch;
  }
  if (buf.trim()) out.push(buf.trim());
  return out;
}

function extractRoutineDirsFromRoutinesVar(routinesVar) {
  const entries = splitTopLevelBySpaces(routinesVar);
  const dirs = [];
  for (const entry of entries) {
    if (!entry) continue;
    const rawDir = entry.includes('(') ? entry.slice(0, entry.indexOf('(')) : entry;
    const dir = String(rawDir || '').trim();
    if (!dir) continue;
    if (dir.includes('$')) continue;
    if (/\.(so|o|obj)$/i.test(dir)) continue;
    if (!dir.startsWith('/')) continue;
    if (!dirs.includes(dir)) dirs.push(dir);
  }
  return dirs;
}

function pickVistaDirs({ dirs, gldPath } = {}) {
  const list = Array.isArray(dirs) ? dirs : [];
  const localrPath = list.find((d) => /\/localr\/?$/.test(d)) || null;
  const routinesPath = list.find((d) => /\/routines\/?$/.test(d)) || null;
  const globalsDir = gldPath ? path.posix.dirname(gldPath) : null;
  const baseCandidates = [];
  if (localrPath) baseCandidates.push(localrPath.replace(/\/localr\/?$/, ''));
  if (routinesPath) baseCandidates.push(routinesPath.replace(/\/routines\/?$/, ''));
  if (gldPath && /\/globals\//.test(gldPath)) baseCandidates.push(gldPath.replace(/\/globals\/.*$/, ''));

  // Pick the longest common candidate (best-effort).
  let basePath = null;
  if (baseCandidates.length) {
    baseCandidates.sort((a, b) => b.length - a.length);
    basePath = baseCandidates[0] || null;
  }

  let envKey = null;
  if (basePath) {
    const m = String(basePath).match(/^\/var\/worldvista\/prod\/([^/]+)$/);
    if (m) envKey = m[1] || null;
  }

  return { gldPath: gldPath || null, globalsDir, localrPath, routinesPath, basePath, envKey };
}

function extractVistaPathsFromProfileText(profileText) {
  const varsRaw = parseVistaProfileVars(profileText);
  const vars = expandVistaProfileVars(varsRaw);
  const gldPath = vars.ydb_gbldir || vars.gtmgbldir || '';
  const routinesVar = vars.ydb_routines || vars.gtmroutines || '';
  const dirs = extractRoutineDirsFromRoutinesVar(routinesVar);
  return {
    ...pickVistaDirs({ dirs, gldPath: gldPath || null }),
    vars
  };
}

function execInCurrentConnection(command, { timeoutMs = 5000 } = {}) {
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;

  return new Promise((resolve) => {
    if (useDocker) {
      if (!cfg?.containerId) return resolve({ ok: false, error: 'No Docker container selected' });
      const escaped = String(command || '').replace(/'/g, `'\\''`);
      const fullCmd = wrapDockerCmd(`docker exec ${cfg.containerId} bash -lc '${escaped}'`);
      exec(fullCmd, { timeout: timeoutMs, maxBuffer: 5 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
        resolve({ ok: true, stdout: stdout || '', stderr: stderr || '' });
      });
      return;
    }

    if (!cfg?.host || !cfg?.username) return resolve({ ok: false, error: 'SSH not configured' });
    const sshPass = cfg.password ? `sshpass -p ${shellQuote(cfg.password)} ` : '';
    const escaped = String(command || '').replace(/"/g, '\\"');
    const fullCmd = `${sshPass}ssh -o StrictHostKeyChecking=no -p ${cfg.port || 22} ${cfg.username}@${cfg.host} "${escaped}"`;
    exec(fullCmd, { timeout: timeoutMs, maxBuffer: 5 * 1024 * 1024 }, (err, stdout, stderr) => {
      if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
      resolve({ ok: true, stdout: stdout || '', stderr: stderr || '' });
    });
  });
}

async function discoverVistaProfilePaths({ profilePath = DEFAULT_VISTA_PROFILE_PATH, timeoutMs = 5000 } = {}) {
  const { connectionConfig } = require('../config/connectionConfig');
  const { hasActiveSshSession } = require('../state/sessions');
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;

  const readCmd = `cat ${shellQuote(profilePath)} 2>/dev/null || true`;
  const res = await execInCurrentConnection(readCmd, { timeoutMs });
  let text = String(res?.stdout || '').trim();

  if (!text) {
    // Fallback: search for a vista-profile under /var/worldvista (best-effort).
    if (profilePath === DEFAULT_VISTA_PROFILE_PATH) {
      try {
        const findCmd = "find /var/worldvista -maxdepth 5 -type f -name 'vista-profile' 2>/dev/null | head -1";
        const findRes = await execInCurrentConnection(findCmd, { timeoutMs: Math.min(timeoutMs, 6000) });
        const found = String(findRes?.stdout || '').trim();
        if (found) {
          const readFound = `cat ${shellQuote(found)} 2>/dev/null || true`;
          const readRes = await execInCurrentConnection(readFound, { timeoutMs: Math.min(timeoutMs, 6000) });
          text = String(readRes?.stdout || '').trim();
          if (text) profilePath = found;
        }
      } catch (_) { }
    }
    if (!text) {
      return { ok: false, error: `vista-profile not found at ${profilePath}` };
    }
  }

  let extracted = extractVistaPathsFromProfileText(text);

  // If paths contain unresolved variables like $1, $2, or ///, we need to discover them
  const hasUnresolvedVars = (
    extracted.localrPath?.includes('$') || extracted.localrPath?.includes('///') ||
    extracted.routinesPath?.includes('$') || extracted.routinesPath?.includes('///') ||
    extracted.basePath?.includes('$') || extracted.basePath?.includes('///')
  );

  if (hasUnresolvedVars) {
    // Try to find actual localr/routines directories
    const findCmd = `sh -c 'find /var/worldvista -maxdepth 4 -type d \\( -name "localr" -o -name "routines" \\) 2>/dev/null | head -20'`;
    const findRes = await execInCurrentConnection(findCmd, { timeoutMs: 6000 });
    const foundPaths = String(findRes?.stdout || '').trim().split('\n').filter(Boolean);

    if (foundPaths.length > 0) {
      // Group by base path
      const pathGroups = {};
      foundPaths.forEach(path => {
        const parts = path.split('/');
        const dirName = parts[parts.length - 1]; // localr or routines
        const basePath = parts.slice(0, -1).join('/');

        if (!pathGroups[basePath]) {
          pathGroups[basePath] = {};
        }
        pathGroups[basePath][dirName] = path;
      });

      // Find first base path with both localr and routines
      for (const [basePath, dirs] of Object.entries(pathGroups)) {
        if (dirs.localr && dirs.routines) {
          console.log('[Vista Profile] Using discovered paths from filesystem:', dirs);
          extracted.localrPath = dirs.localr;
          extracted.routinesPath = dirs.routines;
          extracted.basePath = basePath;

          const envMatch = basePath.match(/\/var\/worldvista\/[^/]+\/([^/]+)$/);
          if (envMatch) {
            extracted.envKey = envMatch[1];
          }

          // Try to find gld file
          const gldPath = `${basePath}/globals/mumps.gld`;
          extracted.gldPath = gldPath;
          break;
        }
      }
    }
  }

  // Clean up any remaining /// patterns (collapsed path separators)
  if (extracted.localrPath) {
    extracted.localrPath = extracted.localrPath.replace(/\/\/+/g, '/');
  }
  if (extracted.routinesPath) {
    extracted.routinesPath = extracted.routinesPath.replace(/\/\/+/g, '/');
  }
  if (extracted.basePath) {
    extracted.basePath = extracted.basePath.replace(/\/\/+/g, '/');
  }
  if (extracted.gldPath) {
    extracted.gldPath = extracted.gldPath.replace(/\/\/+/g, '/');
  }

  // If we got a basePath but not localr/routines, probe the conventional locations.
  try {
    const base = String(extracted.basePath || '').replace(/\/+$/, '');
    if (base && (!extracted.localrPath || !extracted.routinesPath || !extracted.gldPath)) {
      const candidates = {
        localrPath: `${base}/localr`,
        routinesPath: `${base}/routines`,
        gldPath: `${base}/globals/mumps.gld`
      };

      const probeLines = [];
      if (!extracted.localrPath) probeLines.push(`if [ -d ${shellQuote(candidates.localrPath)} ]; then echo "__AIDE__localr=${candidates.localrPath}"; fi`);
      if (!extracted.routinesPath) probeLines.push(`if [ -d ${shellQuote(candidates.routinesPath)} ]; then echo "__AIDE__routines=${candidates.routinesPath}"; fi`);
      if (!extracted.gldPath) probeLines.push(`if [ -f ${shellQuote(candidates.gldPath)} ]; then echo "__AIDE__gld=${candidates.gldPath}"; fi`);

      if (probeLines.length) {
        const probeRes = await execInCurrentConnection(probeLines.join(' ; '), { timeoutMs: Math.min(timeoutMs, 4000) });
        const lines = String(probeRes?.stdout || '').split(/\r?\n/).map(l => l.trim()).filter(Boolean);
        for (const line of lines) {
          if (line.startsWith('__AIDE__localr=')) extracted.localrPath = line.slice('__AIDE__localr='.length) || extracted.localrPath;
          else if (line.startsWith('__AIDE__routines=')) extracted.routinesPath = line.slice('__AIDE__routines='.length) || extracted.routinesPath;
          else if (line.startsWith('__AIDE__gld=')) extracted.gldPath = line.slice('__AIDE__gld='.length) || extracted.gldPath;
        }

        const recomputed = pickVistaDirs({
          dirs: [extracted.localrPath, extracted.routinesPath].filter(Boolean),
          gldPath: extracted.gldPath || null
        });
        extracted = { ...extracted, ...recomputed };
      }
    }
  } catch (_) { }

  const hasAny = !!(extracted.gldPath || extracted.localrPath || extracted.routinesPath || extracted.basePath);

  if (!hasAny) {
    return { ok: false, error: 'vista-profile parsed but no paths found', ...extracted };
  }

  return { ok: true, profilePath, ...extracted };
}

function applyVistaProfilePathsToConfig(cfg, discovered, { override = false } = {}) {
  if (!cfg || !discovered || !discovered.ok) {
    return { changed: false };
  }

  let changed = false;
  const shouldSet = (current, next) => {
    if (!next) return false;
    if (override) return true;
    return !current;
  };

  if (shouldSet(cfg.gldPath, discovered.gldPath)) {
    cfg.gldPath = discovered.gldPath;
    changed = true;
  }
  if (shouldSet(cfg.basePath, discovered.basePath)) {
    cfg.basePath = discovered.basePath;
    changed = true;
  }

  // "routinesPath" in config is the primary routines directory (usually localr)
  if (shouldSet(cfg.routinesPath, discovered.localrPath)) {
    cfg.routinesPath = discovered.localrPath;
    changed = true;
  }

  const rpcDirs = [discovered.localrPath, discovered.routinesPath].filter(Boolean);
  const rpcRoutinesPath = rpcDirs.join(' ').trim();
  if (rpcRoutinesPath && shouldSet(cfg.rpcRoutinesPath, rpcRoutinesPath)) {
    cfg.rpcRoutinesPath = rpcRoutinesPath;
    changed = true;
  }

  // Keep envKey in sync when we can infer it.
  if (shouldSet(cfg.envKey, discovered.envKey)) {
    cfg.envKey = discovered.envKey;
    changed = true;
  }

  cfg.vistaProfile = {
    path: discovered.profilePath || DEFAULT_VISTA_PROFILE_PATH,
    discoveredAt: new Date().toISOString()
  };

  return { changed };
}

module.exports = {
  DEFAULT_VISTA_PROFILE_PATH,
  parseVistaProfileVars,
  extractVistaPathsFromProfileText,
  discoverVistaProfilePaths,
  applyVistaProfilePathsToConfig
};
