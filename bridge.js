// MUMPS bridge using local runtime (YottaDB/GT.M) via Docker or SSH (mirrors web backend defaults)
const { spawn, exec } = require('child_process');
const fs = require('fs');
const path = require('path');
let SSHClient = undefined;
let sshLoadError = null;
function ensureSshClient() {
  if (SSHClient !== undefined) return SSHClient;
  try {
    SSHClient = require('ssh2').Client;
  } catch (e) {
    SSHClient = null;
    sshLoadError = e;
  }
  return SSHClient;
}

const DEFAULT_ENV_KEY = 'cc';
const DOCKER_DEFAULT_ENV_KEY = 'hakeem';

function shellQuote(str = '') {
  return `'${str.replace(/'/g, `'\\''`)}'`;
}

// envKey is the username suffix after the last dash (user-cc => cc); fallback keeps cc when parsing fails.
function deriveEnvKeyFromUsername(username, fallback = DEFAULT_ENV_KEY) {
  const raw = (username || '').trim();
  if (!raw) return fallback;
  const lastDash = raw.lastIndexOf('-');
  if (lastDash === -1) return fallback;
  const key = raw.slice(lastDash + 1).trim();
  return key || fallback;
}

function buildEnvPaths(envKey = DEFAULT_ENV_KEY) {
  const key = (envKey || DEFAULT_ENV_KEY).trim() || DEFAULT_ENV_KEY;
  const basePath = `/var/worldvista/prod/${key}`;
  return {
    envKey: key,
    gldPath: `${basePath}/globals/mumps.gld`,
    routinesPath: `${basePath}/localr`,
    rpcRoutinesPath: `${basePath}/localr ${basePath}/routines`,
    basePath
  };
}

// Default connection config (from web backend)
function buildSshPaths(envKey = DEFAULT_ENV_KEY) {
  return {
    ydbPath: '/opt/fis-gtm/YDB136',
    ...buildEnvPaths(envKey)
  };
}

function mergeSshConfig(cfg = {}) {
  const fallbackEnvKey = cfg.envKey || cfg.baseKey || cfg.namespace || connectionConfig.ssh?.envKey || DEFAULT_ENV_KEY;
  const envKey = deriveEnvKeyFromUsername(cfg.username, fallbackEnvKey);
  const paths = buildSshPaths(envKey);
  return {
    ...connectionConfig.ssh,
    ...paths,
    ...cfg,
    envKey
  };
}

function mergeDockerConfig(cfg = {}) {
  // Docker always uses 'hakeem' as envKey (not configurable)
  const envKey = DOCKER_DEFAULT_ENV_KEY;
  const paths = buildEnvPaths(envKey);
  return {
    ...connectionConfig.docker,
    ...paths,
    ...cfg,
    envKey
  };
}

const connectionConfig = {
  type: 'docker',
  docker: {
    containerId: '8c21cf79fb67',
    ydbPath: '/opt/fis-gtm/YDB136',
    ...buildEnvPaths(DOCKER_DEFAULT_ENV_KEY)
  },
  ssh: {
    host: '',
    port: 22,
    username: '',
    password: '',
    ...buildSshPaths(DEFAULT_ENV_KEY)
  }
};

const debugSessions = {};
const sshSessions = {};
// Default to the richer zstep engine unless explicitly disabled
const USE_ZSTEP_ENGINE = process.env.AHMAD_IDE_DEBUG_ENGINE !== 'legacy';
const sourceMapCache = {};

function run(cmd, args = [], opts = {}) {
  return new Promise((resolve) => {
    const child = spawn(cmd, args, { timeout: 8000, ...opts });
    let stdout = '';
    let stderr = '';
    child.stdout.on('data', d => stdout += d.toString());
    child.stderr.on('data', d => stderr += d.toString());
    child.on('error', err => resolve({ ok: false, error: err.message, stdout, stderr }));
    child.on('close', code => {
      if (code !== 0) return resolve({ ok: false, error: `exit ${code}`, stdout, stderr });
      resolve({ ok: true, stdout, stderr });
    });
  });
}

function tmpFile(dir, ext) {
  const name = `ahmad_ide_${Date.now()}_${Math.random().toString(16).slice(2)}.${ext}`;
  return path.join(dir || process.cwd(), name);
}

// Wrap docker commands. By default, call docker directly.
// If env AHMAD_IDE_USE_SG=1, fall back to sg docker -c "...".
function wrapDockerCmd(cmd) {
  if (process.env.AHMAD_IDE_USE_SG === '1') {
    return `sg docker -c "${cmd.replace(/"/g, '\\"')}"`;
  }
  return cmd;
}

function buildYdbEnv(cfg = {}, opts = {}) {
  const ydbPath = cfg.ydbPath || '';
  const gldPath = cfg.gldPath || '';
  const routines = (cfg.rpcRoutinesPath || cfg.routinesPath || '').trim();
  const tmpDebugDir = (opts.tmpDebugDir || '').trim();
  const extra = Array.isArray(opts.extraRoutines) ? opts.extraRoutines.filter(Boolean) : [];
  const routineParts = [];
  if (tmpDebugDir) routineParts.push(`${tmpDebugDir}(${tmpDebugDir})`);
  if (extra.length) extra.forEach(p => routineParts.push(`${p}(${p})`));
  if (routines) routineParts.push(`${routines}(${routines})`);
  routineParts.push(`${ydbPath}/libgtmutil.so ${ydbPath}`);
  const gtmroutines = routineParts.filter(Boolean).join(' ').trim();
  return [
    `export gtm_dist=${ydbPath}`,
    `export gtmgbldir=${gldPath}`,
    `export gtmroutines='${gtmroutines}'`,
    `export gtm_etrap=''`,
    `export gtm_ztrap=''`
  ].join(' && ');
}

function runHostCommand(cmd) {
  return new Promise((resolve) => {
    const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
    if (useDocker) {
      const cfg = connectionConfig.docker;
      const escaped = cmd.replace(/'/g, `'\\''`);
      const full = wrapDockerCmd(`docker exec ${cfg.containerId} bash -lc '${escaped}'`);
      exec(full, { timeout: 30000, maxBuffer: 10 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
        resolve({ ok: true, stdout, stderr });
      });
    } else {
      const cfg = connectionConfig.ssh;
      const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';
      const escaped = cmd.replace(/"/g, '\\"');
      const full = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${escaped}"`;
      exec(full, { timeout: 30000, maxBuffer: 10 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
        resolve({ ok: true, stdout, stderr });
      });
    }
  });
}

function runLocalGitCommand(cmd) {
  return new Promise((resolve) => {
    exec(cmd, { timeout: 30000, maxBuffer: 10 * 1024 * 1024 }, (err, stdout, stderr) => {
      if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
      resolve({ ok: true, stdout, stderr });
    });
  });
}

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

// ---------------------- Source map helpers (zstep) ----------------------

function buildSourceMapFromCode(routineName, code = '') {
  const lines = (code || '').split('\n').map(l => l.replace(/\r/g, ''));
  let currentTag = '';
  const map = lines.map((raw, idx) => {
    const trimmed = raw.trim();
    const startsAtColumn1 = raw && !/^[\t ]/.test(raw);
    const tagMatch = startsAtColumn1 ? trimmed.match(/^([A-Za-z%][A-Za-z0-9]*)/) : null;
    if (tagMatch) currentTag = tagMatch[1];
    const isComment = trimmed.startsWith(';') || trimmed === '';
    return {
      routine: routineName.toUpperCase(),
      line: idx + 1,
      isComment,
      tag: currentTag,
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

async function loadRoutineSourceMap(routine, inlineSourceMap = null) {
  if (!routine) return null;
  const key = routine.toUpperCase();
  if (sourceMapCache[key]) return sourceMapCache[key];
  if (inlineSourceMap) {
    sourceMapCache[key] = inlineSourceMap;
    return inlineSourceMap;
  }
  const routineDirs = getRoutineDirs();
  for (const dir of routineDirs) {
    const cmd = `cat ${dir}/${key}.m 2>/dev/null`;
    const res = await runHostCommand(cmd);
    if (res.ok && res.stdout) {
      const smap = buildSourceMapFromCode(key, res.stdout);
      sourceMapCache[key] = smap;
      return smap;
    }
  }
  return null;
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
  if (idx >= 0 && idx < map.length && map[idx]) return map[idx];
  const header = session.headerLines || 0;
  return payloadLine > header ? payloadLine - header : payloadLine;
}

// ---------------------- ZSTEP (external harness) ----------------------

function writeRemoteFile(remotePath, content) {
  const b64 = Buffer.from(content, 'utf8').toString('base64');
  const cmd = `mkdir -p $(dirname ${remotePath}) && printf '%s' ${shellQuote(b64)} | base64 -d > ${remotePath}`;
  return runHostCommand(cmd);
}

async function ensureHarness() {
  const harnessPath = path.join(__dirname, 'AHMDBG.m');
  const dest = '/tmp/ahmad_dbg/AHMDBG.m';
  let content = '';
  try {
    content = fs.readFileSync(harnessPath, 'utf8');
  } catch (e) {
    return { ok: false, error: 'Cannot read AHMDBG.m harness' };
  }
  return writeRemoteFile(dest, content);
}

function spawnZStepProcess(entryRoutine, entryTag = '') {
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const tagArg = entryTag ? ` ${entryTag}` : '';
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;
  const envExports = buildYdbEnv(cfg, { tmpDebugDir: '/tmp/ahmad_dbg' });
  const cdCmd = 'cd /tmp/ahmad_dbg';
  // Run the AHMDBGJSON entry point in the AHMDBG routine (tag^routine)
  const runCmd = `${envExports} && ${cdCmd} && ${cfg.ydbPath}/mumps -run AHMDBGJSON^AHMDBG ${entryRoutine}${tagArg}`;

  console.log('[DEBUG] spawnZStepProcess environment:');
  console.log('[DEBUG] envExports:', envExports);
  console.log('[DEBUG] runCmd:', runCmd);

  if (useDocker) {
    const cmd = wrapDockerCmd(
      `docker exec -i ${cfg.containerId} bash -c "${runCmd.replace(/"/g, '\\"')}"`
    );
    console.log('[DEBUG] Final spawn command:', cmd);
    return spawn('bash', ['-lc', cmd], { stdio: ['pipe', 'pipe', 'pipe'] });
  }
  const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';
  const cmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${runCmd.replace(/"/g, '\\"')}"`;
  console.log('[DEBUG] Final spawn command:', cmd);
  return spawn('bash', ['-lc', cmd], { stdio: ['pipe', 'pipe', 'pipe'] });
}

function resolvePending(session, evt) {
  while (session.pending && session.pending.length) {
    const resolver = session.pending.shift();
    resolver(evt);
  }
}

function waitForZStepEvent(session) {
  return new Promise((resolve) => {
    session.pending.push(resolve);
  });
}

async function applyZStepEvent(session, evt) {
  const depth = evt.depth || session.callStack.length;
  const currentDepth = session.callStack.length;
  const posInfo = parseZPos(evt.pos || '');
  const routine = (evt.routine || posInfo.routine || (session.callStack[currentDepth - 1] || {}).routine || '').toUpperCase();
  const tag = (evt.tag || posInfo.tag || '').toUpperCase();
  const offset = Number.isInteger(evt.offset) ? evt.offset : posInfo.offset;

  if (depth > currentDepth) {
    const caller = session.callStack[currentDepth - 1] || {};
    const retLine = nextExecutableLine(session.sourceMap || sourceMapCache[caller.routine], caller.line + 1);
    session.callStack.push({
      routine,
      line: evt.line || 1,
      tag,
      returnRoutine: caller.routine,
      returnLine: retLine,
      returnTag: caller.tag || ''
    });
  } else if (depth < currentDepth) {
    while (session.callStack.length > depth) session.callStack.pop();
  } else if (session.callStack.length && session.callStack[session.callStack.length - 1].routine !== routine) {
    const caller = session.callStack[currentDepth - 1] || {};
    const retLine = nextExecutableLine(session.sourceMap || sourceMapCache[caller.routine], caller.line + 1);
    session.callStack.push({
      routine,
      line: evt.line || 1,
      tag,
      returnRoutine: caller.routine,
      returnLine: retLine,
      returnTag: caller.tag || ''
    });
  }

  const top = session.callStack[session.callStack.length - 1] || {};
  top.routine = routine || top.routine;
  top.tag = tag || top.tag;

  let smap = sourceMapCache[top.routine];
  if (!smap) {
    smap = await loadRoutineSourceMap(top.routine, top.routine === 'TMPDBG' ? session.sourceMap : null);
    if (smap) sourceMapCache[top.routine] = smap;
  }

  let targetLine = evt.line || 0;
  if (smap) {
    // Always map to an executable line using source map to skip comment-only lines
    if (targetLine <= 0) {
      // Compute from tag/offset when line not provided
      if (tag || Number.isInteger(offset)) {
        let tagLine = 1;
        if (tag) {
          for (const entry of smap.map) {
            if (entry.tag && entry.tag.toUpperCase() === tag) {
              tagLine = entry.line;
              break;
            }
          }
        }
        targetLine = tagLine + (Number.isInteger(offset) ? offset : 0);
      }
    }
    targetLine = nextExecutableLine(smap, targetLine || 1);
  }
  if (!targetLine) targetLine = top.line || 1;

  top.line = targetLine;
  session.currentRoutine = top.routine;
  session.currentLine = targetLine;
  session.currentTag = top.tag || '';
}

function handleZStepStdout(session) {
  return (chunk) => {
    session.buffer += chunk.toString();
    let idx;
    while ((idx = session.buffer.indexOf('\n')) >= 0) {
      const line = session.buffer.slice(0, idx).trim();
      session.buffer = session.buffer.slice(idx + 1);
      if (!line) continue;
      try {
        const evt = JSON.parse(line);
        resolvePending(session, evt);
      } catch (e) {
        session.output.push(line);
      }
    }
  };
}

async function startZStepSession(code, breakpoints = [], startLine = null) {
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
      if (/^[\t ]/.test(raw)) return raw;
      if (/^;/.test(trimmed)) return raw;

      const firstToken = trimmed.split(/\s+/)[0];
      const tokenName = firstToken.replace(/\(.*/, '').toUpperCase();
      const isLabel = /^[A-Za-z%][A-Za-z0-9]*/.test(firstToken) && !mCommands.has(tokenName);
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

  const safeUserCode = sanitizeDebugCode(code);
  const { code: guardedUserCode, transformedToUser, userToTransformed } = addGuardQuits(safeUserCode);

  const id = `dbg_${Date.now()}_${Math.random().toString(16).slice(2)}`;
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();

  // Parse the first user tag to check if it has parameters
  const firstTagMatch = safeUserCode.match(/^([A-Za-z%][A-Za-z0-9]*)(\([^)]*\))?/);
  const firstTag = firstTagMatch ? firstTagMatch[1] : '';
  const hasParams = firstTagMatch && firstTagMatch[2];

  // Build the full code payload with TMPDBG tag, QUIT, and optionally a START tag
  let codePayload;
  let headerLines;

  if (hasParams && firstTag) {
    // If first tag has parameters, create a START tag that calls it with dummy values
    // Extract parameter count
    const paramList = firstTagMatch[2].slice(1, -1).split(',').map(p => p.trim()).filter(Boolean);
    const dummyArgs = paramList.map(() => '0').join(',');

    codePayload = `TMPDBG ; Debug temp routine\n QUIT\nSTART ; Entry point for debugging\n DO ${firstTag}(${dummyArgs})\n QUIT\n${guardedUserCode}`;
    headerLines = 5; // TMPDBG, QUIT, START, DO line, QUIT
  } else {
    // No parameters or no first tag, use simple structure
    codePayload = `TMPDBG ; Debug temp routine\n QUIT\n${guardedUserCode}`;
    headerLines = 2; // TMPDBG, QUIT
  }

  // Build source map from the full payload (not just user code) so line numbers match
  const { map, lines } = buildSourceMapFromCode('TMPDBG', codePayload);
  sourceMapCache['TMPDBG'] = { map, lines };

  // Map payload lines back to user lines (header lines map to 0)
  const payloadToUser = [];
  for (let i = 0; i < headerLines; i += 1) payloadToUser.push(0);
  transformedToUser.forEach((uLine) => payloadToUser.push(uLine));

  let entryTag = '';
  // If caller provided a starting line, adjust for the header and any inserted guard lines
  if (Number.isInteger(startLine) && startLine > 0) {
    const mappedStart = (userToTransformed.get(startLine) || startLine) + headerLines;
    const adjustedLine = mappedStart;
    const targetLine = nextExecutableLine({ map, lines }, adjustedLine);
    const entry = map[targetLine - 1];
    entryTag = (entry?.tag || '').toUpperCase();
  } else if (hasParams && firstTag) {
    // If no explicit start line and first tag has parameters, use START tag
    entryTag = 'START';
  } else if (firstTag) {
    // If first tag has no parameters, we can call it directly
    entryTag = firstTag.toUpperCase();
  } else {
    // No tags found, start at routine entry
    entryTag = '';
  }

  console.log('[DEBUG] TMPDBG structure created:');
  console.log('[DEBUG] - Header lines:', headerLines);
  console.log('[DEBUG] - Entry tag:', entryTag || '(none - routine entry)');
  console.log('[DEBUG] - First tag:', firstTag || '(none)');
  console.log('[DEBUG] - Has params:', hasParams ? 'yes' : 'no');
  if (hasParams) {
    console.log('[DEBUG] - Code preview (first 10 lines):');
    codePayload.split('\n').slice(0, 10).forEach((line, i) => {
      console.log(`[DEBUG]   ${i + 1}: ${line}`);
    });
  }

  console.log('[DEBUG] Ensuring AHMDBG.m harness...');
  const harnessRes = await ensureHarness();
  if (!harnessRes.ok) {
    console.log(`[DEBUG] Harness installation failed: ${harnessRes.error || harnessRes.stderr}`);
    return { ok: false, error: harnessRes.error || harnessRes.stderr || 'Failed to install harness' };
  }
  console.log('[DEBUG] AHMDBG.m harness installed successfully');

  // Compile AHMDBG.m to ensure it's available
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;
  const envExports = buildYdbEnv(cfg, { tmpDebugDir: '/tmp/ahmad_dbg' });

  let compileCmd;
  if (useDocker) {
    compileCmd = wrapDockerCmd(
      `docker exec ${cfg.containerId} bash -c "${envExports} && cd /tmp/ahmad_dbg && ${cfg.ydbPath}/mumps AHMDBG.m"`
    );
  } else {
    const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';
    compileCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${envExports} && cd /tmp/ahmad_dbg && ${cfg.ydbPath}/mumps AHMDBG.m"`;
  }

  console.log('[DEBUG] Compiling AHMDBG.m...');
  console.log('[DEBUG] Compile command:', compileCmd);

  const compileRes = await new Promise((resolve) => {
    exec(compileCmd, { timeout: 10000, maxBuffer: 5 * 1024 * 1024 }, (err, stdout, stderr) => {
      if (err) {
        console.log(`[DEBUG] Compilation error: ${err.message}`);
        console.log(`[DEBUG] stderr: ${stderr}`);
        console.log(`[DEBUG] stdout: ${stdout}`);
        return resolve({ ok: false, error: err.message, stdout, stderr });
      }
      resolve({ ok: true, stdout, stderr });
    });
  });

  if (!compileRes.ok) {
    console.log(`[DEBUG] Compilation failed: ${compileRes.error || compileRes.stderr}`);
    return { ok: false, error: `AHMDBG compilation failed: ${compileRes.error || compileRes.stderr}` };
  } else {
    console.log('[DEBUG] AHMDBG.m compiled successfully');
    if (compileRes.stdout) console.log('[DEBUG] Compile output:', compileRes.stdout);
    if (compileRes.stderr) console.log('[DEBUG] Compile stderr:', compileRes.stderr);
  }

  // Verify AHMDBG.o exists
  // Use runHostCommand so the docker/ssh wrapper is applied exactly once
  const verifyCmd = `test -f /tmp/ahmad_dbg/AHMDBG.o && echo OK || echo MISSING`;

  const verifyRes = await runHostCommand(verifyCmd);
  console.log('[DEBUG] AHMDBG.o verification:', verifyRes.ok ? verifyRes.stdout.trim() : verifyRes.error);

  if (!verifyRes.ok || verifyRes.stdout.trim() !== 'OK') {
    console.log('[ERROR] AHMDBG.o file not found after compilation!');

    // List directory contents for debugging
    const lsCmd = `ls -la /tmp/ahmad_dbg/`;
    const lsRes = await runHostCommand(lsCmd);
    console.log('[DEBUG] Directory contents:', lsRes.ok ? lsRes.stdout : lsRes.error);

    return { ok: false, error: 'AHMDBG.o file not found after compilation' };
  }

  // Test if MUMPS can actually access files in /tmp/ahmad_dbg
  console.log('[DEBUG] Testing MUMPS access to /tmp/ahmad_dbg...');

  // First, verify we can list the directory and see both .m and .o files
  const listCmd = `ls -lh /tmp/ahmad_dbg/AHMDBG.*`;
  const listRes = await runHostCommand(listCmd);
  console.log('[DEBUG] AHMDBG files:', listRes.ok ? listRes.stdout : listRes.error);

  // Check file permissions and ownership
  const statCmd = `stat -c '%a %U:%G %s %n' /tmp/ahmad_dbg/AHMDBG.* 2>&1 || ls -l /tmp/ahmad_dbg/AHMDBG.*`;
  const statRes = await runHostCommand(statCmd);
  console.log('[DEBUG] File permissions:', statRes.ok ? statRes.stdout : statRes.error);

  // Try to read the compiled object file to see if it's readable
  const readTestCmd = `test -r /tmp/ahmad_dbg/AHMDBG.o && echo 'READABLE' || echo 'NOT READABLE'`;
  const readRes = await runHostCommand(readTestCmd);
  console.log('[DEBUG] AHMDBG.o readable:', readRes.ok ? readRes.stdout.trim() : readRes.error);

  if (!readRes.ok || readRes.stdout.trim() !== 'READABLE') {
    console.log('[ERROR] AHMDBG.o is not readable!');
    return { ok: false, error: 'AHMDBG.o file exists but is not readable - check permissions' };
  }

  // Check what user we're running as vs file ownership
  const whoamiCmd = `whoami`;
  const whoamiRes = await runHostCommand(whoamiCmd);
  console.log('[DEBUG] Running as user:', whoamiRes.ok ? whoamiRes.stdout.trim() : whoamiRes.error);

  // Try to dump the first few bytes of AHMDBG.o to verify it's a valid object file
  const hexdumpCmd = `hexdump -C /tmp/ahmad_dbg/AHMDBG.o | head -3`;
  const hexRes = await runHostCommand(hexdumpCmd);
  console.log('[DEBUG] AHMDBG.o header:', hexRes.ok ? hexRes.stdout.split('\n')[0] : hexRes.error);

  const destDir = '/tmp/ahmad_dbg';
  const destFile = `${destDir}/TMPDBG.m`;

  // Debug: Log the exact payload being written
  console.log('[DEBUG] TMPDBG.m payload:');
  console.log('--- START ---');
  console.log(codePayload);
  console.log('--- END ---');

  const writeRes = await writeRemoteFile(destFile, codePayload);
  if (!writeRes.ok) return { ok: false, error: writeRes.error || writeRes.stderr || 'Failed to write debug code' };

  // Compile TMPDBG.m to ensure it can be loaded
  console.log('[DEBUG] Compiling TMPDBG.m...');
  let compileTmpCmd;
  if (useDocker) {
    compileTmpCmd = wrapDockerCmd(
      `docker exec ${cfg.containerId} bash -c "${envExports} && cd /tmp/ahmad_dbg && ${cfg.ydbPath}/mumps TMPDBG.m"`
    );
  } else {
    const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';
    compileTmpCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${envExports} && cd /tmp/ahmad_dbg && ${cfg.ydbPath}/mumps TMPDBG.m"`;
  }

  const compileTmpRes = await new Promise((resolve) => {
    exec(compileTmpCmd, { timeout: 10000, maxBuffer: 5 * 1024 * 1024 }, (err, stdout, stderr) => {
      if (err) {
        console.log(`[DEBUG] TMPDBG compilation error: ${err.message}`);
        console.log(`[DEBUG] stderr: ${stderr}`);
        console.log(`[DEBUG] stdout: ${stdout}`);
        return resolve({ ok: false, error: err.message, stdout, stderr });
      }
      resolve({ ok: true, stdout, stderr });
    });
  });

  if (!compileTmpRes.ok) {
    console.log(`[DEBUG] TMPDBG compilation failed: ${compileTmpRes.error}`);
    return { ok: false, error: `TMPDBG compilation failed: ${compileTmpRes.stderr || compileTmpRes.error}` };
  }
  console.log('[DEBUG] TMPDBG.m compiled successfully');

  const proc = spawnZStepProcess('TMPDBG', entryTag);
  const session = {
    engine: 'zstep',
    proc,
    buffer: '',
    pending: [],
    callStack: [{ routine: 'TMPDBG', line: 1, tag: '', returnRoutine: null, returnLine: null, returnTag: null }],
    currentRoutine: 'TMPDBG',
    currentLine: 1,
    currentTag: '',
    sourceMap: { map, lines },
    breakpoints: breakpoints || [],
    output: [],
    procExited: false,
    headerLines: headerLines,  // Track header lines for this session (2 or 5 depending on whether first tag has params)
    payloadToUser,
    userToTransformed
  };

  proc.stdout.on('data', handleZStepStdout(session));
  proc.stderr.on('data', (chunk) => {
    const errMsg = chunk.toString();
    session.output.push(errMsg);
    console.log('[DEBUG] ZSTEP stderr:', errMsg);
  });
  proc.stdin.on('error', (err) => {
    console.log('[DEBUG] ZSTEP stdin error:', err.message);
  });
  proc.on('error', (err) => {
    console.log('[DEBUG] ZSTEP process error:', err.message);
  });
  proc.on('exit', (code, signal) => {
    console.log('[DEBUG] ZSTEP process exited with code:', code, 'signal:', signal);
    if (session.output && session.output.length > 0) {
      console.log('[DEBUG] Full ZSTEP error output:');
      console.log(session.output.join(''));
    }
    session.procExited = true;
    resolvePending(session, { event: 'exit' });
  });

  // Check if stdin is writable
  if (!proc.stdin.writable) {
    console.log('[ERROR] ZSTEP stdin is not writable!');
    proc.kill();
    return { ok: false, error: 'Failed to create writable stdin for debug process' };
  }
  console.log('[DEBUG] ZSTEP process spawned, stdin is writable');

  debugSessions[id] = session;

  // Send breakpoints to the runtime
  console.log('[DEBUG] Setting', breakpoints?.length || 0, 'breakpoints...');
  (breakpoints || []).forEach((ln) => {
    const n = parseInt(ln, 10);
    if (!Number.isInteger(n) || n <= 0) return;
    const mappedUserLine = (userToTransformed.get(n) || n) + headerLines;
    const adjustedLine = nextExecutableLine({ map, lines }, mappedUserLine);
    const bpCmd = `SETBP;TMPDBG;${adjustedLine}\n`;
    console.log('[DEBUG] Writing breakpoint command:', bpCmd.trim());
    session.proc.stdin.write(bpCmd);
  });

  // Store breakpoints for later checking during stepping
  // Note: ZBREAK has issues with TAG+offset format, so we'll check breakpoints manually during ZSTEP
  const bpLines = new Set();
  console.log('[DEBUG] Storing', breakpoints?.length || 0, 'breakpoints for manual checking...');
  (breakpoints || []).forEach((ln) => {
    const n = parseInt(ln, 10);
    if (!Number.isInteger(n) || n <= 0) return;
    const mappedUserLine = (userToTransformed.get(n) || n) + headerLines;
    const adjustedLine = nextExecutableLine({ map, lines }, mappedUserLine);
    bpLines.add(adjustedLine);
    console.log('[DEBUG] Breakpoint at user line', n, '-> file line', adjustedLine);
  });
  session.manualBreakpoints = bpLines;

  // Track user-set breakpoints for TMPDBG so we don't clear them when using temp BPs
  session.zstepUserBps = new Set((breakpoints || []).map(n => `TMPDBG#${parseInt(n, 10)}`));
  session.autoBps = new Set();

  console.log('[DEBUG] Waiting for initial stopped event...');

  // Wait for initial stopped event (runtime is paused waiting for command)
  const firstEvt = await waitForZStepEvent(session);
  console.log('[DEBUG] Received first event:', JSON.stringify(firstEvt));

  if (firstEvt) {
    if (firstEvt.event === 'stopped') {
      console.log('[DEBUG] Processing stopped event...');
      await applyZStepEvent(session, firstEvt);
      console.log('[DEBUG] Stopped event processed successfully');

      // DISABLED: Auto-continue to breakpoint can fail if code has runtime errors
      // Instead, user will manually step through the code
      // TODO: Re-enable this once we have better error handling
      /*
      if (session.manualBreakpoints && session.manualBreakpoints.size > 0) {
        const atBreakpoint = session.manualBreakpoints.has(session.currentLine);
        if (!atBreakpoint) {
          console.log('[DEBUG] Not at breakpoint (line', session.currentLine, '), continuing...');
          while (!session.procExited && !session.manualBreakpoints.has(session.currentLine)) {
            session.proc.stdin.write('INTO\n');
            const evt = await waitForZStepEvent(session);
            if (evt.event === 'stopped') {
              await applyZStepEvent(session, evt);
            } else if (evt.event === 'exit') {
              session.procExited = true;
              delete debugSessions[id];
              return { ok: false, error: 'Program finished without hitting breakpoint', output: (session.output || []).join('\n') };
            } else if (evt.event === 'error') {
              session.procExited = true;
              delete debugSessions[id];
              return { ok: false, error: evt.message || 'Runtime error', output: (session.output || []).join('\n') };
            }
          }
          console.log('[DEBUG] Hit breakpoint at line', session.currentLine);
        } else {
          console.log('[DEBUG] Already at breakpoint, line', session.currentLine);
        }
      }
      */
      console.log('[DEBUG] Debugger started and paused at line', session.currentLine);
    } else if (firstEvt.event === 'error') {
      console.log('[DEBUG] Error event received:', firstEvt.message);
      session.procExited = true;
      delete debugSessions[id];
      return { ok: false, error: firstEvt.message || 'Debug process error', output: (session.output || []).join('\n') };
    } else if (firstEvt.event === 'exit') {
      console.log('[DEBUG] Exit event received before debugger paused');
      session.procExited = true;
      delete debugSessions[id];
      return { ok: false, error: 'Program finished before debugger paused', output: (session.output || []).join('\n') };
    }
  }

  // Map TMPDBG payload line back to user's editor coordinates
  const userLine = payloadLineToUserLine(session, session.currentLine);

  return {
    ok: true,
    sessionId: id,
    currentLine: userLine,
    currentRoutine: session.currentRoutine,
    callStack: session.callStack,
    stack: formatCallStackForClient(session.callStack),
    engine: 'zstep'
  };
}

async function sendZStepCommand(sessionId, command) {
  const session = debugSessions[sessionId];
  if (!session || session.engine !== 'zstep') return { ok: false, error: 'Session not found' };
  if (session.procExited) {
    return { ok: false, error: 'Program finished', output: (session.output || []).join('\n') };
  }
  console.log('[DEBUG] Sending step command:', command);
  session.proc.stdin.write(`${command}\n`);
  console.log('[DEBUG] Waiting for response to:', command);
  const evt = await waitForZStepEvent(session);
  console.log('[DEBUG] Received event for', command, ':', JSON.stringify(evt));
  if (evt.event === 'error') {
    return { ok: false, error: evt.message || 'Runtime error' };
  }
  if (evt.event === 'exit') {
    session.procExited = true;
    return { ok: false, error: 'Program finished', output: (session.output || []).join('\n') };
  }
  if (evt.event === 'stopped') {
    await applyZStepEvent(session, evt);
  }

  // Map TMPDBG payload line back to user's editor coordinates
  const userLine = payloadLineToUserLine(session, session.currentLine);

  return {
    ok: true,
    currentLine: userLine,
    currentRoutine: session.currentRoutine,
    currentTag: session.currentTag,
    callStack: session.callStack,
    stack: formatCallStackForClient(session.callStack),
    output: (session.output || []).join('\n')
  };
}

function readGitConfig(projectPath) {
  try {
    const gitConfigPath = path.join(projectPath, '.git', 'config');
    if (!fs.existsSync(gitConfigPath)) {
      return { ok: false, error: 'No .git/config found' };
    }

    const configContent = fs.readFileSync(gitConfigPath, 'utf8');
    const config = {};

    // Parse git config format
    const lines = configContent.split('\n');
    let currentSection = null;

    for (const line of lines) {
      const sectionMatch = line.match(/^\[(.+)\]$/);
      if (sectionMatch) {
        currentSection = sectionMatch[1];
        if (!config[currentSection]) config[currentSection] = {};
        continue;
      }

      const keyValueMatch = line.match(/^\s*(\w+)\s*=\s*(.*)$/);
      if (keyValueMatch && currentSection) {
        const [, key, value] = keyValueMatch;
        config[currentSection][key] = value.trim();
      }
    }

    // Extract useful info
    const result = {
      ok: true,
      user: {
        name: config.user?.name || '',
        email: config.user?.email || ''
      },
      remote: {}
    };

    // Get remote URLs
    Object.keys(config).forEach(section => {
      if (section.startsWith('remote ')) {
        const remoteName = section.replace('remote ', '').replace(/"/g, '');
        result.remote[remoteName] = config[section].url || '';
      }
    });

    return result;
  } catch (err) {
    return { ok: false, error: err.message };
  }
}

function sanitizeRoutineName(name = '') {
  const trimmed = name.trim();
  // MUMPS routines can start with %, Ø, or other special chars
  // Allow: %NAME, ØNAME, or regular NAME (alphanumeric, up to 31 chars)
  if (!/^[A-Za-z%ØŒÆÐ][A-Za-z0-9%ØŒÆÐ]{0,30}$/i.test(trimmed)) return null;
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
      line.trim().length > 0
    )
    .map(line => line.trimEnd())
    .join('\n')
    .trim();
}

function hasActiveSshSession() {
  return Object.keys(sshSessions).length > 0;
}

function getRoutineDirs() {
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;
  const dirs = [];
  const base = (cfg.routinesPath || '').trim();
  if (base) dirs.push(base);
  const rpc = (cfg.rpcRoutinesPath || '').trim();
  if (rpc) {
    rpc.split(/\s+/).filter(Boolean).forEach(p => {
      if (!dirs.includes(p)) dirs.push(p);
    });
  }
  return dirs;
}

function fetchRoutineDirectoriesToLocal(targetRoot) {
  const routineDirs = getRoutineDirs();
  if (!routineDirs.length) return Promise.resolve({ ok: false, error: 'No routines path configured' });

  fs.mkdirSync(targetRoot, { recursive: true });
  const target = targetRoot.replace(/'/g, `'\\''`);
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;
  const basePath = (cfg.basePath || '').trim();
  const allInBase = basePath && routineDirs.every(d => d.startsWith(`${basePath}/`));

  // Preserve folder structure: fetch as "localr routines" to maintain directory hierarchy
  const relPaths = allInBase
    ? routineDirs.map(d => d.slice(basePath.length + 1)).join(' ')
    : '';

  // Build tar command that preserves directory structure
  // This will create targetRoot/localr/*.m and targetRoot/routines/*.m
  const tarCmdInner = allInBase
    ? `cd ${shellQuote(basePath)} && tar -cf - ${relPaths}`
    : `tar -cf - ${routineDirs.map(d => `"${d.replace(/"/g, '\\"')}"`).join(' ')}`;

  return new Promise((resolve) => {
    if (useDocker) {
      const dockerCmd = `docker exec ${cfg.containerId} bash -lc "${tarCmdInner.replace(/"/g, '\\"')}"`;
      const full = `${wrapDockerCmd(dockerCmd)} | tar -xf - -C '${target}'`;
      exec(full, { timeout: 60000, maxBuffer: 20 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
        resolve({ ok: true });
      });
    } else {
      const sshPass = cfg.password ? `sshpass -p '${(cfg.password || '').replace(/'/g, `'\\''`)}' ` : '';
      const sshCmd = `${sshPass}ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${tarCmdInner.replace(/"/g, '\\"')}" | tar -xf - -C '${target}'`;
      exec(sshCmd, { timeout: 60000, maxBuffer: 20 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
        resolve({ ok: true });
      });
    }
  });
}

// Execute code directly in MUMPS without wrapping in a routine (for debugging)
async function executeYDBDirect(command) {
  const cmdFile = '/tmp/ahmad_debug_cmd.txt';

  // Commands are executed line-by-line in direct mode
  // No routine wrapper, so variables persist in the same session
  const commandsB64 = Buffer.from(command, 'utf8').toString('base64');

  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  if (useDocker) {
    const cfg = connectionConfig.docker;
    const writeCmd = wrapDockerCmd(
      `docker exec ${cfg.containerId} bash -lc "echo ${commandsB64} | base64 -d > ${cmdFile}"`
    );
    const runCmd = wrapDockerCmd(
      `docker exec ${cfg.containerId} bash -lc "export gtm_dist=${cfg.ydbPath} && export gtmgbldir=${cfg.gldPath} && export gtmroutines='${(cfg.rpcRoutinesPath || cfg.routinesPath)}(${(cfg.rpcRoutinesPath || cfg.routinesPath)}) ${cfg.ydbPath}/libgtmutil.so ${cfg.ydbPath}' && export gtm_etrap='' && export gtm_ztrap='' && ${cfg.ydbPath}/mumps -direct < ${cmdFile} 2>&1; rm -f ${cmdFile}"`
    );

    return new Promise((resolve) => {
      exec(`${writeCmd} && ${runCmd}`, { timeout: 30000, maxBuffer: 10 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
        resolve({ ok: true, stdout: cleanOutput(stdout || stderr) });
      });
    });
  } else {
    const cfg = connectionConfig.ssh;
    const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';
    const writeCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "echo ${commandsB64} | base64 -d > ${cmdFile}"`;
    const runCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "export gtm_dist=${cfg.ydbPath} && export gtmgbldir=${cfg.gldPath} && export gtmroutines='${(cfg.rpcRoutinesPath || cfg.routinesPath)}(${(cfg.rpcRoutinesPath || cfg.routinesPath)}) ${cfg.ydbPath}/libgtmutil.so ${cfg.ydbPath}' && export gtm_etrap='' && export gtm_ztrap='' && ${cfg.ydbPath}/mumps -direct < ${cmdFile} 2>&1; rm -f ${cmdFile}"`;

    return new Promise((resolve) => {
      exec(`${writeCmd} && ${runCmd}`, { timeout: 30000, maxBuffer: 10 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
        resolve({ ok: true, stdout: cleanOutput(stdout || stderr) });
      });
    });
  }
}

async function executeYDB(command) {
  const routineName = `TMP${Date.now().toString(16)}`;
  const routineDirs = getRoutineDirs();
  const routinesPath = routineDirs[0];
  if (!routinesPath) return { ok: false, error: 'No routines path configured' };
  const routineFile = `${routinesPath}/${routineName}.m`;

  const lines = (command || '').split('\n').map(l => l.replace(/\r/g, ''));

  // Detect whether user provided their own labels (full routine) vs a bare snippet
  const commandTokens = new Set([
    'B', 'BREAK', 'C', 'CLOSE', 'D', 'DO', 'E', 'ELSE', 'F', 'FOR', 'G', 'GOTO',
    'H', 'HALT', 'HANG', 'I', 'IF', 'J', 'JOB', 'K', 'KILL', 'L', 'LOCK', 'M', 'MERGE',
    'N', 'NEW', 'O', 'OPEN', 'Q', 'QUIT', 'R', 'READ', 'S', 'SET', 'T', 'THEN', 'U', 'USE',
    'V', 'VIEW', 'W', 'WRITE', 'X', 'XECUTE', 'ZBREAK', 'ZB', 'ZGOTO', 'ZHALT', 'ZKILL', 'ZWRITE', 'ZW'
  ]);
  const hasUserLabels = lines.some(raw => {
    const trimmed = raw.replace(/^\s+/, '');
    if (!trimmed || trimmed.startsWith(';')) return false;
    if (/^[\t ]/.test(raw)) return false; // commands are usually indented
    const token = trimmed.split(/[\s;(]/)[0] || '';
    return token && !commandTokens.has(token.toUpperCase());
  });

  let routineSource = `${routineName} ; temp routine\n`;
  let entryCall = `D MAIN^${routineName}`;

  if (hasUserLabels) {
    // Preserve the user's labels and indentation; just prepend our temp routine name
    routineSource += lines.join('\n');
    entryCall = `D ^${routineName}`;
  } else {
    // Treat as a bare snippet: wrap in MAIN and ensure commands are indented
    routineSource += 'MAIN ; entry\n';
    lines.forEach(l => {
      if (!l.trim()) {
        routineSource += '\t;\n';
        return;
      }
      const trimmed = l.trimStart();
      if (trimmed.startsWith(';')) {
        routineSource += `\t${trimmed}\n`;
        return;
      }
      // Ensure at least one leading tab/space so commands are valid under MAIN
      routineSource += /^\s/.test(l) ? `${l}\n` : `\t${trimmed}\n`;
    });
  }

  // Add a safe QUIT to guarantee clean exit
  const contentLines = routineSource.split('\n').filter(line => line.trim() && !/^\s*;/.test(line.trim()));
  const lastContent = (contentLines[contentLines.length - 1] || '').trim();
  if (!/^Q(UIT)?(\s|;|$)/i.test(lastContent)) {
    if (!routineSource.endsWith('\n')) routineSource += '\n';
    routineSource += '\tQUIT\n';
  }

  const codeB64 = Buffer.from(routineSource, 'utf8').toString('base64');
  const cmdFile = '/tmp/ahmad_cmd.txt';
  const runCmdB64 = Buffer.from(`${entryCall}\n`, 'utf8').toString('base64');

  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  if (useDocker) {
    const cfg = connectionConfig.docker;
    const writeCmd = wrapDockerCmd(
      `docker exec ${cfg.containerId} bash -lc "echo ${codeB64} | base64 -d > ${routineFile}"`
    );
    const writeRun = wrapDockerCmd(
      `docker exec ${cfg.containerId} bash -lc "echo ${runCmdB64} | base64 -d > ${cmdFile}"`
    );
    const runCmd = wrapDockerCmd(
      `docker exec ${cfg.containerId} bash -lc "export gtm_dist=${cfg.ydbPath} && export gtmgbldir=${cfg.gldPath} && export gtmroutines='${(cfg.rpcRoutinesPath || cfg.routinesPath)}(${(cfg.rpcRoutinesPath || cfg.routinesPath)}) ${cfg.ydbPath}/libgtmutil.so ${cfg.ydbPath}' && export gtm_etrap='' && export gtm_ztrap='' && ${cfg.ydbPath}/mumps -direct < ${cmdFile} 2>&1; rm -f ${routineFile} ${cmdFile}"`
    );

    return new Promise((resolve) => {
      exec(`${writeCmd} && ${writeRun} && ${runCmd}`, { timeout: 30000, maxBuffer: 10 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
        resolve({ ok: true, stdout: cleanOutput(stdout || stderr) });
      });
    });
  } else {
    const cfg = connectionConfig.ssh;
    const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';
    const writeCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "echo ${codeB64} | base64 -d > ${routineFile}"`;
    const writeRun = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "echo ${runCmdB64} | base64 -d > ${cmdFile}"`;
    const runCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "export gtm_dist=${cfg.ydbPath} && export gtmgbldir=${cfg.gldPath} && export gtmroutines='${(cfg.rpcRoutinesPath || cfg.routinesPath)}(${(cfg.rpcRoutinesPath || cfg.routinesPath)}) ${cfg.ydbPath}/libgtmutil.so ${cfg.ydbPath}' && export gtm_etrap='' && export gtm_ztrap='' && ${cfg.ydbPath}/mumps -direct < ${cmdFile} 2>&1; rm -f ${routineFile} ${cmdFile}"`;

    return new Promise((resolve) => {
      exec(`${writeCmd} && ${writeRun} && ${runCmd}`, { timeout: 30000, maxBuffer: 10 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
        resolve({ ok: true, stdout: cleanOutput(stdout || stderr) });
      });
    });
  }
}

module.exports = {
  async createProject(config) {
    const { projectPath, projectName, fetchRoutines } = config;
    if (!projectPath || !projectName) {
      return { ok: false, error: 'Project path and name are required' };
    }

    const fullPath = projectPath;
    const routinesPath = path.join(fullPath, 'routines');

    try {
      if (fs.existsSync(fullPath) && !fs.statSync(fullPath).isDirectory()) {
        return { ok: false, error: 'Target path exists and is not a directory' };
      }
      fs.mkdirSync(fullPath, { recursive: true });

      fs.mkdirSync(routinesPath, { recursive: true });

      // Fetch routines from Docker/SSH if requested
      let fetchedCount = 0;
      if (fetchRoutines) {
        const pulled = await fetchRoutineDirectoriesToLocal(routinesPath);
        if (pulled.ok) {
          fetchedCount = 1;
        } else {
          return { ok: false, error: pulled.error || 'Failed to fetch routines' };
        }
      }

      // Read folder structure: localr/ and routines/ subdirectories
      const structure = { localr: [], routines: [] };
      if (fs.existsSync(routinesPath)) {
        const localrPath = path.join(routinesPath, 'localr');
        const routinesSubPath = path.join(routinesPath, 'routines');

        if (fs.existsSync(localrPath)) {
          structure.localr = fs.readdirSync(localrPath).filter(f => f.endsWith('.m'));
        }
        if (fs.existsSync(routinesSubPath)) {
          structure.routines = fs.readdirSync(routinesSubPath).filter(f => f.endsWith('.m'));
        }
      }

      return {
        ok: true,
        projectPath: fullPath,
        routinesPath,
        message: fetchedCount > 0
          ? 'Project created with routines fetched from remote'
          : 'Project created successfully',
        structure
      };
    } catch (err) {
      return { ok: false, error: err.message };
    }
  },

  async openProject(projectPath) {
    try {
      if (!fs.existsSync(projectPath)) {
        return { ok: false, error: 'Project path does not exist' };
      }

      const routinesPath = path.join(projectPath, 'routines');
      const hasRoutines = fs.existsSync(routinesPath);

      // Read folder structure: localr/ and routines/ subdirectories
      const structure = { localr: [], routines: [] };
      let totalCount = 0;
      if (hasRoutines) {
        const localrPath = path.join(routinesPath, 'localr');
        const routinesSubPath = path.join(routinesPath, 'routines');

        if (fs.existsSync(localrPath)) {
          structure.localr = fs.readdirSync(localrPath).filter(f => f.endsWith('.m'));
          totalCount += structure.localr.length;
        }
        if (fs.existsSync(routinesSubPath)) {
          structure.routines = fs.readdirSync(routinesSubPath).filter(f => f.endsWith('.m'));
          totalCount += structure.routines.length;
        }
      }

      return {
        ok: true,
        projectPath,
        routinesPath,
        hasRoutines,
        message: `Opened project with ${totalCount} routines (${structure.localr.length} in localr, ${structure.routines.length} in routines)`,
        structure
      };
    } catch (err) {
      return { ok: false, error: err.message };
    }
  },

  async lint(code) {
    const lines = code.split('\n');
    const todos = lines.filter(l => /TODO/i.test(l)).length;
    if (lines.length === 1 && lines[0].trim() === '') {
      return { ok: false, error: 'No code provided.' };
    }
    return { ok: true, summary: `Lines: ${lines.length}, TODOs: ${todos}` };
  },

  async execute(code) {
    const res = await executeYDB(code);
    if (res.ok) return { ok: true, output: res.stdout || res.stderr || '(no output)' };

    const rawErr = res.error || res.stderr || '';
    if (/is not running/i.test(rawErr)) {
      return { ok: false, error: 'Docker container is not running. Start it or pick another container from the Docker list.' };
    }
    if (/permission denied.*docker.sock/i.test(rawErr)) {
      return { ok: false, error: 'No permission to talk to Docker. Add user to docker group or run with proper permissions.' };
    }
    if (/No such container/i.test(rawErr)) {
      return { ok: false, error: 'Container not found. Pick a running container from the Docker list.' };
    }
    return { ok: false, error: rawErr || 'Execution failed' };
  },

  async listRoutines(search = '') {
    const routineDirs = getRoutineDirs();
    if (!routineDirs.length) return { ok: false, error: 'No routines path configured' };

    const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
    const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;
    const basePath = (cfg.basePath || '').trim();

    // List routines with folder prefix: "localr/ROUTINE", "routines/ROUTINE"
    const allRoutines = [];

    for (const dir of routineDirs) {
      const cmd = `find ${dir} -name '*.m' -type f 2>/dev/null | sort`;
      const res = await runHostCommand(cmd);
      if (!res.ok) continue;

      const files = (res.stdout || '').split('\n').filter(Boolean);

      // Determine folder name (localr or routines)
      const folderName = dir.includes('/localr') ? 'localr' : 'routines';

      files.forEach(fullPath => {
        const routineName = path.basename(fullPath, '.m');
        const routineWithFolder = `${folderName}/${routineName}`;
        if (!search || routineWithFolder.toLowerCase().includes(search.toLowerCase())) {
          allRoutines.push(routineWithFolder);
        }
      });
    }

    // Remove duplicates and sort
    const routines = Array.from(new Set(allRoutines)).sort();

    return { ok: true, routines };
  },

  async readRoutine(name) {
    // Handle both "ROUTINE" and "localr/ROUTINE" formats
    let routine, targetFolder;
    if (name.includes('/')) {
      const parts = name.split('/');
      targetFolder = parts[0]; // "localr" or "routines"
      routine = sanitizeRoutineName(parts[1]);
    } else {
      routine = sanitizeRoutineName(name);
    }

    if (!routine) return { ok: false, error: 'Invalid routine name' };
    const routineDirs = getRoutineDirs();
    if (!routineDirs.length) return { ok: false, error: 'No routines path configured' };
    let lastError = '';

    // If folder specified, search only in that folder
    const dirsToSearch = targetFolder
      ? routineDirs.filter(d => d.includes(`/${targetFolder}`))
      : routineDirs;

    for (const dir of dirsToSearch) {
      const cmd = `cat ${dir}/${routine}.m 2>/dev/null`;
      const res = await runHostCommand(cmd);
      if (res.ok && res.stdout) {
        const folder = dir.includes('/localr') ? 'localr' : 'routines';
        return { ok: true, routine, folder, code: res.stdout };
      }
      lastError = res.error || res.stderr || lastError;
    }
    return { ok: false, error: lastError || 'Routine not found' };
  },

  async saveRoutine(name, code) {
    // Handle both "ROUTINE" and "localr/ROUTINE" formats
    let routine, targetFolder;
    if (name.includes('/')) {
      const parts = name.split('/');
      targetFolder = parts[0]; // "localr" or "routines"
      routine = sanitizeRoutineName(parts[1]);
    } else {
      routine = sanitizeRoutineName(name);
    }

    if (!routine) return { ok: false, error: 'Invalid routine name' };
    if (!code) return { ok: false, error: 'Code is empty' };

    const routineDirs = getRoutineDirs();

    // If folder specified, save to that folder; otherwise use first dir (localr)
    let routinesPath;
    if (targetFolder) {
      routinesPath = routineDirs.find(d => d.includes(`/${targetFolder}`));
      if (!routinesPath) return { ok: false, error: `Folder '${targetFolder}' not found` };
    } else {
      routinesPath = routineDirs[0];
    }

    if (!routinesPath) return { ok: false, error: 'No routines path configured' };

    const b64 = Buffer.from(code, 'utf8').toString('base64');
    const cmd = `printf '%s' '${b64}' | base64 -d > ${routinesPath}/${routine}.m`;
    const res = await runHostCommand(cmd);
    if (!res.ok) return { ok: false, error: res.error || res.stderr || 'Failed to save routine' };

    const folder = routinesPath.includes('/localr') ? 'localr' : 'routines';
    return { ok: true, routine, folder };
  },

  async searchRoutines(term, opts = {}) {
    const routineDirs = getRoutineDirs();
    if (!routineDirs.length) return { ok: false, error: 'No routines path configured' };
    const scoped = routineDirs.filter(d => {
      if (!opts.folder) return true;
      return d.endsWith(`/${opts.folder}`) || d.includes(`/${opts.folder}/`);
    });
    if (!scoped.length) {
      return { ok: false, error: `No routines found in scope "${opts.folder || ''}"` };
    }

    // Build grep flags
    const flags = ['-R', '-n', '-I', '--with-filename', '--color=never', "--include='*.m'"];
    if (!opts.matchCase) flags.push('-i');
    if (opts.wholeWords) flags.push('-w');
    flags.push(opts.regex ? '-E' : '-F');

    const pattern = shellQuote(term || '');
    const dirs = scoped.map(d => shellQuote(d)).join(' ');
    // Use `|| true` so grep exit code 1 (no matches) doesn't bubble as an error
    const cmd = `grep ${flags.join(' ')} ${pattern} ${dirs} 2>/dev/null || true`;
    const res = await runHostCommand(cmd);
    if (!res.ok) {
      return { ok: false, error: res.error || res.stderr || 'Search failed' };
    }

    const hits = (res.stdout || '')
      .split('\n')
      .filter(Boolean)
      .map(line => {
        const first = line.indexOf(':');
        const second = first === -1 ? -1 : line.indexOf(':', first + 1);
        if (first === -1 || second === -1) return null;
        const filePath = line.slice(0, first);
        const lineNum = parseInt(line.slice(first + 1, second), 10);
        const snippet = line.slice(second + 1);
        const folder = filePath.includes('/localr/') ? 'localr' : 'routines';
        const base = path.basename(filePath, '.m').toUpperCase();
        const routine = `${folder}/${base}`;
        return { file: routine, line: isNaN(lineNum) ? null : lineNum, snippet };
      })
      .filter(Boolean);

    return { ok: true, hits };
  },

  async zlinkRoutine(name) {
    // Handle both "ROUTINE" and "localr/ROUTINE" formats
    let routine;
    if (name.includes('/')) {
      const parts = name.split('/');
      routine = sanitizeRoutineName(parts[1]);
    } else {
      routine = sanitizeRoutineName(name);
    }

    if (!routine) return { ok: false, error: 'Invalid routine name' };
    const res = await executeYDB(`ZLINK "${routine}"`);
    if (!res.ok) return { ok: false, error: res.error || res.stderr || 'ZLINK failed' };
    return { ok: true, output: res.stdout || '' };
  },

  async hostExec(command) {
    if (!command || !command.trim()) return { ok: false, error: 'No command provided' };
    const res = await runHostCommand(command);
    if (!res.ok) return { ok: false, error: res.error || res.stderr || 'Command failed' };
    return { ok: true, stdout: res.stdout, stderr: res.stderr };
  },
  async git(command) {
    if (!command || !command.trim()) return { ok: false, error: 'No git command provided' };
    // Run git on the host first; if it fails, return that error instead of falling back to containers without git
    const local = await runLocalGitCommand(command);
    if (local.ok) return local;
    return {
      ok: false,
      error: local.error || local.stderr || 'Git command failed (host git unavailable or not a repo)',
      stdout: local.stdout,
      stderr: local.stderr
    };
  },

  getGitConfig(projectPath) {
    return readGitConfig(projectPath);
  },

  setConnection(type, cfg = {}) {
    if (type === 'docker') {
      connectionConfig.type = 'docker';
      connectionConfig.docker = mergeDockerConfig(cfg.docker || {});
    } else if (type === 'ssh') {
      connectionConfig.type = 'ssh';
      connectionConfig.ssh = mergeSshConfig(cfg.ssh || {});
    }
    return { ok: true, type: connectionConfig.type, config: connectionConfig };
  },

  // Line-by-line debug execution with real variable capture
  async debugStart(code, breakpoints = [], startLine = null) {
    console.log(`[DEBUG] debugStart called. USE_ZSTEP_ENGINE=${USE_ZSTEP_ENGINE}, startLine=${startLine}`);

    if (USE_ZSTEP_ENGINE) {
      console.log('[DEBUG] Starting ZSTEP engine...');
      const zres = await startZStepSession(code, breakpoints, startLine);
      if (!zres.ok) {
        console.log(`[DEBUG] ZSTEP engine failed: ${zres.error}`);
        return { ok: false, error: zres.error || 'zstep engine unavailable', output: zres.output || '' };
      }
      console.log('[DEBUG] ZSTEP engine started successfully');
      return {
        ok: true,
        sessionId: zres.sessionId,
        currentLine: zres.currentLine,
        currentRoutine: zres.currentRoutine || 'TMPDBG',
        locals: {},
        callStack: zres.callStack,
        stack: zres.stack || formatCallStackForClient(zres.callStack),
        output: '',
        engine: 'zstep'
      };
    }

    const id = `dbg_${Date.now()}_${Math.random().toString(16).slice(2)}`;
    const lines = code
      .split('\n')
      .map(l => l.replace(/\r/g, ''));
    const startAt = (Number.isInteger(startLine) && startLine > 0) ? startLine : 1;

    debugSessions[id] = {
      code,
      lines,
      currentLine: startAt,
      currentRoutine: 'TMPDBG',
      breakpoints,
      locals: {},
      callStack: [{
        routine: 'TMPDBG',
        line: startAt,
        returnLine: null,
        locals: {}
      }],
      stack: [],
      output: []
    };
    const session = debugSessions[id];
    advanceToNextExecutableLine(session);
    if (session.callStack[0]) {
      session.callStack[0].line = session.currentLine;
    }
    session.currentRoutine = (session.callStack[session.callStack.length - 1] || {}).routine || session.currentRoutine || 'TMPDBG';
    session.stack = session.callStack.map(f => `${f.routine}:${f.line}`);

    return {
      ok: true,
      sessionId: id,
      currentLine: session.currentLine,
      currentRoutine: session.currentRoutine,
      locals: {},
      stack: session.stack,
      output: ''
    };
  },

  async debugStep(sessionId, stepType = 'into') {
    const session = debugSessions[sessionId];
    if (!session) return { ok: false, error: 'Session not found' };

    if (session.engine === 'zstep') {
      const cmdMap = { into: 'INTO', over: 'OVER', out: 'OUTOF' };
      // If stepping into an external routine, add a temporary breakpoint at target entry
      if (stepType === 'into') {
        try {
          const smap = await loadRoutineSourceMap(session.currentRoutine, session.currentRoutine === 'TMPDBG' ? session.sourceMap : null);
          const lineText = smap?.lines?.[session.currentLine - 1] || '';
          const trimmed = lineText.replace(/^[\t ]+/, '');
          const extMatch =
            trimmed.match(/^(?:D|DO)\s+(?:([A-Za-z%][A-Za-z0-9]*)\s*)?\^([A-Za-z%][A-Za-z0-9]+)/i) ||
            trimmed.match(/^SET\s+\w+\s*=\s*\$\$([A-Za-z%][A-Za-z0-9]*)\s*\^([A-Za-z%][A-Za-z0-9]+)/i);
          if (extMatch) {
            const tag = (extMatch[1] || extMatch[3] || '').trim();
            const routine = (extMatch[2] || extMatch[4] || '').toUpperCase();
            const targetMap = await loadRoutineSourceMap(routine);
            if (targetMap) {
              let tagLine = 1;
              if (tag) {
                for (const entry of targetMap.map) {
                  if (entry.tag && entry.tag.toUpperCase() === tag.toUpperCase()) {
                    tagLine = entry.line;
                    break;
                  }
                }
              } else {
                // default to first executable line
                tagLine = nextExecutableLine(targetMap, 1);
              }
              const targetLine = nextExecutableLine(targetMap, tagLine);
              const key = `${routine}#${targetLine}`;
              const already = (session.zstepUserBps && session.zstepUserBps.has(key)) || (session.autoBps && session.autoBps.has(key));
              if (!already) {
                session.autoBps = session.autoBps || new Set();
                session.autoBps.add(key);
                session.proc.stdin.write(`SETBP;${routine};${targetLine}\n`);
              }
            }
          }
        } catch (e) {
          // fall through; stepping will still work without temp breakpoint
        }
      }
      return sendZStepCommand(sessionId, cmdMap[stepType] || 'INTO');
    }

    // Ensure call stack exists
    if (!Array.isArray(session.callStack) || !session.callStack.length) {
      session.callStack = [{
        routine: session.currentRoutine || 'TMPDBG',
        line: session.currentLine || 1,
        returnLine: null,
        locals: session.locals || {}
      }];
    }

    // Align to executable line
    advanceToNextExecutableLine(session);
    const topFrame = session.callStack[session.callStack.length - 1];
    if (topFrame) topFrame.line = session.currentLine;
    if (session.currentLine > session.lines.length) {
      return { ok: false, error: 'End of code reached' };
    }

    const currentLineText = session.lines[session.currentLine - 1] || '';
    const trimmed = currentLineText.replace(/^[\t ]+/, '');

    // External routine call, step into: return target without executing
    if (stepType === 'into') {
      const externalCallMatch =
        trimmed.match(/^(?:D|DO)\s+(?:([A-Za-z%][A-Za-z0-9]*)\s*)?\^([A-Za-z%][A-Za-z0-9]+)/i) ||
        trimmed.match(/^SET\s+\w+\s*=\s*\$\$([A-Za-z%][A-Za-z0-9]*)\s*\^([A-Za-z%][A-Za-z0-9]+)/i);
      if (externalCallMatch) {
        const tag = (externalCallMatch[1] || externalCallMatch[3] || '').trim();
        const routine = externalCallMatch[2] || externalCallMatch[4];
        return {
          ok: true,
          isExternalCall: true,
          callTarget: { routine, tag: tag || '' },
          currentLine: session.currentLine,
          locals: session.locals,
          stack: session.callStack.map(f => `${f.routine}:${f.line}`)
        };
      }

      // Local tag call (same routine)
      const localTagMatch = trimmed.match(/^(?:D|DO)\s+([A-Za-z%][A-Za-z0-9]*)\b(?!\s*\^)/i);
      if (localTagMatch) {
        const tag = localTagMatch[1];
        let tagLine = null;
        const tagDefRe = new RegExp(`^${tag}(\\s|;|\\(|$)`, 'i');
        for (let i = 0; i < session.lines.length; i++) {
          const tline = (session.lines[i] || '').replace(/^[\t ]+/, '');
          if (tagDefRe.test(tline)) {
            tagLine = i + 1;
            break;
          }
        }
        if (tagLine) {
          const returnLine = session.currentLine + 1;
          session.callStack.push({
            routine: session.callStack[session.callStack.length - 1].routine,
            line: tagLine,
            tag,
            returnLine,
            locals: { ...session.locals }
          });
          session.currentLine = tagLine;
          advanceToNextExecutableLine(session);
          return {
            ok: true,
            isLocalTagCall: true,
            tagLine: session.currentLine,
            tagName: tag,
            currentLine: session.currentLine,
            locals: session.locals,
            stack: session.callStack.map(f => `${f.routine}:${f.line}`)
          };
        }
      }
    }

    // Build execution code up to current line, skipping comment-only lines
    const linesToExecute = (session.lines || [])
      .slice(0, session.currentLine)
      .map((line) => {
        if (isSkippableDebugLine(line)) return '';
        const raw = line.replace(/\r/g, '');
        const trimmedLine = raw.replace(/^[\t ]+/, '');
        if (/^\S/.test(raw)) {
          const parts = raw.split(/\s+/);
          if (parts.length > 1) return parts.slice(1).join(' ');
          return '';
        }
        return trimmedLine;
      })
      .filter(Boolean);

    let codeToExecute = linesToExecute.join('\n');
    codeToExecute += '\nWRITE "<<<DEBUG_VARS_START>>>",!\nZWRITE\nWRITE "<<<DEBUG_VARS_END>>>",!\n';

    const result = await executeYDBDirect(codeToExecute);
    if (!result.ok) {
      return { ok: false, error: result.error || 'Execution failed' };
    }

    const output = result.stdout || result.output || '';
    const lines = output.split('\n');
    const locals = {};
    const execOutput = [];

    for (const line of lines) {
      const t = line.trim();
      if (!t) continue;
      const match = t.match(/^([A-Za-z%][A-Za-z0-9]*)(\([^)]+\))?\s*=\s*(.*)$/);
      if (match) {
        const varName = match[1].toUpperCase();
        const subscript = match[2];
        let value = match[3].trim();
        if ((value.startsWith('"') && value.endsWith('"')) ||
            (value.startsWith("'") && value.endsWith("'"))) {
          value = value.slice(1, -1);
        }
        if (subscript) {
          if (!locals[varName]) locals[varName] = { _isArray: true, _elements: {} };
          locals[varName]._elements[subscript] = value;
        } else {
          locals[varName] = value;
        }
      } else if (
        !t.includes('ZWRITE') &&
        !t.includes('<<<DEBUG_VARS_START>>>') &&
        !t.includes('<<<DEBUG_VARS_END>>>') &&
        !t.includes('QUIT') &&
        !t.startsWith('>')
      ) {
        execOutput.push(line);
      }
    }

    const executedLineText = trimmed;
    const isQuit = /^\s*Q(?:UIT)?(?:\s|;|$)/i.test(executedLineText);

    if (isQuit && session.callStack.length > 1) {
      const frame = session.callStack.pop();
      session.currentLine = frame.returnLine || session.currentLine + 1;
    } else {
      session.currentLine += 1;
    }

    session.locals = locals;
    session.stack = session.callStack.map(f => `${f.routine}:${f.line}`);
    session.output = (session.output || []).concat(execOutput);

    // Skip comment-only lines after moving
    advanceToNextExecutableLine(session);

    return {
      ok: true,
      currentLine: session.currentLine,
      currentRoutine: (session.callStack[session.callStack.length - 1] || {}).routine || session.currentRoutine || 'TMPDBG',
      locals,
      stack: session.stack,
      output: execOutput.join('\n'),
      isReturn: isQuit && session.callStack.length >= 1
    };
  },

  async debugContinue(sessionId) {
    const session = debugSessions[sessionId];
    if (!session) return { ok: false, error: 'Session not found' };

    if (session.engine === 'zstep') {
      return sendZStepCommand(sessionId, 'CONTINUE');
    }

    // Execute all lines until next breakpoint or end
    while (session.currentLine <= session.lines.length) {
      // Check if current line is a breakpoint
      if (session.breakpoints.includes(session.currentLine)) {
        break;
      }

      // Execute current line
      const stepResult = await this.debugStep(sessionId, 'over');
      if (!stepResult.ok) break;
    }

    return {
      ok: true,
      currentLine: session.currentLine,
      currentRoutine: session.currentRoutine,
      locals: session.locals,
      stack: session.stack,
      output: session.output.join('\n')
    };
  },

  async debugStop(sessionId) {
    const session = debugSessions[sessionId];
    if (session && session.engine === 'zstep') {
      try {
        session.proc?.kill('SIGTERM');
      } catch (e) {
        // ignore
      }
    }
    delete debugSessions[sessionId];
    return { ok: true };
  },

  async sshConnect(config) {
    const sshCtor = ensureSshClient();
    if (!sshCtor) {
      return { ok: false, error: `SSH not available (missing native module ssh2${sshLoadError ? `: ${sshLoadError.message}` : ''})` };
    }
    return new Promise((resolve) => {
      const conn = new sshCtor();
      const id = `ssh_${Date.now()}_${Math.random().toString(16).slice(2)}`;

      conn.on('ready', () => {
        // Store connection details for execute/ debug over SSH
        connectionConfig.type = 'ssh';
        connectionConfig.ssh = mergeSshConfig(config || {});
        sshSessions[id] = conn;
        resolve({ ok: true, sessionId: id });
      }).on('error', (err) => {
        resolve({ ok: false, error: err.message });
      }).connect({
        host: config.host,
        port: config.port || 22,
        username: config.username,
        password: config.password
      });
    });
  },

  async sshExec(sessionId, command) {
    const conn = sshSessions[sessionId];
    if (!conn) return { ok: false, error: 'SSH session not found' };

    return new Promise((resolve) => {
      conn.exec(command, (err, stream) => {
        if (err) return resolve({ ok: false, error: err.message });
        let stdout = '';
        let stderr = '';
        stream.on('close', () => {
          resolve({ ok: true, stdout, stderr });
        }).on('data', (data) => {
          stdout += data.toString();
        }).stderr.on('data', (data) => {
          stderr += data.toString();
        });
      });
    });
  },

  async sshDisconnect(sessionId) {
    const conn = sshSessions[sessionId];
    if (conn) {
      conn.end();
      delete sshSessions[sessionId];
    }
    return { ok: true };
  }
};
