// MUMPS bridge using local runtime (YottaDB/GT.M) via Docker or SSH (mirrors web backend defaults)
const { spawn, exec } = require('child_process');
const fs = require('fs');
const path = require('path');
const { logger } = require('./utils/logger');
const { log: dbgLog } = require('./utils/debug-log');
const net = require('net');
const { EventEmitter } = require('events');

// In snap environment, ensure PATH includes snap + system binaries (git/docker live under $SNAP/usr/bin).
if (process.env.SNAP) {
  const snap = String(process.env.SNAP || '').trim();
  const snapPaths = snap
    ? `${snap}/usr/local/sbin:${snap}/usr/local/bin:${snap}/usr/sbin:${snap}/usr/bin:${snap}/sbin:${snap}/bin`
    : '';
  const sysPaths = '/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin';
  const prefix = snapPaths ? `${snapPaths}:${sysPaths}` : sysPaths;
  process.env.PATH = prefix + (process.env.PATH ? ':' + process.env.PATH : '');
}
let SSHClient = undefined;
let sshLoadError = null;
let mdebugEnsureLock = false;
function hasActiveMdebugSession() {
  return Object.keys(mdebugSessions).length > 0;
}
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
function buildSshPaths(envKey = DEFAULT_ENV_KEY, ydbPath = '/opt/fis-gtm/YDB136') {
  return {
    ydbPath,
    ...buildEnvPaths(envKey)
  };
}

function buildDockerPaths(envKey = DOCKER_DEFAULT_ENV_KEY, ydbPath = null) {
  // In universal mode (no ydbPath), don't set any YottaDB-specific paths
  if (!ydbPath) {
    return {
      ydbPath: null,
      gldPath: null,
      routinesPath: null,
      rpcRoutinesPath: null,
      basePath: null,
      envKey: envKey || DOCKER_DEFAULT_ENV_KEY
    };
  }
  // In configured mode, build all the paths
  const basePaths = buildEnvPaths(envKey);
  return { ydbPath, ...basePaths };
}

function mergeSshConfig(cfg = {}) {
  const fallbackEnvKey = cfg.envKey || cfg.baseKey || cfg.namespace || connectionConfig.ssh?.envKey || DEFAULT_ENV_KEY;
  const envKey = deriveEnvKeyFromUsername(cfg.username, fallbackEnvKey);
  const ydbPath = cfg.ydbPath || connectionConfig.ssh?.ydbPath || '/opt/fis-gtm/YDB136';
  const paths = buildSshPaths(envKey, ydbPath);
  return {
    ...connectionConfig.ssh,
    ...paths,
    ...cfg,
    envKey
  };
}

function mergeDockerConfig(cfg = {}) {
  // Use envKey from config if provided, otherwise use default
  const envKey = cfg.envKey || connectionConfig.docker?.envKey || DOCKER_DEFAULT_ENV_KEY;
  // ydbPath is optional - if not provided, Docker works in universal mode
  const ydbPath = cfg.ydbPath !== undefined ? cfg.ydbPath : connectionConfig.docker?.ydbPath;
  const paths = buildDockerPaths(envKey, ydbPath);
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
    containerId: null, // Will be set when user selects a container
    envKey: DOCKER_DEFAULT_ENV_KEY,
    ydbPath: null,  // null = universal mode (no YottaDB paths)
    gldPath: null,
    routinesPath: null,
    rpcRoutinesPath: null,
    basePath: null
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
const mdebugSessions = {};
// Default to the richer zstep engine unless explicitly disabled
const USE_ZSTEP_ENGINE = process.env.AHMAD_IDE_DEBUG_ENGINE !== 'legacy';
const sourceMapCache = {};
// Default MDEBUG host/port (using AHMDBG.m on port 9200 to avoid zombie connections on 9000)
const MDEBUG_DEFAULT_HOST = process.env.MDEBUG_HOST || '127.0.0.1';
const MDEBUG_DEFAULT_PORT = parseInt(process.env.MDEBUG_PORT || '9200', 10);

// --- MDEBUG TCP client (parity with mumps-debug-master) ---
const mdebugStates = {
  disconnected: 'disconnected',
  waitingForStart: 'waitingForStart',
  waitingForVars: 'waitingForVars',
  waitingForBreakpoints: 'waitingForBreakpoints',
  waitingForSingleVar: 'waitingForSingleVar',
  waitingForSingleVarContent: 'waitingForSingleVarContent',
  waitingForErrorReport: 'waitingForErrorReport',
  waitingForHints: 'waitingForHints',
  waitingForGlobals: 'waitingForGlobals'
};

function normalizeRoutineName(name = '') {
  const trimmed = (name || '').trim();
  if (!trimmed) return '';
  return trimmed.replace(/\.m$/i, '').toUpperCase();
}

function defaultRoutinePath(routineName) {
  const cfg = connectionConfig.docker || {};
  const root = (cfg.routinesPath || cfg.rpcRoutinesPath || '').split(' ')[0] || '';
  const norm = normalizeRoutineName(routineName);
  if (!norm) return '';
  if (!root) return `${norm}.m`;
  return `${root}/${norm}.m`;
}

async function getDockerContainerIp() {
  try {
    const id = (connectionConfig.docker || {}).containerId;
    if (!id) return null;
    return await new Promise((resolve, reject) => {
      const inspectCmd = wrapDockerCmd(`docker inspect -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' ${id}`);
      exec(inspectCmd, { timeout: 4000 }, (err, stdout) => {
        if (err) return reject(err);
        const ip = (stdout || '').trim();
        resolve(ip || null);
      });
    });
  } catch (e) {
    return null;
  }
}

async function isPortOpen(host, port, timeoutMs = 800) {
  return new Promise((resolve) => {
    const sock = net.connect({ host, port, timeout: timeoutMs }, () => {
      sock.destroy();
      resolve(true);
    });
    sock.on('error', () => {
      sock.destroy();
      resolve(false);
    });
    sock.on('timeout', () => {
      sock.destroy();
      resolve(false);
    });
  });
}

async function startMdebugServer(targetHost, targetPort) {
  // Use AHMDBG.m (our port 9200 version) instead of MDEBUG.m
  // AHMDBG is already in the project and should be compiled in /tmp/ahmad_dbg
  const ahmdbgPath = path.join(__dirname, 'AHMDBG.m');

  let mdebugSource = null;
  try {
    if (fs.existsSync(ahmdbgPath)) {
      mdebugSource = fs.readFileSync(ahmdbgPath, 'utf8');
    }
  } catch (e) {
  }

  if (!mdebugSource) {
    return { ok: false, error: 'AHMDBG.m source not found in project root.' };
  }

  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;
  const envExports = buildYdbEnv(cfg, { tmpDebugDir: '/tmp/ahmad_dbg' });
  const destDir = '/tmp/ahmad_dbg';
  const destFile = `${destDir}/AHMDBG.m`;

  const writeRes = await writeRemoteFile(destFile, mdebugSource);
  if (!writeRes.ok) {
    return { ok: false, error: writeRes.error || 'Failed to copy AHMDBG.m' };
  }

  const compileCmd = useDocker
    ? wrapDockerCmd(`docker exec ${cfg.containerId} bash -c "${envExports} && cd ${destDir} && ${cfg.ydbPath}/mumps AHMDBG.m"`)
    : `${cfg.password ? `sshpass -p '${cfg.password}' ` : ''}ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${envExports} && cd ${destDir} && ${cfg.ydbPath}/mumps AHMDBG.m"`;

  const compileRes = await new Promise((resolve) => {
    exec(compileCmd, { timeout: 10000, maxBuffer: 5 * 1024 * 1024 }, (err, stdout, stderr) => {
      if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
      resolve({ ok: true, stdout, stderr });
    });
  });
  if (!compileRes.ok) {
    return { ok: false, error: compileRes.error || 'Failed to compile AHMDBG.m' };
  }

  // Kill any existing AHMDBG processes (CRITICAL: AHMDBG only handles ONE connection)
  if (useDocker) {
    await new Promise((resolve) => {
      exec(wrapDockerCmd(`docker exec ${cfg.containerId} pkill -9 -f 'AHMDBG' || true`), { timeout: 3000 }, () => resolve(null));
    });
  } else {
    await new Promise((resolve) => {
      exec(`${cfg.password ? `sshpass -p '${cfg.password}' ` : ''}ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "pkill -9 -f 'AHMDBG' || true"`, { timeout: 3000 }, () => resolve(null));
    });
  }

  // Wait for killed processes and zombie connections to clean up
  await new Promise(r => setTimeout(r, 1000));

  // Start AHMDBG server in background using -d flag for docker exec
  // CRITICAL: Use MAIN^AHMDBG not ^AHMDBG (AHMDBG.m has QUIT on line 6)
  // CRITICAL: Do NOT redirect stdout/stderr - it breaks TCP socket handling in YottaDB
  const startCmd = useDocker
    ? wrapDockerCmd(`docker exec -d ${cfg.containerId} bash -c "cd ${destDir} && ${envExports} && ${cfg.ydbPath}/mumps -run MAIN^AHMDBG"`)
    : `${cfg.password ? `sshpass -p '${cfg.password}' ` : ''}ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${envExports} && cd ${destDir} && nohup ${cfg.ydbPath}/mumps -run MAIN^AHMDBG >/dev/null 2>&1 &"`;

  const startRes = await new Promise((resolve) => {
    exec(startCmd, { timeout: 5000 }, (err, stdout, stderr) => {
      if (err) {
        resolve({ ok: false, error: err.message, stderr });
      } else {
        resolve({ ok: true, stdout, stderr });
      }
    });
  });

  if (!startRes.ok) {
    return { ok: false, error: `Failed to start AHMDBG: ${startRes.error}`, log: startRes.stderr };
  }

  // Wait for server to bind to port (with retries)
  let portOpen = false;
  for (let i = 0; i < 10; i++) {
    await new Promise(r => setTimeout(r, 300));
    portOpen = await isPortOpen(targetHost, targetPort, 500);
    if (portOpen) {
      break;
    }
  }

  if (!portOpen) {
    // Check if process is running
    let processInfo = '';
    try {
      const psRes = await runHostCommand('ps aux | grep -i ahmdbg | grep -v grep');
      if (psRes.ok) processInfo = psRes.stdout.trim();
    } catch (e) {
      // ignore
    }

    // Check network status
    let netInfo = '';
    try {
      const netRes = await runHostCommand('netstat -antp 2>/dev/null | grep 9200');
      if (netRes.ok) netInfo = netRes.stdout.trim();
    } catch (e) {
      // ignore
    }


    return {
      ok: false,
      error: `AHMDBG server failed to bind on ${targetHost}:${targetPort}`,
      processInfo,
      netInfo
    };
  }

  return { ok: true };
}

async function ensureMdebugServer(host, port, { forceRestart = false } = {}) {
  if (!forceRestart) {
    const open = await isPortOpen(host, port, 500);
    if (open) {
      return { ok: true, host, port };
    }
  }
  return startMdebugServer(host, port);
}

async function forceRestartMdebug(host, port) {
  return ensureMdebugServer(host, port, { forceRestart: true });
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

class MDebugClient extends EventEmitter {
  constructor({ host = MDEBUG_DEFAULT_HOST, port = MDEBUG_DEFAULT_PORT } = {}) {
    super();
    this.host = host;
    this.port = port;
    this.socket = null;
    this.state = mdebugStates.disconnected;
    this.buffer = '';
    this.vars = { system: {}, local: {} };
    this.stack = [];
    this.activeBreakpoints = [];  // Server-reported breakpoints (raw position strings)
    this.requestedBreakpoints = []; // Client-requested breakpoints with file/line
    this.pendingStop = null;
    this.pendingPromise = null;
    this.errorLines = [];
    this.singleVar = '';
    this.singleVarContent = '';
    this.lastError = ''; // Track last error for loop detection (mism: MISMATCH #7)
  }

  async connect(retries = 3) {
    if (this.socket) {
      return;
    }

    for (let attempt = 1; attempt <= retries; attempt++) {
      try {

        await new Promise((resolve, reject) => {
          const connectTimeout = setTimeout(() => {
            if (sock && !sock.destroyed) sock.destroy();
            reject(new Error(`Connection timeout after 3s (attempt ${attempt}/${retries})`));
          }, 3000);


          const sock = net.connect(this.port, this.host, () => {
            clearTimeout(connectTimeout);
            this.state = mdebugStates.waitingForStart;
            sock.setNoDelay(true);
            sock.setKeepAlive(true, 1000);
            try { sock.setNoDelay(true); } catch { }
            resolve();
          });


          sock.on('error', (err) => {
            clearTimeout(connectTimeout);
            reject(err);
          });

          sock.on('data', (chunk) => this._onData(chunk.toString()));

          sock.on('end', () => {
            this.state = mdebugStates.disconnected;
          });

          this.socket = sock;
        });

        // Connection successful, return
        return;

      } catch (err) {
        // Clean up failed socket
        if (this.socket) {
          try { this.socket.destroy(); } catch (e) { /* ignore */ }
          this.socket = null;
        }

        if (attempt < retries) {
          await new Promise(r => setTimeout(r, 1000)); // Wait 1s before retry
        } else {
          // Final attempt failed
          throw err;
        }
      }
    }
  }

  _onData(data) {
    this.buffer += data;
    this.buffer = this.buffer.replace(/\r\n/g, '\n');
    let pos;
    while ((pos = this.buffer.indexOf('\n')) !== -1 || (pos = this.buffer.indexOf('\r')) !== -1) {
      const line = this.buffer.slice(0, pos).trimEnd();
      this.buffer = this.buffer.slice(pos + 1);
      if (!line) continue;
      this._processLine(line);
    }

  }
  _isStopLike(line) {
    return /^(STOP|AT|PAUSE|HALT)(?:[ ;])([^\s;]+)(?:[ ;])(\d+)(?:[ ;]|$)/i.test(line);
  }
  _parseStopLike(line) {
    const m = line.match(/^(STOP|AT|PAUSE|HALT)(?:[ ;])([^\s;]+)(?:[ ;])(\d+)(?:[ ;]|$)/i);
    if (!m) return null;
    return { reason: m[1].toUpperCase(), routine: m[2].toUpperCase(), line: parseInt(m[3], 10) || 1 };
  }
  _processLine(line) {

    // NEW: if server emits a plain STOP/AT/PAUSE/HALT line, resolve immediately.
    if (this._isStopLike(line)) {
      const info = this._parseStopLike(line);
      if (info) {
        this._resolveStop({ ok: true, reason: info.reason, file: defaultRoutinePath(info.routine), line: info.line });
        return;
      }
    }

    switch (this.state) {

      case mdebugStates.waitingForStart: {
        if (line === '***STARTVAR') {
          this.state = mdebugStates.waitingForVars;
          this.vars = { system: {}, local: {} };
          this.stack = [];
        } else if (line === '***STARTBP') {
          this.state = mdebugStates.waitingForBreakpoints;
          this.activeBreakpoints = [];
        } else if (line === '***ENDPROGRAM') {
          this._resolveStop({ reason: 'end' });
        }
        if (line === '***READY' || line === '***START') {
          this._write('VARS');
        }

        break;
      }
      case mdebugStates.waitingForVars: {
        if (line === '***ENDVAR') {
          this.state = mdebugStates.waitingForStart;
          this._checkEvents();
        } else {
          const vartype = line.substring(0, 1);
          if (vartype === 'S') {
            this.stack.push(line.substring(2));
            break;
          }
          let varname = line.substring(2, line.indexOf('='));
          while ((varname.split('"').length - 1) % 2 !== 0) {
            varname = line.substring(0, line.indexOf('=', varname.length + 1));
          }
          const value = line.substring(varname.length + 3).replace(/^"/, '').replace(/"$/, '');
          const typeKey = vartype === 'V' ? 'local' : 'system';
          this.vars[typeKey][varname] = value;
        }
        break;
      }
      case mdebugStates.waitingForBreakpoints: {
        if (line === '***ENDBP') {
          this.state = mdebugStates.waitingForStart;
          // Verify that requested breakpoints match server's active breakpoints (MISMATCH #2 fix)
          this._verifyBreakpoints();
        } else {
          this.activeBreakpoints.push(line);
        }
        break;
      }
      default:
        break;
    }
  }

  _write(msg) {
    if (!this.socket) throw new Error('Socket not connected');
    // AHMDBG is picky about CRLF; always send \r\n
    const out = msg.endsWith('\r\n')
      ? msg
      : (msg.endsWith('\n') ? msg.replace(/\n$/, '\r\n') : `${msg}\r\n`);
    this.socket.write(out);
  }


  async start(routineOrFile, breakpoints = [], stopOnEntry = false, startTimeoutMs = 10000) {
    // Extract routine name from file path if needed
    // Input can be: "AAAA", "TAG^AAAA", or "/path/to/AAAA.m"
    let routine = routineOrFile;
    if (routine.includes('/') || routine.endsWith('.m')) {
      // Extract routine name from file path: /var/.../AAAA.m -> AAAA
      const baseName = routine.split('/').pop().replace(/\.m$/i, '');
      routine = baseName.toUpperCase();
    }

    await this.connect();

    this.clearBreakpoints(routine);

    this.setBreakpoints(routine, breakpoints);

    // MISMATCH #3 fix: Handle stopOnEntry for both labels and regular routines
    if (stopOnEntry) {
      if (routine.indexOf("^") !== -1) {
        // File contains a label reference (TAG^ROUTINE) - stop at label line (line 0)
        this._write(`SETBP;${routine};0;`);
      } else {
        // Regular routine - stop at line 1 (first executable line)
        this._write(`SETBP;${routine};1`);
      }
    }

    this.requestBreakpoints();

    this._write(`START;${routine};`);
    this._write('VARS');



    // Add timeout to prevent infinite hang
    // Wait for first stop with a real timeout
    let stopInfo;
    try {
      stopInfo = await Promise.race([
        this._waitForStop(),
        new Promise((_, reject) =>
          setTimeout(() => reject(new Error('Timeout waiting for AHMDBG response after START')), startTimeoutMs)
        )
      ]);
    } catch (e) {
      // If we didn't ask to stop on entry, try forcing a break-in once
      if (!stopOnEntry) {
        this._write(`PAUSE;${routine}`);
        stopInfo = await Promise.race([
          this._waitForStop(),
          new Promise((_, reject) =>
            setTimeout(() => reject(new Error('Timeout waiting after PAUSE')), 4000)
          )
        ]);
      } else {
        throw e;
      }
    }

    // Normalize and return stop line/reason
    if (stopInfo && typeof stopInfo.line === 'number') {
      return { line: stopInfo.line, reason: stopInfo.reason || 'STOP' };
    }
    return { line: 1, reason: 'STOP' };

  }

  clearBreakpoints(routineOrFile) {
    // Convert to routine name for AHMDBG command
    let routine = this._extractRoutineName(routineOrFile);

    // Remove requested breakpoints for this routine (track by full path for verification)
    const routinePath = this._normalizePath(defaultRoutinePath(routine));
    this.requestedBreakpoints = this.requestedBreakpoints.filter(bp => bp.file !== routinePath);

    this._write(`CLEARBP;${routine}`);
  }

  setBreakpoints(routineOrFile, bps = []) {
    // Convert to routine name for AHMDBG commands
    let routine = this._extractRoutineName(routineOrFile);
    const routinePath = this._normalizePath(defaultRoutinePath(routine));

    // Track requested breakpoints for verification (use full path)
    bps.forEach((bp) => {
      if (!bp || bp.line == null) return;
      const line = parseInt(bp.line, 10);
      if (!Number.isInteger(line) || line <= 0) return;

      // Store with full file path for later verification
      this.requestedBreakpoints.push({
        file: routinePath,
        line,  // 1-based line number (as sent to server)
        condition: bp.condition || '',
        verified: false
      });

      // But send routine name to AHMDBG
      const cond = (bp.condition || '').trim();
      this._write(cond ? `SETBP;${routine};${line};${cond}` : `SETBP;${routine};${line}`);

    });
  }

  // Helper to extract routine name from file path or routine reference
  _extractRoutineName(routineOrFile) {
    let routine = routineOrFile;
    if (routine.includes('/') || routine.endsWith('.m')) {
      // Extract from file path: /var/.../AAAA.m -> AAAA
      const baseName = routine.split('/').pop().replace(/\.m$/i, '');
      routine = baseName.toUpperCase();
    }
    return routine;
  }

  requestBreakpoints() {
    this._write('REQUESTBP');
  }

  // Normalize file path for comparison (handle case and path separators)
  _normalizePath(filePath) {
    return (filePath || '').replace(/\\/g, '/').toLowerCase();
  }

  // Verify breakpoints after receiving server response (MISMATCH #2 fix)
  _verifyBreakpoints() {

    // Reset verification status
    this.requestedBreakpoints.forEach(bp => bp.verified = false);

    // Match requested breakpoints with active breakpoints from server
    this.requestedBreakpoints.forEach(requestedBp => {
      for (const activeBpStr of this.activeBreakpoints) {
        const activeBp = convertMdebugPosition(activeBpStr);
        const activeBpFile = this._normalizePath(activeBp.routine ? defaultRoutinePath(activeBp.routine) : '');

        // CRITICAL: Server reports 0-based line, we send 1-based
        // So activeBp.line (0-based) + 1 should equal requestedBp.line (1-based)
        if (activeBpFile === requestedBp.file && activeBp.line + 1 === requestedBp.line) {
          requestedBp.verified = true;
          break;
        }
      }
    });

    // Log unverified breakpoints
    const unverified = this.requestedBreakpoints.filter(bp => !bp.verified);
    if (unverified.length > 0) {
      console.warn(`[MDEBUG] ⚠️  ${unverified.length} breakpoint(s) not verified by server:`);
      unverified.forEach(bp => console.warn(`  - ${bp.file}:${bp.line}`));
    }
  }

  async step(type = 'OVER') {
    this._write(type);
    return this._waitForStop();
  }

  async cont() {
    this._write('CONTINUE');
    return this._waitForStop();
  }

  stop() {
    if (this.socket) {
      try {
        this._write('RESET');
        this.socket.end();
      } catch (e) {
        // ignore
      }
      this.socket = null;
      this.state = mdebugStates.disconnected;
    }
    return { ok: true };
  }

  /**
   * Get call stack frames (MISMATCH #5 fix)
   * @param {number} startFrame - Starting frame index (0-based)
   * @param {number} endFrame - Ending frame index
   * @returns {object} { frames: Array<{index, name, file, line}>, count: number }
   *
   * Converts raw MUMPS stack positions to structured stack frames with file/line info
   */
  stack(startFrame = 0, endFrame = 1000) {
    const frames = [];

    for (let i = startFrame; i < this.stack.length && i < endFrame; i++) {
      const position = this.stack[i];
      if (position.indexOf("^") !== -1) {
        const filePosition = convertMdebugPosition(position);
        const filePath = defaultRoutinePath(filePosition.routine);

        frames.push({
          index: i,
          name: `${position} (${i})`,
          file: filePath,
          line: filePosition.line  // 0-based line from convertMdebugPosition
        });
      }
    }

    return {
      frames,
      count: Math.min(frames.length, endFrame - startFrame)
    };
  }

  _waitForStop() {
    if (this.pendingPromise) return this.pendingPromise;
    this.pendingPromise = new Promise((resolve) => {
      this.pendingStop = resolve;
    }).finally(() => {
      this.pendingPromise = null;
    });
    return this.pendingPromise;
  }

  _resolveStop(payload) {
    if (this.pendingStop) {
      const resolve = this.pendingStop;
      this.pendingStop = null;
      resolve(payload);
    }
  }

  _checkEvents() {
    const internals = this.vars.system || {};
    const zpos = internals['$ZPOSITION'] || '';
    const zstatus = internals['$ZSTATUS'] || '';
    const posInfo = convertMdebugPosition(zpos);
    const filePath = defaultRoutinePath(posInfo.routine);
    const currentLine1 = (posInfo.line ?? posInfo.offset ?? 0) + 1;

    // MISMATCH #7 fix: Handle errors with loop detection
    if (zstatus && zstatus !== '') {
      // Check if this is the same error repeating (error loop detection)
      const etrap = internals['$ETRAP'] || '';
      const zstep = internals['$ZSTEP'] || '';

      if (zstatus === this.lastError && etrap === zstep) {
        // Repeated error with same trap = end session
        this._resolveStop({ ok: true, reason: 'end', file: filePath, line: currentLine1 });
        return;
      }

      // New error - track it and report exception
      if (zstatus !== this.lastError) {
        this.lastError = zstatus;
        // Parse error position from $ZSTATUS if available
        const statusParts = zstatus.split(',');
        if (statusParts.length > 1) {
          const errorPosInfo = convertMdebugPosition(statusParts[1]);
          if (errorPosInfo.routine) {
            this._resolveStop({
              ok: true,
              reason: 'exception',
              file: defaultRoutinePath(errorPosInfo.routine),
              line: (errorPosInfo.line ?? 0) + 1,
              error: zstatus
            });
            return;
          }
        }
        // Fallback to current position
        this._resolveStop({ ok: true, reason: 'exception', file: filePath, line: currentLine1, error: zstatus });
        return;
      }
    }

    // MISMATCH #4 fix: Check if current position matches a verified breakpoint
    const currentFile = this._normalizePath(filePath);
    const matchingBp = this.requestedBreakpoints.find(bp =>
      bp.verified &&
      bp.file === currentFile &&
      bp.line === currentLine1  // Both are 1-based now
    );

    if (matchingBp) {
      this._resolveStop({ ok: true, reason: 'breakpoint', file: filePath, line: currentLine1 });
    } else {
      this._resolveStop({ ok: true, reason: 'step', file: filePath, line: currentLine1 });
    }
  }
}


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
  // In snap, docker is bundled at $SNAP/usr/bin/docker
  const dockerPath = process.env.SNAP ? `${process.env.SNAP}/usr/bin/docker` : 'docker';
  return cmd.replace(/^docker\s/, `${dockerPath} `);
}

function buildYdbEnv(cfg = {}, opts = {}) {
  const ydbPath = cfg.ydbPath || '';
  // gldPath should be pre-discovered and passed in via opts.discoveredGldPath or cfg.gldPath
  const gldPath = opts.discoveredGldPath || cfg.gldPath || '';
  const routines = (cfg.rpcRoutinesPath || cfg.routinesPath || '').trim();
  const tmpDebugDir = (opts.tmpDebugDir || '').trim();
  const extra = Array.isArray(opts.extraRoutines) ? opts.extraRoutines.filter(Boolean) : [];
  const routineParts = [];
  if (tmpDebugDir) routineParts.push(`${tmpDebugDir}(${tmpDebugDir})`);
  if (extra.length) extra.forEach(p => routineParts.push(`${p}(${p})`));
  if (routines) routineParts.push(`${routines}(${routines})`);
  routineParts.push(`${ydbPath}/libgtmutil.so ${ydbPath}`);
  const gtmroutines = routineParts.filter(Boolean).join(' ').trim();

  const exports = [];

  // Export gtmgbldir - this should be pre-discovered and passed in
  if (gldPath) {
    exports.push(`export gtmgbldir=${gldPath}`);
  }

  // Export gtm_dist if ydbPath is configured
  if (ydbPath) {
    exports.push(`export gtm_dist=${ydbPath}`);
  }

  // Always set gtmroutines to include our debug directory
  exports.push(`export gtmroutines='${gtmroutines}'`);

  // Disable error traps to prevent interference with debugging
  exports.push(`export gtm_etrap=''`);
  exports.push(`export gtm_ztrap=''`);

  return exports.join(' && ');
}

// Discover gld path from the container/host
async function discoverGldPath() {
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;

  // If gldPath is already configured, use it
  if (cfg.gldPath) {
    return cfg.gldPath;
  }

  // Try to discover from common locations
  const discoveryCmd = "test -f /var/worldvista/prod/hakeem/globals/mumps.gld && echo '/var/worldvista/prod/hakeem/globals/mumps.gld' || (find /var/worldvista -name 'mumps.gld' 2>/dev/null | head -1)";

  return new Promise((resolve) => {
    if (useDocker) {
      const escaped = discoveryCmd.replace(/'/g, `'\\''`);
      const fullCmd = wrapDockerCmd(`docker exec ${cfg.containerId} bash -c '${escaped}'`);
      exec(fullCmd, { timeout: 10000 }, (err, stdout) => {
        const gldPath = (stdout || '').trim();
        dbgLog('[DEBUG] discoverGldPath result:', gldPath || '(empty)');
        resolve(gldPath || '/var/worldvista/prod/hakeem/globals/mumps.gld'); // Fallback
      });
    } else {
      const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';
      const fullCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${discoveryCmd.replace(/"/g, '\\"')}"`;
      exec(fullCmd, { timeout: 10000 }, (err, stdout) => {
        const gldPath = (stdout || '').trim();
        dbgLog('[DEBUG] discoverGldPath result:', gldPath || '(empty)');
        resolve(gldPath || '/var/worldvista/prod/hakeem/globals/mumps.gld'); // Fallback
      });
    }
  });
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
  if (idx >= 0 && idx < map.length && map[idx] !== undefined) return map[idx];
  const header = session.headerLines || 0;
  return payloadLine > header ? payloadLine - header : payloadLine;
}

// ---------------------- ZSTEP (external harness) ----------------------

// Robust file write using stdin to avoid command-line length limits (ARG_MAX)
function writeRemoteFile(remotePath, content) {
  return new Promise((resolve) => {
    try {
      const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();

      let cmdProc;

      if (useDocker) {
        const cfg = connectionConfig.docker;
        if (!cfg || !cfg.containerId) {
          return resolve({ ok: false, error: 'Docker container ID not configured' });
        }
        const b64 = Buffer.from(content, 'utf8').toString('base64');
        const fullCmd = wrapDockerCmd(`docker exec -i ${cfg.containerId} bash -c "mkdir -p $(dirname ${remotePath}) && base64 -d > ${remotePath}"`);

        cmdProc = exec(fullCmd, (err, stdout, stderr) => {
          if (err) resolve({ ok: false, error: err.message, stderr });
          else resolve({ ok: true });
        });

        if (!cmdProc || !cmdProc.stdin) {
          return resolve({ ok: false, error: 'Failed to create stdin stream for Docker exec' });
        }

        cmdProc.stdin.on('error', (e) => {
          console.error('writeRemoteFile STDIN error (Docker):', e);
        });
        cmdProc.stdin.write(b64);
        cmdProc.stdin.end();

      } else {
        const cfg = connectionConfig.ssh;
        if (!cfg || !cfg.host) {
          return resolve({ ok: false, error: 'SSH host not configured' });
        }
        const b64 = Buffer.from(content, 'utf8').toString('base64');
        const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';
        const remoteCmd = `mkdir -p $(dirname ${remotePath}) && base64 -d > ${remotePath}`;
        const fullCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${remoteCmd.replace(/"/g, '\\"')}"`;

        cmdProc = exec(fullCmd, (err, stdout, stderr) => {
          if (err) resolve({ ok: false, error: err.message, stderr });
          else resolve({ ok: true });
        });

        if (!cmdProc || !cmdProc.stdin) {
          return resolve({ ok: false, error: 'Failed to create stdin stream for SSH exec' });
        }

        cmdProc.stdin.on('error', (e) => {
          console.error('writeRemoteFile STDIN error (SSH):', e);
        });
        cmdProc.stdin.write(b64);
        cmdProc.stdin.end();
      }
    } catch (err) {
      console.error('[writeRemoteFile] CRITICAL ERROR:', err);
      resolve({ ok: false, error: 'writeRemoteFile crashed: ' + err.message });
    }
  });
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

function spawnZStepProcess(entryRoutine, entryTag = '', discoveredGldPath = '') {
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const tagArg = entryTag ? ` ${entryTag}` : '';
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;
  // Pass discovered gld path to buildYdbEnv
  const envExports = buildYdbEnv(cfg, { tmpDebugDir: '/tmp/ahmad_dbg', discoveredGldPath });
  const cdCmd = 'cd /tmp/ahmad_dbg';
  // Run the AHMDBGJSON entry point in the AHMDBG routine (tag^routine)
  const runCmd = `${envExports} && ${cdCmd} && ${cfg.ydbPath}/mumps -run AHMDBGJSON^AHMDBG ${entryRoutine}${tagArg}`;

  dbgLog('[DEBUG] spawnZStepProcess environment:');
  dbgLog('[DEBUG] envExports:', envExports);
  dbgLog('[DEBUG] runCmd:', runCmd);

  if (useDocker) {
    const cmd = wrapDockerCmd(
      `docker exec -i ${cfg.containerId} bash -c "${runCmd.replace(/"/g, '\\"')}"`
    );
    dbgLog('[DEBUG] Final spawn command:', cmd);
    return spawn('bash', ['-lc', cmd], { stdio: ['pipe', 'pipe', 'pipe'] });
  }
  const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';
  const cmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "${runCmd.replace(/"/g, '\\"')}"`;
  dbgLog('[DEBUG] Final spawn command:', cmd);
  return spawn('bash', ['-lc', cmd], { stdio: ['pipe', 'pipe', 'pipe'] });
}

function resolvePending(session, evt) {
  // If someone is waiting, resolve ONE pending callback
  if (session.pending && session.pending.length > 0) {
    const resolver = session.pending.shift();
    dbgLog('[DEBUG] resolvePending: resolving pending callback with event:', evt.event);
    resolver(evt);
  } else {
    // No one waiting - queue the event for later
    session.eventQueue = session.eventQueue || [];
    session.eventQueue.push(evt);
    dbgLog('[DEBUG] resolvePending: queued event (no pending callbacks):', evt.event, 'queue length:', session.eventQueue.length);
  }
}

function pullQueuedEvent(session, eventName) {
  if (!session || !session.eventQueue || !eventName) return null;
  for (let i = 0; i < session.eventQueue.length; i += 1) {
    if ((session.eventQueue[i] || {}).event === eventName) {
      return session.eventQueue.splice(i, 1)[0];
    }
  }
  return null;
}

function tryParseDebuggerEvent(line) {
  if (!line) return null;

  const attempts = [line];

  // If stdout had leading noise before the JSON, try parsing from the first brace onward
  const braceIdx = line.indexOf('{');
  if (braceIdx > 0) {
    attempts.push(line.slice(braceIdx));
  }

  // Attempt to strip non-printable control chars that can break JSON.parse
  const noControl = line.replace(/[\u0000-\u001F]+/g, '');
  if (noControl !== line) attempts.push(noControl);

  // If we see backslash-escaped quotes, also try a variant where we collapse any over-escaped sequences.
  const collapsed = line.replace(/\\\\+"/g, '\\"');
  if (collapsed !== line) attempts.push(collapsed);

  for (const candidate of attempts) {
    try {
      return JSON.parse(candidate);
    } catch (_) {
      // keep trying
    }
  }

  // Last-chance fallback: extract the event type and output so we can still resolve the pending promise
  const eventMatch = line.match(/"event"\s*:\s*"([^"]+)"/);
  if (!eventMatch) return null;
  const evtType = eventMatch[1];
  const fallback = { event: evtType, raw: line };

  const okMatch = line.match(/"ok"\s*:\s*(\d+)/);
  if (okMatch) fallback.ok = parseInt(okMatch[1], 10);

  const outputMatch = line.match(/"output"\s*:\s*"((?:\\.|[^"])*)"/);
  if (outputMatch) {
    // Preserve the escaped content; decode minimal backslash-escaped quotes so UI can show it.
    fallback.output = outputMatch[1].replace(/\\\\/g, '\\').replace(/\\"/g, '"');
  }

  // Note: locals parsing is intentionally skipped in fallback to keep it simple.
  return fallback;
}

function waitForEvent(session, allowedEvents = ['stopped', 'exit', 'error'], timeoutMs = 10000) {
  return new Promise((resolve) => {
    const allowed = new Set(allowedEvents || []);
    const isAllowed = (evt) => allowed.size === 0 || allowed.has(evt.event);

    const findMatchingQueuedEvent = () => {
      if (!session.eventQueue || session.eventQueue.length === 0) return null;
      for (let i = 0; i < session.eventQueue.length; i++) {
        const evt = session.eventQueue[i];
        if (isAllowed(evt)) {
          session.eventQueue.splice(i, 1);
          return evt;
        }
      }
      return null;
    };

    // Check queue first
    const queuedEvt = findMatchingQueuedEvent();
    if (queuedEvt) {
      resolve(queuedEvt);
      return;
    }

    // No queued event, so wait for one to arrive via resolvePending
    const timer = setTimeout(() => {
      dbgLog('[DEBUG] waitForEvent TIMEOUT after', timeoutMs, 'ms');
      session.pending = (session.pending || []).filter(r => r !== resolver);
      resolve({ event: 'error', message: 'Timeout waiting for debugger event' });
    }, timeoutMs);

    const resolver = (evt) => {
      if (isAllowed(evt)) {
        clearTimeout(timer);
        session.pending = (session.pending || []).filter(r => r !== resolver);
        resolve(evt);
        return;
      }
      // Not for us: keep it queued and keep waiting
      session.eventQueue = session.eventQueue || [];
      session.eventQueue.push(evt);
      session.pending = session.pending || [];
      session.pending.push(resolver);
    };

    // Register this resolver as pending
    session.pending = session.pending || [];
    session.pending.push(resolver);
    dbgLog('[DEBUG] waitForEvent: registered pending callback, queue depth:', session.pending.length);

    // CRITICAL: Check queue again AFTER registering - event might have arrived in between
    const lateEvt = findMatchingQueuedEvent();
    if (lateEvt) {
      clearTimeout(timer);
      session.pending = session.pending.filter(r => r !== resolver);
      dbgLog('[DEBUG] waitForEvent: found late-arriving event in queue:', lateEvt.event);
      resolve(lateEvt);
      return;
    }
  });
}

function waitForZStepEvent(session, timeoutMs = 10000) {
  return waitForEvent(session, ['stopped', 'exit', 'error'], timeoutMs);
}

async function applyZStepEvent(session, evt) {
  // Depth from AHMDBG is $STACK-1 (0-based). Treat 0 as a real frame and normalize the stack to this depth.
  const reportedDepth = Number.isInteger(evt.depth) ? evt.depth : null;
  const targetDepth = reportedDepth !== null
    ? Math.max(1, reportedDepth)
    : (session.callStack.length || 1);
  const currentDepth = session.callStack.length || 0;
  const posInfo = parseZPos(evt.pos || '');
  const routine = (evt.routine || posInfo.routine || (session.callStack[currentDepth - 1] || {}).routine || '').toUpperCase();
  const tag = (evt.tag || posInfo.tag || '').toUpperCase();
  const offset = Number.isInteger(evt.offset) ? evt.offset : posInfo.offset;

  // Normalize call stack length to reported depth
  while (session.callStack.length > targetDepth) session.callStack.pop();

  const pushFrame = () => {
    const caller = session.callStack[session.callStack.length - 1] || {};
    const callerSmap = caller.routine === 'TMPDBG' ? session.sourceMap : sourceMapCache[caller.routine];
    const retLine = nextExecutableLine(callerSmap, (caller.line || 0) + 1);
    session.callStack.push({
      routine: routine || caller.routine || 'TMPDBG',
      line: evt.line || 1,
      tag,
      returnRoutine: caller.routine || null,
      returnLine: retLine,
      returnTag: caller.tag || ''
    });
  };

  while (session.callStack.length < targetDepth) {
    pushFrame();
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
  let computedFromTagOffset = false;

  // LINE COMPUTATION FIX:
  // ALWAYS prefer tag+offset from $ZPOSITION over evt.line when available,
  // because tag+offset is the authoritative source from the runtime.
  // evt.line from AHMDBG's LINENUM can be incorrect when:
  // - Returning from external calls
  // - Stepping through routines with complex control flow
  // - Source file differs from compiled routine
  if (smap && (tag || Number.isInteger(offset))) {
    let tagLine = 1;
    if (tag) {
      // Find the tag definition line in the source map
      for (const entry of smap.map) {
        if (entry.tag && entry.tag.toUpperCase() === tag.toUpperCase()) {
          tagLine = entry.line;
          break;
        }
      }
    }
    // Compute line from tag+offset
    // offset is relative to the tag line (tag+0 = tag definition line)
    targetLine = tagLine + offset;
    computedFromTagOffset = true;
    dbgLog(`[DEBUG] Computed line from tag+offset: routine=${top.routine}, tag=${tag}, offset=${offset}, tagLine=${tagLine}, targetLine=${targetLine}`);

    // Skip comment-only lines to find the next executable line
    targetLine = nextExecutableLine(smap, targetLine || 1);
    dbgLog(`[DEBUG] After skipping comments: targetLine=${targetLine}`);
  }

  // Fallback to evt.line if tag+offset not available
  if (!targetLine) targetLine = evt.line || top.line || 1;

  top.line = targetLine;
  session.currentRoutine = top.routine;
  session.currentLine = targetLine;
  session.currentTag = top.tag || '';
}

function handleZStepStdout(session) {
  return (chunk) => {
    const chunkStr = chunk.toString();
    dbgLog('[DEBUG] AHMDBG stdout chunk received, length:', chunkStr.length);
    dbgLog('[DEBUG] AHMDBG stdout raw:', chunkStr.substring(0, 200));
    session.buffer += chunkStr;
    let idx;
    while ((idx = session.buffer.indexOf('\n')) >= 0) {
      const line = session.buffer.slice(0, idx).trim();
      session.buffer = session.buffer.slice(idx + 1);
      if (!line) continue;
      dbgLog('[DEBUG] AHMDBG stdout line:', line);
      try {
        const evt = tryParseDebuggerEvent(line);
        if (evt) {
          dbgLog('[DEBUG] AHMDBG parsed event:', evt.event);
          resolvePending(session, evt);
        } else {
          dbgLog('[DEBUG] AHMDBG output (unparsed):', line);
          session.output.push(line);
        }
      } catch (e) {
        dbgLog('[DEBUG] AHMDBG output (non-JSON):', line);
        session.output.push(line);
      }
    }
  };
}

function consumeSessionOutput(session) {
  if (!session) return '';
  if (session.buffer && session.buffer.trim().length) {
    session.output = session.output || [];
    session.output.push(session.buffer.trim());
    session.buffer = '';
  }
  const outArr = Array.isArray(session.output) ? session.output : [];
  const start = Number.isInteger(session.outputCursor) ? session.outputCursor : 0;
  const slice = outArr.slice(start);
  session.outputCursor = outArr.length;
  return slice.join('\n');
}


async function startZStepSession(code, breakpoints = [], startLine = null) {
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;

  // Clean up any existing debug sessions first
  for (const [sessionId, session] of Object.entries(debugSessions)) {
    if (session && session.engine === 'zstep') {
      dbgLog('[DEBUG] Cleaning up existing session:', sessionId);
      try {
        if (session.proc && !session.procExited) {
          session.proc.kill('SIGTERM');
          // Give it a moment to exit
          await new Promise(r => setTimeout(r, 200));
        }
      } catch (e) {
        dbgLog('[DEBUG] Error killing old session:', e.message);
      }
      delete debugSessions[sessionId];
    }
  }

  // Remove old TMPDBG files to force clean state
  dbgLog('[DEBUG] Removing old TMPDBG files...');
  let rmTmpCmd;
  if (useDocker) {
    rmTmpCmd = wrapDockerCmd(`docker exec ${cfg.containerId} bash -c "rm -f /tmp/ahmad_dbg/TMPDBG.m /tmp/ahmad_dbg/TMPDBG.o"`);
  } else {
    const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';
    rmTmpCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "rm -f /tmp/ahmad_dbg/TMPDBG.m /tmp/ahmad_dbg/TMPDBG.o"`;
  }

  try {
    await new Promise((resolve) => {
      exec(rmTmpCmd, { timeout: 5000 }, resolve);
    });
  } catch (e) {
    dbgLog('[DEBUG] Warning: Failed to remove old TMPDBG files:', e.message);
  }

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

  const safeUserCode = sanitizeDebugCode(code);
  const { code: guardedUserCode, transformedToUser, userToTransformed } = addGuardQuits(safeUserCode);

  const id = `dbg_${Date.now()}_${Math.random().toString(16).slice(2)}`;


  // Parse the first user tag to check if it has parameters
  const firstTagMatch = safeUserCode.match(/^([A-Za-z%][A-Za-z0-9]*)(\([^)]*\))?/);
  const firstTag = firstTagMatch ? firstTagMatch[1] : '';
  const hasParams = firstTagMatch && firstTagMatch[2];

  // Add () to first tag definition if missing (required for extrinsic calls)
  let finalUserCode = guardedUserCode;
  if (firstTag && !hasParams) {
    // Add () to the label definition: "AAAA ; comment" becomes "AAAA() ; comment"
    const tagRegex = new RegExp(`^(${firstTag})([\\s;])`, 'm');
    finalUserCode = guardedUserCode.replace(tagRegex, `$1()$2`);
    dbgLog('[DEBUG] Added () to label definition for extrinsic call compatibility');
  }

  // Build the full code payload with TMPDBG tag, QUIT, and optionally a START tag
  let codePayload;
  let headerLines;
  let hasStartWrapper = false;

  if (firstTag) {
    hasStartWrapper = true;

    if (hasParams) {
      // If first tag has parameters, create entry that calls it with dummy values
      // Extract parameter count
      const paramList = firstTagMatch[2].slice(1, -1).split(',').map(p => p.trim()).filter(Boolean);
      const dummyArgs = paramList.map(() => '0').join(',');

      codePayload = [
        'TMPDBG ; Debug temp routine',
        ` NEW RET SET RET=$$${firstTag}(${dummyArgs})`,
        ' QUIT:$QUIT RET  QUIT',
        finalUserCode
      ].join('\n');
      headerLines = 3; // TMPDBG, SET RET=$$..., QUIT
    } else {
      // No parameters - use extrinsic call with () (we added () to definition above)
      codePayload = [
        'TMPDBG ; Debug temp routine',
        ` NEW RET SET RET=$$${firstTag}()`,
        ' QUIT:$QUIT RET  QUIT',
        finalUserCode
      ].join('\n');
      headerLines = 3; // TMPDBG, SET RET=$$..., QUIT
    }
  } else {
    // No tags found, use simple structure
    codePayload = `TMPDBG ; Debug temp routine\n QUIT:$QUIT 0  QUIT\n${finalUserCode}`;
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
  // CRITICAL: If caller provided a starting line, adjust for the header and any inserted guard lines
  // This is where we determine WHERE to start debugging (NOT defaulting to line 1)
  if (Number.isInteger(startLine) && startLine > 0) {
    dbgLog(`[DEBUG] User requested start at line ${startLine}`);
    dbgLog('[runtime] User requested start line', { startLine });
    const mappedStart = (userToTransformed.get(startLine) || startLine) + headerLines;
    const adjustedLine = mappedStart;
    const targetLine = nextExecutableLine({ map, lines }, adjustedLine);
    dbgLog(`[DEBUG] Mapped user line ${startLine} -> payload line ${targetLine}`);
    dbgLog('[runtime] Mapped start line', { userLine: startLine, mappedStart, targetLine });

    if (targetLine < 1 || targetLine > lines.length) {
      dbgLog(`[DEBUG] WARNING: Target line ${targetLine} is out of range, falling back to first executable line`);
      dbgLog('[runtime] Start line out of range, using first executable', { targetLine, linesCount: lines.length });
      // Don't use line 1; find first executable line instead
      const firstExec = nextExecutableLine({ map, lines }, headerLines + 1);
      entryTag = (map[firstExec - 1]?.tag || '').toUpperCase();
    } else {
      const entry = map[targetLine - 1];
      entryTag = (entry?.tag || '').toUpperCase();
    }
  } else if (hasStartWrapper) {
    // Always use routine entry when we have a wrapper (no START tag anymore)
    entryTag = '';
    dbgLog('[DEBUG] No start line provided, using routine entry (wrapper handles first tag call)');
    dbgLog('[runtime] Using wrapper for entry', { firstTag, hasParams });
  } else {
    // No tags found, start at routine entry
    entryTag = '';
    dbgLog(`[DEBUG] No tags found, starting at routine entry`);
    dbgLog('[runtime] No tags, starting at routine entry');
  }

  // If the first tag is parameterized but we ended up targeting it directly (e.g. because a start line was provided),
  // force routine entry so the wrapper handles the call with dummy arguments.
  if (hasParams && firstTag && entryTag && entryTag === firstTag.toUpperCase()) {
    dbgLog('[DEBUG] First tag is parameterized; forcing routine entry to use wrapper');
    dbgLog('[runtime] Forcing routine entry for parameterized label', { entryTag, firstTag });
    entryTag = '';
  }

  // Validate entryTag exists; if missing, fall back to the first label or routine entry
  if (entryTag) {
    const hasEntry = map.some((m) => m.isLabel && (m.tag || '').toUpperCase() === entryTag);
    if (!hasEntry) {
      const firstLabel = (map.find((m) => m.isLabel && m.tag) || {}).tag || '';
      dbgLog(`[DEBUG] Entry tag "${entryTag}" not found; falling back to`, firstLabel || '(routine entry)');
      dbgLog('[runtime] Entry tag missing, applying fallback', { requested: entryTag, fallback: firstLabel || '(routine)' });
      entryTag = (firstLabel || '').toUpperCase();
    }
  }
  // As a final guard, if we still don't have a valid label in the payload, clear the entry tag so we start at routine entry
  if (entryTag) {
    const stillMissing = !map.some((m) => m.isLabel && (m.tag || '').toUpperCase() === entryTag);
    if (stillMissing) {
      dbgLog(`[DEBUG] Entry tag "${entryTag}" still not found after fallback; starting at routine entry`);
      dbgLog('[runtime] Entry tag missing after fallback', { entryTag });
      entryTag = '';
    }
  }

  dbgLog('[DEBUG] TMPDBG structure created:');
  dbgLog('[DEBUG] - Header lines:', headerLines);
  dbgLog('[DEBUG] - Entry tag:', entryTag || '(none - routine entry)');
  dbgLog('[DEBUG] - First tag:', firstTag || '(none)');
  dbgLog('[DEBUG] - Has params:', hasParams ? 'yes' : 'no');
  dbgLog('[DEBUG] - Total lines (in-memory):', lines.length);

  // CRITICAL: Show first 15 lines to understand structure
  dbgLog('[DEBUG] - TMPDBG preview (first 15 lines):');
  codePayload.split('\n').slice(0, 15).forEach((line, i) => {
    dbgLog(`[DEBUG]   Line ${i + 1}: ${line}`);
  });

  // Show where entryTag will start execution
  if (entryTag) {
    for (let i = 0; i < map.length; i++) {
      if (map[i].tag && map[i].tag.toUpperCase() === entryTag) {
        dbgLog(`[DEBUG] - Entry tag "${entryTag}" found at line ${map[i].line}, text: "${lines[i]}"`);
        const nextLine = i + 1 < lines.length ? lines[i + 1] : '(end)';
        dbgLog(`[DEBUG] - Next line after entry: "${nextLine}"`);
        break;
      }
    }
  }

  dbgLog('[DEBUG] Ensuring AHMDBG.m harness...');
  const harnessRes = await ensureHarness();
  if (!harnessRes.ok) {
    dbgLog(`[DEBUG] Harness installation failed: ${harnessRes.error || harnessRes.stderr}`);
    return { ok: false, error: harnessRes.error || harnessRes.stderr || 'Failed to install harness' };
  }
  dbgLog('[DEBUG] AHMDBG.m harness installed successfully');

  // Compile AHMDBG.m to ensure it's available
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

  dbgLog('[DEBUG] Compiling AHMDBG.m...');
  dbgLog('[DEBUG] Compile command:', compileCmd);

  const compileRes = await new Promise((resolve) => {
    exec(compileCmd, { timeout: 10000, maxBuffer: 5 * 1024 * 1024 }, (err, stdout, stderr) => {
      if (err) {
        dbgLog(`[DEBUG] Compilation error: ${err.message}`);
        dbgLog(`[DEBUG] stderr: ${stderr}`);
        dbgLog(`[DEBUG] stdout: ${stdout}`);
        return resolve({ ok: false, error: err.message, stdout, stderr });
      }
      resolve({ ok: true, stdout, stderr });
    });
  });

  if (!compileRes.ok) {
    dbgLog(`[DEBUG] Compilation failed: ${compileRes.error || compileRes.stderr}`);
    const friendlyError = summarizeCompileError(
      compileRes.error,
      compileRes.stdout,
      compileRes.stderr
    );
    const output = [compileRes.stderr, compileRes.stdout].filter(Boolean).join('\n');
    return {
      ok: false,
      error: `AHMDBG compilation failed`,
      friendlyError,
      output
    };
  } else {
    dbgLog('[DEBUG] AHMDBG.m compiled successfully');
    if (compileRes.stdout) dbgLog('[DEBUG] Compile output:', compileRes.stdout);
    if (compileRes.stderr) dbgLog('[DEBUG] Compile stderr:', compileRes.stderr);
  }

  // Verify AHMDBG.o exists
  // Use runHostCommand so the docker/ssh wrapper is applied exactly once
  const verifyCmd = `test -f /tmp/ahmad_dbg/AHMDBG.o && echo OK || echo MISSING`;

  const verifyRes = await runHostCommand(verifyCmd);
  dbgLog('[DEBUG] AHMDBG.o verification:', verifyRes.ok ? verifyRes.stdout.trim() : verifyRes.error);

  if (!verifyRes.ok || verifyRes.stdout.trim() !== 'OK') {
    console.log('[ERROR] AHMDBG.o file not found after compilation!');

    // List directory contents for debugging
    const lsCmd = `ls -la /tmp/ahmad_dbg/`;
    const lsRes = await runHostCommand(lsCmd);
    dbgLog('[DEBUG] Directory contents:', lsRes.ok ? lsRes.stdout : lsRes.error);

    return { ok: false, error: 'AHMDBG.o file not found after compilation' };
  }

  // Test if MUMPS can actually access files in /tmp/ahmad_dbg
  dbgLog('[DEBUG] Testing MUMPS access to /tmp/ahmad_dbg...');

  // First, verify we can list the directory and see both .m and .o files
  const listCmd = `ls -lh /tmp/ahmad_dbg/AHMDBG.*`;
  const listRes = await runHostCommand(listCmd);
  dbgLog('[DEBUG] AHMDBG files:', listRes.ok ? listRes.stdout : listRes.error);

  // Check file permissions and ownership
  const statCmd = `stat -c '%a %U:%G %s %n' /tmp/ahmad_dbg/AHMDBG.* 2>&1 || ls -l /tmp/ahmad_dbg/AHMDBG.*`;
  const statRes = await runHostCommand(statCmd);
  dbgLog('[DEBUG] File permissions:', statRes.ok ? statRes.stdout : statRes.error);

  // Try to read the compiled object file to see if it's readable
  const readTestCmd = `test -r /tmp/ahmad_dbg/AHMDBG.o && echo 'READABLE' || echo 'NOT READABLE'`;
  const readRes = await runHostCommand(readTestCmd);
  dbgLog('[DEBUG] AHMDBG.o readable:', readRes.ok ? readRes.stdout.trim() : readRes.error);

  if (!readRes.ok || readRes.stdout.trim() !== 'READABLE') {
    console.log('[ERROR] AHMDBG.o is not readable!');
    return { ok: false, error: 'AHMDBG.o file exists but is not readable - check permissions' };
  }

  // Check what user we're running as vs file ownership
  const whoamiCmd = `whoami`;
  const whoamiRes = await runHostCommand(whoamiCmd);
  dbgLog('[DEBUG] Running as user:', whoamiRes.ok ? whoamiRes.stdout.trim() : whoamiRes.error);

  // Try to dump the first few bytes of AHMDBG.o to verify it's a valid object file
  const hexdumpCmd = `hexdump -C /tmp/ahmad_dbg/AHMDBG.o | head -3`;
  const hexRes = await runHostCommand(hexdumpCmd);
  dbgLog('[DEBUG] AHMDBG.o header:', hexRes.ok ? hexRes.stdout.split('\n')[0] : hexRes.error);

  const destDir = '/tmp/ahmad_dbg';
  const destFile = `${destDir}/TMPDBG.m`;

  // Debug: Log the exact payload being written
  dbgLog('[DEBUG] TMPDBG.m payload:');
  dbgLog('--- START ---');
  dbgLog(codePayload);
  dbgLog('--- END ---');

  const writeRes = await writeRemoteFile(destFile, codePayload);
  if (!writeRes.ok) return { ok: false, error: writeRes.error || writeRes.stderr || 'Failed to write debug code' };

  // Verify the file was written correctly (line count matches expected)
  const expectedLineCount = codePayload.split('\n').length;
  dbgLog('[DEBUG] TMPDBG.m written. Expected lines:', expectedLineCount, ', In-memory lines array:', lines.length);
  if (expectedLineCount !== lines.length) {
    dbgLog('[DEBUG] WARNING: Line count mismatch! File has', expectedLineCount, 'lines but in-memory array has', lines.length);
    dbgLog('[runtime] TMPDBG line count mismatch', { fileLines: expectedLineCount, memoryLines: lines.length });
  }

  // Compile TMPDBG.m to ensure it can be loaded
  dbgLog('[DEBUG] Compiling TMPDBG.m...');
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
        dbgLog(`[DEBUG] TMPDBG compilation error: ${err.message}`);
        dbgLog(`[DEBUG] stderr: ${stderr}`);
        dbgLog(`[DEBUG] stdout: ${stdout}`);
        return resolve({ ok: false, error: err.message, stdout, stderr });
      }
      resolve({ ok: true, stdout, stderr });
    });
  });

  if (!compileTmpRes.ok) {
    dbgLog(`[DEBUG] TMPDBG compilation failed: ${compileTmpRes.error}`);
    const friendlyError = summarizeCompileError(
      compileTmpRes.error,
      compileTmpRes.stdout,
      compileTmpRes.stderr
    );
    const output = [compileTmpRes.stderr, compileTmpRes.stdout].filter(Boolean).join('\n');
    return {
      ok: false,
      error: 'TMPDBG compilation failed',
      friendlyError,
      output
    };
  }
  dbgLog('[DEBUG] TMPDBG.m compiled successfully');

  // Discover gld path before spawning the debugger process
  const discoveredGldPath = await discoverGldPath();
  dbgLog('[DEBUG] discovered gld path:', discoveredGldPath);

  const proc = spawnZStepProcess('TMPDBG', entryTag, discoveredGldPath);
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
    outputCursor: 0,
    procExited: false,
    headerLines: headerLines,  // Track header lines for this session (2 or 5 depending on whether first tag has params)
    payloadToUser,
    userToTransformed,
    lastUserLine: null
  };

  proc.stdout.on('data', handleZStepStdout(session));
  proc.stderr.on('data', (chunk) => {
    const errMsg = chunk.toString();
    session.output.push(errMsg);
    dbgLog('[DEBUG] ZSTEP stderr:', errMsg);
  });
  proc.stdin.on('error', (err) => {
    dbgLog('[DEBUG] ZSTEP stdin error:', err.message);
  });
  proc.on('error', (err) => {
    dbgLog('[DEBUG] ZSTEP process error:', err.message);
  });
  proc.on('exit', (code, signal) => {
    dbgLog('[DEBUG] ZSTEP process exited with code:', code, 'signal:', signal);
    if (session.output && session.output.length > 0) {
      dbgLog('[DEBUG] Full ZSTEP error output:');
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
  dbgLog('[DEBUG] ZSTEP process spawned, stdin is writable');

  debugSessions[id] = session;

  // -------------------------
  // Send breakpoints to runtime (SETBP;<routine>;<tag>;<offset>)
  // -------------------------
  dbgLog('[DEBUG] Setting', breakpoints?.length || 0, 'breakpoints...');
  dbgLog('[DEBUG] TMPDBG has', lines.length, 'lines total');
  dbgLog('[editor] TMPDBG structure', { totalLines: lines.length, headerLines });

  const resolvedBps = [];
  const tagOffsetForLine = (payloadLine) => {
    const idx = payloadLine - 1;
    if (!Number.isInteger(idx) || idx < 0 || idx >= lines.length) {
      dbgLog('[DEBUG] tagOffsetForLine: payloadLine out of bounds', { payloadLine, idx, maxLines: lines.length });
      return null;
    }

    // Find the nearest preceding label by walking backward
    // Look for map entries where isLabel is true
    let labelLine = -1;
    let labelTag = '';
    let labelFullText = '';

    for (let i = idx; i >= 0; i -= 1) {
      const entry = map[i];
      if (entry && entry.isLabel) {
        labelLine = entry.line; // 1-indexed
        labelFullText = (entry.labelText || entry.tag || '');
        labelTag = labelFullText.split('(')[0].toUpperCase(); // Tag without params
        dbgLog('[DEBUG] tagOffsetForLine: Found label', labelTag, '(full:', labelFullText, ') at line', labelLine);
        break;
      }
    }

    if (labelLine < 0 || !labelTag) {
      // No label found - use routine entry point (TMPDBG)
      // Fallback: offset from line 1
      const offset = payloadLine - 1; // 0-based offset from routine start
      dbgLog('[DEBUG] tagOffsetForLine: No label found, using TMPDBG with offset', offset);
      return {
        tag: 'TMPDBG',
        offset: Math.max(0, offset),
        tagLine: 1
      };
    }

    // Calculate offset from label line
    let offset = payloadLine - labelLine;
    const rawOffset = offset; // keep the raw offset for potential fallback attempts

    // CRITICAL FIX: If the label line itself is non-executable (tag-only line),
    // MUMPS counts offsets from the NEXT line, so we subtract 1
    const labelEntry = map[labelLine - 1];
    const labelIsNonExecutable = labelEntry && labelEntry.isComment;

    if (labelIsNonExecutable) {
      offset -= 1;
      dbgLog('[DEBUG] tagOffsetForLine: Label line is non-executable, adjusting offset:', {
        labelTag,
        labelLine,
        targetLine: payloadLine,
        rawOffset: payloadLine - labelLine,
        adjustedOffset: offset
      });
    } else {
      dbgLog('[DEBUG] tagOffsetForLine: Label', labelTag, 'at line', labelLine, ', target line', payloadLine, ', offset:', offset);
    }

    // VALIDATION: Ensure offset is not negative
    if (offset < 0) {
      dbgLog('[DEBUG] tagOffsetForLine: WARNING Negative offset!', { payloadLine, labelLine, offset });
      return null;
    }

    // VALIDATION: Ensure the breakpoint makes sense
    // If label is non-executable, the first line after it is at offset 0
    const expectedTargetLine = labelIsNonExecutable ? (labelLine + 1 + offset) : (labelLine + offset);
    if (expectedTargetLine !== payloadLine) {
      dbgLog('[DEBUG] tagOffsetForLine: WARNING Calculated offset mismatch!', {
        tag: labelTag,
        tagLine: labelLine,
        offset,
        expectedTargetLine,
        actualPayloadLine: payloadLine,
        labelIsNonExecutable
      });
    }

    if (expectedTargetLine > lines.length) {
      dbgLog('[DEBUG] tagOffsetForLine: WARNING Target exceeds bounds!', {
        tag: labelTag,
        tagLine: labelLine,
        offset,
        targetLine: expectedTargetLine,
        maxLines: lines.length
      });
      return null;
    }

    return {
      tag: labelTag,
      offset,
      tagLine: labelLine,
      rawOffset,
      labelIsNonExecutable
    };
  };

  (breakpoints || []).forEach((ln) => {
    // Handle both { line: N } objects and raw numbers
    const rawLine = (typeof ln === 'object' && ln !== null && ln.line !== undefined) ? ln.line : ln;
    const n = parseInt(rawLine, 10);
    dbgLog('[DEBUG] Processing breakpoint:', { original: ln, rawLine, parsed: n });
    if (!Number.isInteger(n) || n <= 0) {
      dbgLog('[DEBUG] Skipping breakpoint: invalid line number', { ln, rawLine, n });
      return;
    }
    const transformedLine = userToTransformed.get(n);
    const mappedUserLine = (transformedLine || n) + headerLines;
    dbgLog('[DEBUG] Breakpoint mapping:', {
      userLine: n,
      transformedLine,
      headerLines,
      mappedUserLine,
      totalLines: lines.length
    });
    const adjustedLine = nextExecutableLine({ map, lines }, mappedUserLine);
    dbgLog('[DEBUG] After nextExecutableLine:', { mappedUserLine, adjustedLine });

    if (!Number.isInteger(adjustedLine) || adjustedLine < 1 || adjustedLine > lines.length) {
      dbgLog('[editor] Breakpoint INVALID', {
        userLine: n,
        mappedLine: mappedUserLine,
        adjustedLine,
        reason: 'outside valid range'
      });
      dbgLog('[DEBUG] Skipping breakpoint: mapped line outside TMPDBG range', { userLine: n, payloadLine: adjustedLine });
      return;
    }

    const tagInfo = tagOffsetForLine(adjustedLine);
    if (!tagInfo) {
      dbgLog('[editor] Breakpoint UNVERIFIED', { userLine: n, adjustedLine, reason: 'no tag info' });
      dbgLog('[DEBUG] Skipping breakpoint: unable to resolve tag info', { userLine: n, payloadLine: adjustedLine });
      return;
    }

    // FINAL VALIDATION: Double-check offset is reasonable (max 1000 lines from tag)
    if (tagInfo.offset > 1000) {
      dbgLog('[DEBUG] WARNING: Breakpoint offset is suspiciously large!', {
        userLine: n,
        tag: tagInfo.tag,
        offset: tagInfo.offset,
        tagLine: tagInfo.tagLine
      });
      dbgLog('[editor] Breakpoint REJECTED', { userLine: n, reason: 'offset too large', offset: tagInfo.offset });
      return;
    }

    // IMPORTANT: Match M-side protocol
    // Command: SETBP;<routine>;<tag>;<offset>
    const bpCmd = `SETBP;TMPDBG;${tagInfo.tag};${tagInfo.offset}\n`;

    dbgLog('[editor] Breakpoint VERIFIED', {
      userLine: n,
      payloadLine: adjustedLine,
      tag: tagInfo.tag,
      offset: tagInfo.offset,
      tagLine: tagInfo.tagLine,
      targetAbsoluteLine: tagInfo.tagLine + tagInfo.offset,
      command: bpCmd.trim()
    });
    dbgLog(
      '[DEBUG] Writing breakpoint command:',
      bpCmd.trim(),
      '(user line',
      n,
      '-> payload line',
      adjustedLine,
      'tag',
      tagInfo.tag,
      'offset',
      tagInfo.offset,
      'tagLine',
      tagInfo.tagLine,
      ')'
    );

    // Don't send breakpoint yet - save for later (after entering user code)
    resolvedBps.push({
      userLine: n,
      payloadLine: adjustedLine,
      tag: tagInfo.tag,
      offset: tagInfo.offset,
      rawOffset: tagInfo.rawOffset,
      labelIsNonExecutable: tagInfo.labelIsNonExecutable,
      tagLine: tagInfo.tagLine,
      command: bpCmd.trim()
    });
  });

  // Check if any breakpoints were successfully resolved
  if (breakpoints && breakpoints.length > 0 && resolvedBps.length === 0) {
    dbgLog('[DEBUG] WARNING: No breakpoints could be resolved! User requested', breakpoints.length, 'breakpoints but none were valid.');
    dbgLog('[editor] Breakpoint resolution failed', {
      requested: breakpoints,
      resolved: 0,
      totalLines: lines.length,
      headerLines
    });
  } else {
    dbgLog('[DEBUG] Successfully resolved', resolvedBps.length, 'of', breakpoints?.length || 0, 'breakpoints');
    dbgLog('[DEBUG] Breakpoints will be set AFTER entering user code to avoid timing issues');
    dbgLog('[editor] Breakpoints resolved', {
      requested: breakpoints?.length || 0,
      resolved: resolvedBps.length,
      breakpoints: resolvedBps.map(bp => `line ${bp.userLine} -> ${bp.tag}+${bp.offset}`)
    });
  }

  // Track breakpoints for logging/manual fallback
  const bpLines = new Set(resolvedBps.map(bp => bp.payloadLine));
  session.manualBreakpoints = bpLines;
  session.userBreakpoints = new Set(resolvedBps.map(bp => bp.userLine));

  // Track user-set breakpoints for TMPDBG so we don't clear them when using temp BPs
  session.zstepUserBps = new Set(resolvedBps.map(bp => `TMPDBG#${bp.payloadLine}`));
  session.autoBps = new Set();

  // IMPORTANT: Store pending breakpoints to set after entering user code
  session.pendingBreakpoints = resolvedBps;
  session.breakpointsInstalled = false;

  dbgLog('[DEBUG] Waiting for ready event...');

  // Wait for ready event (AHMDBG sends 'ready' and waits for command)
  let readyEvt = null;
  const maxAttempts = 10;
  for (let attempt = 0; attempt < maxAttempts; attempt++) {
    const evt = await waitForEvent(session, ['ready', 'stopped', 'exit', 'error'], 10000);
    dbgLog(`[DEBUG] Received event attempt ${attempt + 1}:`, JSON.stringify(evt));

    if (!evt) {
      dbgLog('[DEBUG] No event received, continuing to wait...');
      continue;
    }

    if (evt.event === 'ready') {
      dbgLog('[DEBUG] Got "ready" event - debugger initialized and waiting for command');
      readyEvt = evt;
      break;
    }

    if (evt.event === 'error' || evt.event === 'exit') {
      readyEvt = evt;
      break;
    }

    // Ignore other events (e.g., bp-set/bp-error) here
  }

  dbgLog('[DEBUG] Ready event:', JSON.stringify(readyEvt));

  if (readyEvt) {
    if (readyEvt.event === 'ready') {
      dbgLog('[DEBUG] Debugger ready. Setting up session without executing...');

      // Initialize session state without execution
      session.currentRoutine = readyEvt.routine || 'TMPDBG';
      session.currentTag = readyEvt.tag || '';
      session.currentLine = 1; // Will be set when execution starts
      session.locals = {}; // No variables yet

      dbgLog('[DEBUG] ========== DEBUG SESSION READY ==========');
      dbgLog('[DEBUG] Debugger initialized and waiting for Continue/Step command');
      dbgLog('[DEBUG]   Routine:', session.currentRoutine);
      dbgLog('[DEBUG]   Tag:', session.currentTag);
      dbgLog('[DEBUG] ==============================================');

    } else if (readyEvt.event === 'error') {
      dbgLog('[DEBUG] Error event received:', readyEvt.message);
      session.procExited = true;
      delete debugSessions[id];
      return { ok: false, error: readyEvt.message || 'Debug process error', output: (session.output || []).join('\n') };
    } else if (readyEvt.event === 'exit') {
      dbgLog('[DEBUG] Exit event received before debugger ready');
      session.procExited = true;
      delete debugSessions[id];
      return { ok: false, error: 'Program finished before debugger ready', output: (session.output || []).join('\n') };
    }
  }

  // Return without executing - user must click Continue or Step to start
  const returnValue = {
    ok: true,
    sessionId: id,
    currentLine: null, // Not executing yet
    currentRoutine: session.currentRoutine,
    currentTag: session.currentTag,
    callStack: [],
    stack: [],
    locals: {},
    engine: 'zstep',
    ready: true // Indicate debugger is ready but not running
  };

  dbgLog('[DEBUG] Returning from startZStepSession:', JSON.stringify(returnValue, null, 2));
  return returnValue;
}
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

function summarizeCompileError(err = '', stdout = '', stderr = '') {
  const collected = [];
  [err, stderr, stdout].forEach((src) => {
    if (!src) return;
    const lines = `${src}`.split('\n').map(l => l.trim()).filter(Boolean);
    collected.push(...lines);
  });
  if (!collected.length) return 'Compilation failed';
  const first = collected.find(Boolean) || 'Compilation failed';
  return first.length > 220 ? `${first.slice(0, 220)}…` : first;
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

async function fetchZStepVariables(session) {
  if (!session || session.engine !== 'zstep') return {};
  if (session.procExited) return {};

  try {
    dbgLog('[DEBUG] Fetching variables via GETVARS...');
    session.proc.stdin.write('GETVARS\n');

    // Wait for vars event (or exit/error)
    const evt = await waitForEvent(session, ['vars', 'exit', 'error'], 4000);
    dbgLog('[DEBUG] GETVARS response:', JSON.stringify(evt));

    if (evt.event === 'vars' && evt.vars) {
      dbgLog('[DEBUG] Variables fetched:', Object.keys(evt.vars).length, 'variables');
      return normalizeDebuggerVars(evt.vars);
    }

    dbgLog('[DEBUG] No variables returned from GETVARS');
    return {};
  } catch (err) {
    dbgLog('[DEBUG] Error fetching variables:', err.message);
    return {};
  }
}

async function sendZStepCommand(sessionId, command) {
  const session = debugSessions[sessionId];
  if (!session || session.engine !== 'zstep') return { ok: false, error: 'Session not found' };
  if (session.procExited) {
    return { ok: false, error: 'Program finished', output: (session.output || []).join('\n') };
  }

  // Show what line we're currently at in the source code
  // Use the correct source map based on the current routine
  const currentRoutineSmap = session.currentRoutine === 'TMPDBG'
    ? session.sourceMap
    : sourceMapCache[session.currentRoutine];
  const currentLineText = currentRoutineSmap?.lines?.[session.currentLine - 1] || '(unknown)';
  dbgLog('[DEBUG] Current position before step:', {
    line: session.currentLine,
    routine: session.currentRoutine,
    tag: session.currentTag,
    text: currentLineText.trim()
  });

  dbgLog('[DEBUG] Sending step command:', command);
  dbgLog('[runtime] Sending command', {
    command,
    currentLine: session.currentLine,
    currentRoutine: session.currentRoutine,
    currentLineText: currentLineText.trim(),
    breakpoints: Array.from(session.manualBreakpoints || [])
  });

  try {
    session.proc.stdin.write(`${command}\n`);
  } catch (err) {
    dbgLog('[DEBUG] ERROR writing to stdin:', err.message);
    return { ok: false, error: 'Failed to send command: ' + err.message };
  }

  dbgLog('[DEBUG] Waiting for response to:', command);
  const evt = await waitForZStepEvent(session);
  dbgLog('[DEBUG] Received event for', command, ':', JSON.stringify(evt));

  dbgLog('[runtime] Received event', {
    command,
    event: evt.event,
    pos: evt.pos,
    depth: evt.depth,
    message: evt.message
  });

  if (evt.event === 'error') {
    dbgLog('[runtime] Error event', { error: evt.message });
    return { ok: false, error: evt.message || 'Runtime error', output: consumeSessionOutput(session) };
  }
  if (evt.event === 'exit') {
    session.procExited = true;
    dbgLog('[runtime] Program exited', { output: session.output });
    return { ok: false, error: 'Program finished', output: consumeSessionOutput(session) };
  }
  if (evt.event === 'stopped') {
    await applyZStepEvent(session, evt);

    // Fetch current variables from GT.M
    session.locals = await fetchZStepVariables(session);

    dbgLog('[runtime] Stopped after command', {
      command,
      currentLine: session.currentLine,
      currentRoutine: session.currentRoutine,
      currentTag: session.currentTag,
      atBreakpoint: session.manualBreakpoints?.has(session.currentLine),
      localsCount: Object.keys(session.locals || {}).length
    });
  }

  // Map TMPDBG payload line back to user's editor coordinates
  const isHeaderPos = session.currentRoutine === 'TMPDBG' &&
    Number.isInteger(session.headerLines) &&
    session.headerLines > 0 &&
    session.currentLine <= session.headerLines;

  // Auto-step over header lines for any step command (CONTINUE, INTO, OVER, OUTOF)
  // BUT: Only during initial startup (before breakpoints are installed).
  // Once breakpoints are installed, we're in user code and shouldn't auto-step.
  // This prevents the "double step" bug where the first step from a breakpoint doesn't advance.
  dbgLog('[DEBUG] Checking auto-step: isHeaderPos=', isHeaderPos, 'command=', command, 'currentLine=', session.currentLine, 'headerLines=', session.headerLines, 'breakpointsInstalled=', session.breakpointsInstalled);
  if (isHeaderPos && !session.breakpointsInstalled && (command === 'CONTINUE' || command === 'INTO' || command === 'OVER' || command === 'OUTOF')) {
    dbgLog('[DEBUG] *** AUTO-STEPPING ACTIVATED *** Stopped in header at line', session.currentLine, '- auto-stepping to reach user code');

    const installPendingBreakpoints = async () => {
      if (!session.pendingBreakpoints || session.pendingBreakpoints.length === 0 || session.breakpointsInstalled) return;
      dbgLog('[DEBUG] Installing', session.pendingBreakpoints.length, 'breakpoints now (after header reach)...');
      let installedCount = 0;
      for (const bp of session.pendingBreakpoints) {
        const sendBpCmd = async (cmd, label) => {
          try {
            session.proc.stdin.write(`${cmd}\n`);
            const cmdParts = cmd.split(';');
            const offForLog = cmdParts.length >= 4 ? cmdParts[3] : '(unknown)';
            dbgLog('[DEBUG] Pre-installed breakpoint', label, ':', cmd.trim(), `(line ${bp.payloadLine} tag ${bp.tag} offset ${offForLog})`);
            // Wait briefly for bp-set/bp-error so we can retry if needed
            const evt = await waitForEvent(session, ['bp-set', 'bp-error', 'error', 'exit'], 800);
            if (evt && (evt.event === 'bp-set' || evt.event === 'bp-error' || evt.event === 'error' || evt.event === 'exit')) {
              return evt;
            }
          } catch (err) {
            dbgLog('[DEBUG] ERROR pre-installing breakpoint:', err.message);
          }
          return null;
        };

        // Try multiple candidates to maximize compatibility with GT.M offset rules.
        // Prefer routine-based offsets first because they are always valid, then tag-based forms.
        const candidates = [];
        const routineOffset = Math.max(0, bp.payloadLine - 1); // payload line N => TMPDBG+(N-1)
        candidates.push({ cmd: `SETBP;TMPDBG;TMPDBG;${routineOffset}`, label: 'primary-routine' });

        // Tag-based command computed from mapping (may fail on param labels)
        const primaryCmd = bp.command || `SETBP;TMPDBG;${bp.tag};${bp.offset}`;
        candidates.push({ cmd: primaryCmd, label: 'tag-mapped' });

        if (bp.labelIsNonExecutable && Number.isInteger(bp.rawOffset) && bp.rawOffset !== bp.offset) {
          candidates.push({ cmd: `SETBP;TMPDBG;${bp.tag};${bp.rawOffset}`, label: 'tag-raw' });
        }

        // Some GT.M builds only accept offset 0 after a tag-only line; try it explicitly
        if (bp.labelIsNonExecutable && bp.offset !== 0) {
          candidates.push({ cmd: `SETBP;TMPDBG;${bp.tag};0`, label: 'tag-zero' });
        }

        let success = false;
        for (const cand of candidates) {
          const evt = await sendBpCmd(cand.cmd, cand.label);
          if (!evt) continue;
          if (evt.event === 'bp-set') {
            success = true;
            installedCount += 1;
            break;
          }
          if (evt.event === 'error' || evt.event === 'exit') {
            dbgLog('[DEBUG] Breakpoint install aborted due to process state:', evt.event);
            break;
          }
          // bp-error -> try next candidate
        }

        if (!success) {
          dbgLog('[DEBUG] Breakpoint still failed after trying all strategies:', bp);
        }
      }
      session.breakpointsInstalled = true;
      session.breakpointsSetCount = installedCount;
      // Give AHMDBG time to process breakpoints
      await new Promise(resolve => setTimeout(resolve, 100));
    };

    let stepCount = 0;
    const maxSteps = 20; // Prevent infinite loops

    while (session.currentLine <= session.headerLines && stepCount < maxSteps) {
      dbgLog('[DEBUG] Auto-step', stepCount + 1, ': stepping from header line', session.currentLine);
      try {
        session.proc.stdin.write('INTO\n');
        const nextEvt = await waitForZStepEvent(session);
        dbgLog('[DEBUG] Auto-step received event:', nextEvt.event, 'line:', nextEvt.line);

        if (nextEvt.event === 'stopped') {
          await applyZStepEvent(session, nextEvt);
          session.locals = await fetchZStepVariables(session);
          stepCount++;

          // Check if we're now past the header
          if (session.currentLine > session.headerLines) {
            dbgLog('[DEBUG] *** AUTO-STEP COMPLETE *** Reached user code at line', session.currentLine);
            break;
          }
        } else if (nextEvt.event === 'exit') {
          session.procExited = true;
          return { ok: false, error: 'Program finished', output: consumeSessionOutput(session) };
        } else if (nextEvt.event === 'error') {
          return { ok: false, error: nextEvt.message || 'Runtime error', output: consumeSessionOutput(session) };
        } else {
          break; // Unknown event, stop stepping
        }
      } catch (err) {
        dbgLog('[DEBUG] Error auto-stepping over header:', err.message);
        break;
      }
    }

    if (stepCount >= maxSteps) {
      dbgLog('[DEBUG] WARNING: Reached max auto-step limit, still in header');
    }

    // Now that we're at user code, install breakpoints (after header) before continuing
    if (session.currentLine > session.headerLines && session.pendingBreakpoints && session.pendingBreakpoints.length > 0 && !session.breakpointsInstalled) {
      await installPendingBreakpoints();
    }

    // If user clicked CONTINUE and we have breakpoints, continue to the first breakpoint
    if (command === 'CONTINUE' && session.breakpointsInstalled && session.pendingBreakpoints && session.pendingBreakpoints.length > 0) {
      if (session.breakpointsSetCount && session.breakpointsSetCount > 0) {
        dbgLog('[DEBUG] Continuing to first breakpoint...');
        session.proc.stdin.write('CONTINUE\n');
        const bpEvt = await waitForZStepEvent(session);

        if (bpEvt.event === 'stopped') {
          await applyZStepEvent(session, bpEvt);
          session.locals = await fetchZStepVariables(session);
          dbgLog('[DEBUG] Stopped at breakpoint, line:', session.currentLine);
        } else if (bpEvt.event === 'exit') {
          session.procExited = true;
          return { ok: false, error: 'Program finished', output: consumeSessionOutput(session) };
        } else if (bpEvt.event === 'error') {
          return { ok: false, error: bpEvt.message || 'Runtime error', output: consumeSessionOutput(session) };
        }
      } else {
        dbgLog('[DEBUG] No breakpoints were installed (all attempts failed); performing manual run-to-line fallback.');
        const targetPayload = (session.pendingBreakpoints[0] || {}).payloadLine;
        if (Number.isInteger(targetPayload) && targetPayload > session.currentLine) {
          const maxSteps = Math.max(50, (targetPayload - session.currentLine) + 10);
          let steps = 0;
          let done = false;
          while (steps < maxSteps && !done) {
            try {
              session.proc.stdin.write('INTO\n');
              const stepEvt = await waitForZStepEvent(session);
              if (stepEvt.event === 'stopped') {
                await applyZStepEvent(session, stepEvt);
                session.locals = await fetchZStepVariables(session);
                steps += 1;
                if (session.currentLine >= targetPayload) {
                  dbgLog('[DEBUG] Manual run-to-line reached target payload line', targetPayload, 'after', steps, 'steps');
                  done = true;
                  break;
                }
              } else if (stepEvt.event === 'exit' || stepEvt.event === 'error') {
                dbgLog('[DEBUG] Manual run-to-line aborted due to event:', stepEvt.event);
                done = true;
                break;
              } else {
                break;
              }
            } catch (err) {
              dbgLog('[DEBUG] Manual run-to-line error:', err.message);
              break;
            }
          }
          if (!done) {
            dbgLog('[DEBUG] Manual run-to-line fallback did not reach target (steps:', steps, 'target:', targetPayload, ')');
          }
        } else {
          dbgLog('[DEBUG] No valid target payload line for manual run-to-line fallback.');
        }
      }
    }
  } else {
    dbgLog('[DEBUG] Auto-step NOT triggered');
  }

  // Recalculate isHeaderPos after potential auto-stepping
  let stillInHeader = session.currentRoutine === 'TMPDBG' &&
    Number.isInteger(session.headerLines) &&
    session.headerLines > 0 &&
    session.currentLine <= session.headerLines;

  // EXTERNAL ROUTINE FIX:
  // Only convert payload line to user line for TMPDBG, not for external routines
  // External routines already have the correct line numbers from the source map
  let userLine;
  if (session.currentRoutine === 'TMPDBG') {
    // For TMPDBG, convert payload line to user line
    userLine = payloadLineToUserLine(session, session.currentLine);
    if (stillInHeader && session.lastUserLine) {
      // Stay on the last real user line instead of bouncing to TMPDBG header
      userLine = session.lastUserLine;
    }
    // Update last user line
    if (stillInHeader && session.lastUserLine) {
      userLine = session.lastUserLine;
    } else {
      session.lastUserLine = userLine;
    }
  } else {
    // For external routines, use the line number as-is (already correct from applyZStepEvent)
    userLine = session.currentLine;
    dbgLog(`[DEBUG] External routine ${session.currentRoutine}: using line ${userLine} directly (no payload mapping)`);
  }

  const clientCallStack = (session.callStack || []).map((frame) => {
    // Only convert TMPDBG lines, leave external routine lines as-is
    const frameRoutine = frame.routine || 'TMPDBG';
    return {
      ...frame,
      line: frameRoutine === 'TMPDBG'
        ? payloadLineToUserLine(session, frame.line || frame.returnLine || 1)
        : (frame.line || frame.returnLine || 1),
      returnLine: frameRoutine === 'TMPDBG'
        ? payloadLineToUserLine(session, frame.returnLine || null)
        : (frame.returnLine || null)
    };
  });

  dbgLog(`[DEBUG] Command ${command} completed. Routine: ${session.currentRoutine}, Line: ${userLine} (raw: ${session.currentLine})`);

  return {
    ok: true,
    currentLine: userLine,
    currentRoutine: session.currentRoutine,
    currentTag: session.currentTag,
    callStack: clientCallStack,
    stack: formatCallStackForClient(clientCallStack),
    locals: session.locals || {},
    output: consumeSessionOutput(session)
  };
}

async function sendZStepEval(sessionId, code = '') {
  const session = debugSessions[sessionId];
  if (!session || session.engine !== 'zstep') return { ok: false, error: 'Session not found' };
  if (session.procExited) {
    return { ok: false, error: 'Program finished', output: consumeSessionOutput(session) };
  }

  const prevPayloadLine = session.currentLine || 0;
  const prevUserLine = payloadLineToUserLine(session, prevPayloadLine);
  const safeCode = (code || '').replace(/\r/g, '');
  try {
    session.proc.stdin.write(`EVAL;${safeCode}\n`);
  } catch (err) {
    return { ok: false, error: 'Failed to send eval: ' + err.message };
  }
  const EVAL_TIMEOUT_MS = 15000;
  let evt = await waitForEvent(session, ['eval', 'error', 'exit'], EVAL_TIMEOUT_MS);
  // If the M side is a bit slow, give one more chance to consume a late eval event
  if (evt && evt.event === 'error' && evt.message === 'Timeout waiting for debugger event') {
    const queuedEval = pullQueuedEvent(session, 'eval');
    if (queuedEval) {
      evt = queuedEval;
    } else {
      const graceEvt = await waitForEvent(session, ['eval', 'error', 'exit'], 4000);
      if (graceEvt) evt = graceEvt;
    }
  }
  if (!evt) return { ok: false, error: 'Eval timeout' };
  if (evt.event === 'exit') {
    session.procExited = true;
    return { ok: false, error: 'Program finished', output: consumeSessionOutput(session) };
  }
  if (evt.event === 'error') {
    return { ok: false, error: evt.message || 'Runtime error', output: consumeSessionOutput(session) };
  }
  if (evt.event === 'eval') {
    if (evt.ok === 0) {
      return { ok: false, error: evt.error || 'Eval failed', output: consumeSessionOutput(session) };
    }
    const locals = normalizeDebuggerVars(evt.locals || {});
    session.locals = locals;
    return {
      ok: true,
      output: `${consumeSessionOutput(session)}${decodeMString(evt.output || '')}`,
      locals
    };
  }
  return { ok: false, error: 'Unexpected eval response' };
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

function hasActiveSshSession() {
  return Object.keys(sshSessions).length > 0;
}

function detectYottaDBPath(sshConn) {
  // Try to detect YottaDB installation by checking common paths
  const commonPaths = [
    '/usr/local/lib/yottadb/r138',
    '/usr/local/lib/yottadb/r136',
    '/usr/local/lib/yottadb/r134',
    '/usr/local/lib/yottadb/r132',
    '/opt/yottadb/current',
    '/opt/fis-gtm/YDB136',
    '/opt/fis-gtm/YDB138',
    '/usr/lib/fis-gtm/V6.3-011_x86_64',
    '/usr/lib/x86_64-linux-gnu/fis-gtm/V6.3-011_x86_64'
  ];

  // Try comprehensive search
  return new Promise((resolve) => {
    console.log('[SSH] Detecting YottaDB path...');

    // First try: which mumps or ydb
    sshConn.exec('which mumps 2>/dev/null || which ydb 2>/dev/null', (err, stream) => {
      if (err) {
        console.log('[SSH] which command failed, trying common paths');
        checkCommonPaths(0);
        return;
      }

      let output = '';
      stream.on('close', (code) => {
        const mumpsPath = output.trim();
        console.log(`[SSH] which result: "${mumpsPath}"`);
        if (mumpsPath && mumpsPath.includes('/') && !mumpsPath.includes('not found')) {
          // Extract directory from the full path
          const ydbPath = mumpsPath.substring(0, mumpsPath.lastIndexOf('/'));
          console.log(`[SSH] Found via which: ${ydbPath}`);
          resolve(ydbPath);
        } else {
          console.log('[SSH] which returned no valid path, trying common paths');
          checkCommonPaths(0);
        }
      }).on('data', (data) => {
        output += data.toString();
      });
    });

    function checkCommonPaths(index) {
      if (index >= commonPaths.length) {
        // Last resort: try to find mumps anywhere
        console.log('[SSH] Common paths exhausted, searching filesystem...');
        sshConn.exec('find /usr /opt -name mumps -type f 2>/dev/null | head -1', (err, stream) => {
          if (err) {
            console.log('[SSH] find command failed');
            resolve(null);
            return;
          }

          let output = '';
          stream.on('close', () => {
            const mumpsPath = output.trim();
            if (mumpsPath && mumpsPath.includes('/')) {
              const ydbPath = mumpsPath.substring(0, mumpsPath.lastIndexOf('/'));
              console.log(`[SSH] Found via find: ${ydbPath}`);
              resolve(ydbPath);
            } else {
              console.log('[SSH] No mumps executable found anywhere');
              resolve(null);
            }
          }).on('data', (data) => {
            output += data.toString();
          });
        });
        return;
      }

      const testPath = commonPaths[index];
      sshConn.exec(`test -f "${testPath}/mumps" && echo "found"`, (err, stream) => {
        if (err) {
          checkCommonPaths(index + 1);
          return;
        }

        let output = '';
        stream.on('close', () => {
          if (output.trim() === 'found') {
            console.log(`[SSH] Found at common path: ${testPath}`);
            resolve(testPath);
          } else {
            checkCommonPaths(index + 1);
          }
        }).on('data', (data) => {
          output += data.toString();
        });
      });
    }
  });
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

  // Universal mode fallback: use /workspace if no paths are configured
  if (dirs.length === 0) {
    dirs.push('/workspace');
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

  // Get YottaDB configuration
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;

  // Check if YottaDB is configured
  if (!cfg.ydbPath) {
    return {
      ok: false,
      error: 'YottaDB not configured. Please configure YottaDB path in Connections panel to run MUMPS code.'
    };
  }

  const routineFile = `${routinesPath}/${routineName}.m`;

  const lines = (command || '').split('\n').map(l => l.replace(/\r/g, ''));

  // Detect whether user provided their own labels (full routine) vs a bare snippet
  const commandTokens = new Set([
    'B', 'BREAK', 'C', 'CLOSE', 'D', 'DO', 'E', 'ELSE', 'F', 'FOR', 'G', 'GOTO',
    'H', 'HALT', 'HANG', 'I', 'IF', 'J', 'JOB', 'K', 'KILL', 'L', 'LOCK', 'M', 'MERGE',
    'N', 'NEW', 'O', 'OPEN', 'Q', 'QUIT', 'R', 'READ', 'S', 'SET', 'T', 'THEN', 'U', 'USE',
    'V', 'VIEW', 'W', 'WRITE', 'X', 'XECUTE', 'ZBREAK', 'ZB', 'ZGOTO', 'ZHALT', 'ZKILL', 'ZWRITE', 'ZW'
  ]);
  let firstLabel = null;
  const hasUserLabels = lines.some(raw => {
    const trimmed = raw.replace(/^\s+/, '');
    if (!trimmed || trimmed.startsWith(';')) return false;
    if (/^[\t ]/.test(raw)) return false; // commands are usually indented
    const token = trimmed.split(/[\s;(]/)[0] || '';
    const upper = token.toUpperCase();
    const isLabel = token && upper && !commandTokens.has(upper);
    if (isLabel && !firstLabel) firstLabel = token;
    return isLabel;
  });

  let routineSource = `${routineName} ; temp routine\n`;
  let entryCall = `D MAIN^${routineName}`;

  if (hasUserLabels) {
    // Preserve the user's labels and indentation; just prepend our temp routine name
    routineSource += 'QUIT\n';
    routineSource += lines.join('\n');
    if (!firstLabel) {
      // find label again to ensure we have one for entry call
      for (const raw of lines) {
        const trimmed = raw.replace(/^\s+/, '');
        if (!trimmed || trimmed.startsWith(';') || /^[\t ]/.test(raw)) continue;
        const token = trimmed.split(/[\s;(]/)[0] || '';
        if (token && !commandTokens.has(token.toUpperCase())) {
          firstLabel = token;
          break;
        }
      }
    }
    const entryLabel = (firstLabel || routineName).toUpperCase();
    entryCall = `D ${entryLabel}^${routineName}`;
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

  // ============================================================================
  // SAFETY: Do not strip QUIT values. If user writes QUIT 1, let it run.
  // Although DO calls normally error on return value (%YDB-E-NOTEXTRINSIC), 
  // we will suppress that specific error in the output if it occurs at the end.
  // ============================================================================

  // Add a safe QUIT to guarantee clean exit if not present
  const contentLines = routineSource.split('\n').filter(line => line.trim() && !/^\s*;/.test(line.trim()));
  const lastContent = (contentLines[contentLines.length - 1] || '').trim();

  // If the last line is not a QUIT, append one to be safe
  // (We don't strictly need to check for arguments anymore if we are tolerant)
  const hasQuit = /^Q(UIT)?(\s|;|$)/i.test(lastContent);

  if (!hasQuit) {
    if (!routineSource.endsWith('\n')) routineSource += '\n';
    routineSource += '\tQUIT\n';
  }
  // Always ensure the routine ends with a newline (required by M/MUMPS)
  if (!routineSource.endsWith('\n')) routineSource += '\n';


  const codeB64 = Buffer.from(routineSource, 'utf8').toString('base64');
  const cmdFile = '/tmp/ahmad_cmd.txt';

  // Use NOECHO to prevent command echoing in output
  const runCmdB64 = Buffer.from(`USE $P:(NOECHO) ${entryCall}\n`, 'utf8').toString('base64');

  const useDockerExec = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  if (useDockerExec) {
    // Build gtmroutines: include temp file location first
    const gtmroutines = routinesPath
      ? `${routinesPath}(${routinesPath}) ${cfg.ydbPath}/libgtmutil.so ${cfg.ydbPath}`
      : `${cfg.ydbPath}/libgtmutil.so ${cfg.ydbPath}`;

    // Use a default globals dir if not configured
    const gldPath = cfg.gldPath || '/tmp/mumps.gld';

    const writeCmd = wrapDockerCmd(
      `docker exec ${cfg.containerId} bash -lc "echo ${codeB64} | base64 -d > ${routineFile}"`
    );
    const writeRun = wrapDockerCmd(
      `docker exec ${cfg.containerId} bash -lc "echo ${runCmdB64} | base64 -d > ${cmdFile}"`
    );
    const runCmd = wrapDockerCmd(
      `docker exec ${cfg.containerId} bash -lc "export gtm_dist=${cfg.ydbPath} && export gtmgbldir=${gldPath} && export gtmroutines='${gtmroutines}' && export gtm_etrap='' && export gtm_ztrap='' && ${cfg.ydbPath}/mumps -direct < ${cmdFile} 2>&1; rm -f ${routineFile} ${cmdFile}"`
    );

    return new Promise((resolve) => {
      exec(`${writeCmd} && ${writeRun} && ${runCmd}`, { timeout: 30000, maxBuffer: 10 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
        resolve({ ok: true, stdout: cleanOutput(stdout || stderr) });
      });
    });
  } else {
    // SSH mode
    const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';

    // Build gtmroutines: include temp file location first
    const gtmroutines = routinesPath
      ? `${routinesPath}(${routinesPath}) ${cfg.ydbPath}/libgtmutil.so ${cfg.ydbPath}`
      : `${cfg.ydbPath}/libgtmutil.so ${cfg.ydbPath}`;

    // Use a default globals dir if not configured
    const gldPath = cfg.gldPath || '/tmp/mumps.gld';

    const writeCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "echo ${codeB64} | base64 -d > ${routineFile}"`;
    const writeRun = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "echo ${runCmdB64} | base64 -d > ${cmdFile}"`;
    const runCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "export gtm_dist=${cfg.ydbPath} && export gtmgbldir=${gldPath} && export gtmroutines='${gtmroutines}' && export gtm_etrap='' && export gtm_ztrap='' && ${cfg.ydbPath}/mumps -direct < ${cmdFile} 2>&1; rm -f ${routineFile} ${cmdFile}"`;

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
    logger.info('ROUTINE_LIST', { search, dirs: routineDirs });

    const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
    const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;
    const basePath = (cfg.basePath || '').trim();

    // List routines with folder prefix: "localr/ROUTINE", "routines/ROUTINE"
    const allRoutines = [];

    for (const dir of routineDirs) {
      // First, ensure the directory exists
      const mkdirCmd = `mkdir -p ${shellQuote(dir)}`;
      await runHostCommand(mkdirCmd);

      const cmd = `find ${dir} -name '*.m' -type f 2>/dev/null | sort`;
      const res = await runHostCommand(cmd);
      if (!res.ok) continue;

      const files = (res.stdout || '').split('\n').filter(Boolean);

      files.forEach(fullPath => {
        // Extract relative path from the base dir
        const relativePath = fullPath.replace(`${dir}/`, '');
        const routineName = path.basename(fullPath, '.m');

        // Determine folder name based on the directory structure
        let folderName;
        if (relativePath.includes('/')) {
          // File is in a subdirectory, use that as folder name
          folderName = relativePath.split('/')[0];
        } else if (dir.includes('/localr')) {
          folderName = 'localr';
        } else if (dir.includes('/routines')) {
          folderName = 'routines';
        } else {
          // Universal mode: no subfolder
          folderName = 'workspace';
        }

        const routineWithFolder = relativePath.includes('/')
          ? relativePath
          : `${folderName}/${routineName}.m`;

        if (!search || routineWithFolder.toLowerCase().includes(search.toLowerCase())) {
          allRoutines.push(routineWithFolder);
        }
      });
    }

    // Remove duplicates and sort
    const routines = Array.from(new Set(allRoutines)).sort();
    logger.info('ROUTINE_LIST_RESULT', { count: routines.length });

    return { ok: true, routines };
  },

  async readRoutine(name) {
    // Handle both "ROUTINE" and "localr/ROUTINE" or "routines/HELLO.m" formats
    let routine, targetFolder;
    if (name.includes('/')) {
      const parts = name.split('/');
      targetFolder = parts[0]; // "localr", "routines", or subfolder name
      // Handle both "folder/ROUTINE" and "folder/ROUTINE.m"
      routine = parts[1].endsWith('.m') ? parts[1].slice(0, -2) : parts[1];
      routine = sanitizeRoutineName(routine);
    } else {
      routine = sanitizeRoutineName(name);
    }

    if (!routine) {
      logger.warn('ROUTINE_READ_INVALID', { name });
      return { ok: false, error: 'Invalid routine name' };
    }
    logger.info('ROUTINE_READ', { routine, targetFolder, name });
    const routineDirs = getRoutineDirs();
    if (!routineDirs.length) return { ok: false, error: 'No routines path configured' };
    let lastError = '';

    // Try each directory with multiple path strategies
    for (const dir of routineDirs) {
      const pathsToTry = [];

      if (targetFolder) {
        // Strategy 1: dir contains targetFolder (e.g., "/var/.../localr" matches "localr")
        if (dir.includes(`/${targetFolder}`)) {
          pathsToTry.push(`${dir}/${routine}.m`);
        }
        // Strategy 2: targetFolder is a subdirectory (e.g., "/workspace" + "/routines/HELLO.m")
        pathsToTry.push(`${dir}/${targetFolder}/${routine}.m`);
      } else {
        pathsToTry.push(`${dir}/${routine}.m`);
      }

      for (const filePath of pathsToTry) {
        const cmd = `cat ${shellQuote(filePath)} 2>/dev/null || echo "___FILE_NOT_FOUND___"`;
        const res = await runHostCommand(cmd);
        if (res.ok && res.stdout && !res.stdout.includes('___FILE_NOT_FOUND___')) {
          logger.info('ROUTINE_READ_OK', { routine, folder: targetFolder || 'root', filePath });
          return { ok: true, code: res.stdout };
        }
      }
      lastError = `File not found in any expected location`;
    }

    logger.warn('ROUTINE_READ_FAIL', { routine, targetFolder, lastError });
    return { ok: false, error: lastError || 'Routine not found' };
  },

  async saveRoutine(name, code) {
    // Handle both "ROUTINE", "localr/ROUTINE", and "routines/HELLO.m" formats
    let routine, targetFolder;
    if (name.includes('/')) {
      const parts = name.split('/');
      targetFolder = parts[0].toLowerCase(); // normalize to lowercase
      // Handle both "folder/ROUTINE" and "folder/ROUTINE.m"
      routine = parts[1].endsWith('.m') ? parts[1].slice(0, -2) : parts[1];
      routine = sanitizeRoutineName(routine);
    } else {
      // Handle "ROUTINE" or "ROUTINE.m"
      const nameWithoutExt = name.endsWith('.m') ? name.slice(0, -2) : name;
      routine = sanitizeRoutineName(nameWithoutExt);
    }

    if (!routine) {
      logger.warn('ROUTINE_SAVE_INVALID', { name });
      return { ok: false, error: 'Invalid routine name' };
    }
    if (!code) return { ok: false, error: 'Code is empty' };
    logger.info('ROUTINE_SAVE', { routine, targetFolder, name });
    const routineDirs = getRoutineDirs();

    // If folder specified, save to that folder; otherwise use first dir
    let routinesPath;
    if (targetFolder) {
      routinesPath = routineDirs.find(d => d.toLowerCase().includes(`/${targetFolder}`));
      // If not found, create it under the first routines dir
      if (!routinesPath) {
        routinesPath = `${routineDirs[0]}/${targetFolder}`;
      }
    } else {
      routinesPath = routineDirs[0];
    }

    if (!routinesPath) return { ok: false, error: 'No routines path configured' };

    // Use writeRemoteFile which creates directories automatically
    const filePath = `${routinesPath}/${routine}.m`;
    logger.info('ROUTINE_SAVE_PATH', { filePath });
    const res = await writeRemoteFile(filePath, code);
    if (!res.ok) {
      logger.error('ROUTINE_SAVE_FAIL', { routine, error: res.error || res.stderr });
      return { ok: false, error: res.error || res.stderr || 'Failed to save routine' };
    }

    const folder = routinesPath.includes('/localr') ? 'localr' : (targetFolder || 'workspace');
    logger.info('ROUTINE_SAVE_OK', { routine, folder, filePath });
    return { ok: true, routine, folder };
  },

  async searchRoutines(term, opts = {}) {
    const routineDirs = getRoutineDirs();
    if (!routineDirs.length) return { ok: false, error: 'No routines path configured' };
    logger.info('ROUTINE_SEARCH', { term, opts, dirs: routineDirs });
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
      logger.error('ROUTINE_SEARCH_ERROR', { error: res.error || res.stderr });
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

    logger.info('ROUTINE_SEARCH_RESULT', { count: hits.length });
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
    try {
      const validStartLine = (Number.isInteger(startLine) && startLine > 0) ? startLine : null;
      dbgLog(`[DEBUG] debugStart called. USE_ZSTEP_ENGINE=${USE_ZSTEP_ENGINE}, startLine=${startLine}, validStartLine=${validStartLine}`);
      dbgLog('[adapter] debugStart', { USE_ZSTEP_ENGINE, startLine, validStartLine, bpCount: breakpoints?.length });

      if (USE_ZSTEP_ENGINE) {
        dbgLog('[DEBUG] Starting ZSTEP engine...');
        dbgLog('[adapter] Starting ZSTEP engine', { startLine: validStartLine });
        const zres = await startZStepSession(code, breakpoints, validStartLine);
        if (!zres.ok) {
          dbgLog(`[DEBUG] ZSTEP engine failed: ${zres.error}`);
          dbgLog('[adapter] ZSTEP engine failed', { error: zres.error });
          return { ok: false, error: zres.error || 'zstep engine unavailable', output: zres.output || '' };
        }
        dbgLog('[DEBUG] ZSTEP engine started successfully');
        dbgLog('[adapter] ZSTEP engine started', { sessionId: zres.sessionId, currentLine: zres.currentLine });
        dbgLog('[DEBUG] startZStepSession returned zres:', JSON.stringify(zres, null, 2));
        return {
          ok: true,
          sessionId: zres.sessionId,
          currentLine: zres.currentLine,
          currentRoutine: zres.currentRoutine || 'TMPDBG',
          currentTag: zres.currentTag,
          locals: zres.locals || {},
          callStack: zres.callStack || [],
          stack: zres.stack || formatCallStackForClient(zres.callStack || []),
          output: zres.output || '',
          engine: 'zstep',
          ready: zres.ready || false  // CRITICAL FIX: Include ready flag
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
        breakpoints: (breakpoints || []).map(b => (b && typeof b === 'object' ? b.line : b)),
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
    } catch (err) {
      dbgLog('[DEBUG] CRITICAL ERROR caused by debugStart:', err);
      return { ok: false, error: 'Internal debugger error: ' + err.message };
    }
  },

  async debugStep(sessionId, stepType = 'into') {
    const session = debugSessions[sessionId];
    if (!session) return { ok: false, error: 'Session not found' };

    if (session.engine === 'zstep') {
      const cmdMap = { into: 'INTO', over: 'OVER', out: 'OUTOF' };
      // Simplified: Let ZSTEP engine handle the stepping natively.
      // Trying to manually parse and set breakpoints on DO/$$ logic was causing NOPLACE errors.
      return sendZStepCommand(sessionId, cmdMap[stepType]);
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

  async debugEval(sessionId, code) {
    const session = debugSessions[sessionId];
    if (!session) return { ok: false, error: 'Session not found' };
    if (session.engine === 'zstep') {
      return sendZStepEval(sessionId, code);
    }
    return { ok: false, error: 'Eval not supported for this engine' };
  },

  async debugStop(sessionId) {
    const session = debugSessions[sessionId];
    if (session && session.engine === 'zstep') {
      try {
        // Send HALT command to allow clean exit and capture final output
        if (session.proc && !session.procExited && session.proc.stdin.writable) {
          session.proc.stdin.write('HALT\n');
          // Wait a bit for process to exit cleanly
          await new Promise(r => setTimeout(r, 300));
        }
        // Force kill if still running
        if (session.proc && !session.procExited) {
          session.proc.kill('SIGTERM');
          await new Promise(r => setTimeout(r, 150));
          if (!session.procExited) {
            session.proc.kill('SIGKILL');
          }
        }
      } catch (e) {
        dbgLog('[DEBUG] Error during stop:', e.message);
      }
      // Return accumulated output to show in terminal
      const output = consumeSessionOutput(session);
      session.procExited = true;
      delete debugSessions[sessionId];
      return { ok: true, output };
    }
    delete debugSessions[sessionId];
    return { ok: true };
  },


  async debugStartMdebug(routineName, breakpoints = [], options = {}) {
    return {
      ok: false,
      error: 'MDEBUG TCP engine is disabled in this build. Use debugStart (zstep / JSON engine) instead.'
    };
  },


  async debugStepMdebug(sessionId, stepType = 'over') {
    return {
      ok: false,
      error: 'MDEBUG TCP engine is disabled. Use debugStep with a zstep/JSON session.'
    };
  },

  async debugContinueMdebug(sessionId) {
    return {
      ok: false,
      error: 'MDEBUG TCP engine is disabled. Use debugContinue with a zstep/JSON session.'
    };
  },

  async debugStopMdebug(sessionId) {
    // nothing to clean up (no TCP session)
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

      conn.on('ready', async () => {
        // Store connection details for execute/ debug over SSH
        connectionConfig.type = 'ssh';
        connectionConfig.ssh = mergeSshConfig(config || {});
        sshSessions[id] = conn;

        // Auto-detect YottaDB path if not provided or if default path doesn't exist
        if (!config.ydbPath || config.ydbPath === '/opt/fis-gtm/YDB136') {
          const detectedPath = await detectYottaDBPath(conn);
          if (detectedPath) {
            connectionConfig.ssh.ydbPath = detectedPath;
            console.log(`[SSH] Auto-detected YottaDB at: ${detectedPath}`);
          } else {
            console.warn('[SSH] Warning: Could not auto-detect YottaDB path. Please configure it in Connection settings.');
          }
        }

        resolve({ ok: true, sessionId: id, ydbPath: connectionConfig.ssh.ydbPath });
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
  },

  /**
   * Create a directory in the current environment (Docker or SSH).
   * Works in both universal mode (no YottaDB) and configured mode.
   * @param {string} dirPath - The directory path to create (relative or absolute)
   * @returns {Promise<{ok: boolean, error?: string}>}
   */
  async createDirectoryInCurrentEnv(dirPath) {
    if (!dirPath || typeof dirPath !== 'string') {
      return { ok: false, error: 'Directory path is required' };
    }

    const cfg = connectionConfig.type === 'ssh' ? connectionConfig.ssh : connectionConfig.docker;
    const isDocker = connectionConfig.type === 'docker';
    const mkdirCmd = `mkdir -p ${shellQuote(dirPath)}`;

    return new Promise((resolve) => {
      try {
        if (isDocker) {
          if (!cfg.containerId) {
            return resolve({ ok: false, error: 'No Docker container selected' });
          }
          const dockerCmd = wrapDockerCmd(`docker exec ${cfg.containerId} bash -c ${shellQuote(mkdirCmd)}`);
          exec(dockerCmd, { timeout: 8000 }, (err, stdout, stderr) => {
            if (err) {
              resolve({ ok: false, error: err.message || stderr || 'Failed to create directory' });
            } else {
              resolve({ ok: true });
            }
          });
        } else {
          // SSH mode
          if (!cfg.host || !cfg.username) {
            return resolve({ ok: false, error: 'SSH not connected' });
          }
          const sshPass = cfg.password ? `sshpass -p '${cfg.password}' ` : '';
          const sshCmd = `${sshPass}ssh -o StrictHostKeyChecking=no -p ${cfg.port || 22} ${cfg.username}@${cfg.host} ${shellQuote(mkdirCmd)}`;
          exec(sshCmd, { timeout: 8000 }, (err, stdout, stderr) => {
            if (err) {
              resolve({ ok: false, error: err.message || stderr || 'Failed to create directory' });
            } else {
              resolve({ ok: true });
            }
          });
        }
      } catch (err) {
        resolve({ ok: false, error: err.message || 'Failed to create directory' });
      }
    });
  }
};
