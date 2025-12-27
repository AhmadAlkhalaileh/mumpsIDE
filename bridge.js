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
const sshClient = require('./src/bridge/ssh/sshClient');
const { ensureSshClient } = sshClient;

const { mergeSshConfig, mergeDockerConfig, setConnectionConfig } = require('./src/bridge/config/mergeConnectionConfig');

const { connectionConfig } = require('./src/bridge/config/connectionConfig');
setConnectionConfig(connectionConfig);

const { debugSessions, sshSessions, hasActiveSshSession } = require('./src/bridge/state/sessions');
// Default to the richer zstep engine unless explicitly disabled
const USE_ZSTEP_ENGINE = process.env.AHMAD_IDE_DEBUG_ENGINE !== 'legacy';
const { sourceMapCache, loadRoutineSourceMap } = require('./src/bridge/debug/sourceMaps');

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

function wrapDockerCmd(cmd) {
  if (process.env.AHMAD_IDE_USE_SG === '1') {
    return `sg docker -c "${cmd.replace(/"/g, '\\"')}"`;
  }
  // In snap, docker is bundled at $SNAP/usr/bin/docker
  const dockerPath = process.env.SNAP ? `${process.env.SNAP}/usr/bin/docker` : 'docker';
  return cmd.replace(/^docker\s/, `${dockerPath} `);
}

const { discoverGldPath } = require('./src/bridge/ydb/discoverGldPath');

const { runHostCommand } = require('./src/bridge/util/runHostCommand');

const { resolveGitBinary, rewriteGitCommand, runLocalGitCommand } = require('./src/bridge/git/localGit');

const { isSkippableDebugLine, advanceToNextExecutableLine } = require('./src/bridge/debug/legacyStepping');

// ---------------------- Source map helpers (zstep) ----------------------

const { buildSourceMapFromCode, nextExecutableLine, parseZPos, formatCallStackForClient, payloadLineToUserLine } = require('./src/bridge/debug/sourceMapUtils');

// ---------------------- ZSTEP (external harness) ----------------------

// Robust file write using stdin to avoid command-line length limits (ARG_MAX)
const { writeRemoteFile } = require('./src/bridge/util/writeRemoteFile');

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

const { resolvePending, pullQueuedEvent, tryParseDebuggerEvent, waitForEvent, waitForZStepEvent, handleZStepStdout, consumeSessionOutput } = require('./src/bridge/debug/zstepEventQueue');

const { applyZStepEvent } = require('./src/bridge/debug/applyZStepEvent');


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

  const { sanitizeDebugCode, addGuardQuits } = require('./src/bridge/debug/userCodeTransforms');

  const safeUserCode = sanitizeDebugCode(code);
  const { code: guardedUserCode, transformedToUser, userToTransformed } = addGuardQuits(safeUserCode);

  const id = `dbg_${Date.now()}_${Math.random().toString(16).slice(2)}`;


  const { buildTmpdbgPayload } = require('./src/bridge/debug/buildTmpdbgPayload');
  const { firstTag, hasParams, codePayload, headerLines, hasStartWrapper } = buildTmpdbgPayload(safeUserCode, guardedUserCode, dbgLog);

  // Build source map from the full payload (not just user code) so line numbers match
  const { map, lines } = buildSourceMapFromCode('TMPDBG', codePayload);
  sourceMapCache['TMPDBG'] = { map, lines };

  // Map payload lines back to user lines (header lines map to 0)
  const payloadToUser = [];
  for (let i = 0; i < headerLines; i += 1) payloadToUser.push(0);
  transformedToUser.forEach((uLine) => payloadToUser.push(uLine));

  const { selectEntryTag } = require('./src/bridge/debug/selectEntryTag');
  const entryTag = selectEntryTag({ startLine, headerLines, userToTransformed, map, lines, nextExecutableLine, dbgLog, hasStartWrapper, firstTag, hasParams });

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

  const { prepareHarnessAndTmpdbg } = require('./src/bridge/debug/prepareHarnessAndTmpdbg');
  const prepRes = await prepareHarnessAndTmpdbg({
    useDocker,
    cfg,
    ensureHarness,
    buildYdbEnv,
    wrapDockerCmd,
    exec,
    runHostCommand,
    summarizeCompileError,
    writeRemoteFile,
    codePayload,
    lines,
    dbgLog
  });
  if (!prepRes.ok) return prepRes;

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
  const { createTagOffsetForLine } = require('./src/bridge/debug/tagOffsetForLine');
  const tagOffsetForLine = createTagOffsetForLine(map, lines, dbgLog);

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
const { decodeMString, normalizeDebuggerVars } = require('./src/bridge/debug/vars');

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
const { fetchZStepVariables, sendZStepCommand, sendZStepEval } = require('./src/bridge/debug/zstepCommands');

const { detectGitRepo, readGitConfig } = require('./src/bridge/git/repo');

const { sanitizeRoutineName, cleanOutput } = require('./src/bridge/util/ydb');

const { detectYottaDBPath } = require('./src/bridge/ssh/detectYottaDBPath');

const { getRoutineDirs, fetchRoutineDirectoriesToLocal } = require('./src/bridge/routines/fetchRoutineDirectoriesToLocal');

const { executeYDBDirect, executeYDB } = require('./src/bridge/ydb/executeYDB');

const { lint, execute, hostExec, git, detectGitRepo: detectGitRepoApi, getGitConfig } = require('./src/bridge/api/coreApi');

const { debugStep } = require('./src/bridge/debug/debugStep');

const { debugContinue, debugEval, debugStop } = require('./src/bridge/debug/debugControls');

const { setConnection, getConnection, sshConnect, sshExec, sshDisconnect, createDirectoryInCurrentEnv } = require('./src/bridge/connection/connectionApi');

const { createProject, openProject } = require('./src/bridge/projects/projectApi');

const { listRoutines, readRoutine, saveRoutine, searchRoutines, zlinkRoutine } = require('./src/bridge/routines/routinesApi');

module.exports = {
  createProject,

  openProject,

  lint,

  execute,

  listRoutines,

  readRoutine,

  saveRoutine,

  searchRoutines,

  zlinkRoutine,

  hostExec,

  git,

  detectGitRepo: detectGitRepoApi,

  getGitConfig,

  setConnection,

  getConnection,

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

  debugStep,

  debugContinue,

  debugEval,

  debugStop,


  sshConnect,

  sshExec,

  sshDisconnect,

  createDirectoryInCurrentEnv
};
