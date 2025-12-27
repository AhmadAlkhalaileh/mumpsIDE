const { exec } = require('child_process');
const { connectionConfig } = require('../config/connectionConfig');
const { hasActiveSshSession } = require('../state/sessions');
const { wrapDockerCmd } = require('../util/process');
const { discoverGldPath } = require('./discoverGldPath');
const { cleanOutput } = require('../util/ydb');
const { getRoutineDirs } = require('../routines/fetchRoutineDirectoriesToLocal');
const { discoverVistaProfilePaths, applyVistaProfilePathsToConfig } = require('../vista/vistaProfilePaths');

async function ensureVistaProfilePathsDiscovered() {
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;

  const needsAny = !String(cfg?.gldPath || '').trim()
    || (!String(cfg?.routinesPath || '').trim() && !String(cfg?.rpcRoutinesPath || '').trim());
  if (!needsAny) return;

  if (useDocker) {
    if (!cfg?.containerId) return;
  } else {
    if (!cfg?.host || !cfg?.username) return;
  }

  try {
    const discovered = await discoverVistaProfilePaths({ timeoutMs: 4000 });
    applyVistaProfilePathsToConfig(cfg, discovered, { override: true });
  } catch (_) { }
}

// Execute code directly in MUMPS without wrapping in a routine (for debugging)
async function executeYDBDirect(command) {
  const cmdFile = '/tmp/ahmad_debug_cmd.txt';
  await ensureVistaProfilePathsDiscovered();

  // Commands are executed line-by-line in direct mode
  // No routine wrapper, so variables persist in the same session
  const commandsB64 = Buffer.from(command, 'utf8').toString('base64');

  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  if (useDocker) {
    const cfg = connectionConfig.docker;

    // Check if YottaDB and global directory are configured
    if (!cfg.ydbPath) {
      return {
        ok: false,
        error: 'YottaDB not configured. Please configure YottaDB path in Connections panel.'
      };
    }
    if (!cfg.gldPath) {
      return {
        ok: false,
        error: 'Global directory not configured. Direct Mode requires a YottaDB global directory. Please configure it in Connections panel.'
      };
    }

    // Verify global directory file exists
    const checkGldCmd = wrapDockerCmd(
      `docker exec ${cfg.containerId} test -f ${cfg.gldPath}`
    );
    const gldCheckResult = await new Promise((resolve) => {
      exec(checkGldCmd, { timeout: 5000 }, (err) => {
        resolve({ exists: !err });
      });
    });

    if (!gldCheckResult.exists) {
      return {
        ok: false,
        error: `Global directory file not found: ${cfg.gldPath}. Direct Mode requires a valid YottaDB global directory file. Please ensure the file exists or reconfigure in Connections panel.`
      };
    }

    const writeCmd = wrapDockerCmd(
      `docker exec ${cfg.containerId} bash -lc "echo ${commandsB64} | base64 -d > ${cmdFile}"`
    );
    const runCmd = wrapDockerCmd(
      `docker exec ${cfg.containerId} bash -lc "export gtm_dist=${cfg.ydbPath} && export gtmgbldir=${cfg.gldPath} && export gtmroutines='${(cfg.rpcRoutinesPath || cfg.routinesPath)}(${(cfg.rpcRoutinesPath || cfg.routinesPath)}) ${cfg.ydbPath}/libgtmutil.so ${cfg.ydbPath}' && export gtm_etrap='' && export gtm_ztrap='' && ${cfg.ydbPath}/mumps -direct < ${cmdFile} 2>&1; rm -f ${cmdFile}"`
    );

    return new Promise((resolve) => {
      exec(`${writeCmd} && ${runCmd}`, { timeout: 30000, maxBuffer: 10 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) {
          const output = stdout || stderr || err.message || '';
          // Detect ZGBLDIRACC error and provide helpful message
          if (/ZGBLDIRACC|Cannot access global directory/i.test(output)) {
            return resolve({
              ok: false,
              error: `Global directory file not accessible: ${cfg.gldPath}. The file may not exist or permissions are incorrect. Please check your YottaDB configuration in the Connections panel.`,
              stdout,
              stderr
            });
          }
          return resolve({ ok: false, error: err.message, stdout, stderr });
        }
        resolve({ ok: true, stdout: cleanOutput(stdout || stderr) });
      });
    });
  } else {
    const cfg = connectionConfig.ssh;

    // Check if YottaDB and global directory are configured
    if (!cfg.ydbPath) {
      return {
        ok: false,
        error: 'YottaDB not configured. Please configure YottaDB path in Connections panel.'
      };
    }
    if (!cfg.gldPath) {
      return {
        ok: false,
        error: 'Global directory not configured. Direct Mode requires a YottaDB global directory. Please configure it in Connections panel.'
      };
    }

    // Verify global directory file exists
    const sshPass = cfg.password ? `sshpass -p '${cfg.password}'` : '';
    const checkGldCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "test -f ${cfg.gldPath}"`;
    const gldCheckResult = await new Promise((resolve) => {
      exec(checkGldCmd, { timeout: 5000 }, (err) => {
        resolve({ exists: !err });
      });
    });

    if (!gldCheckResult.exists) {
      return {
        ok: false,
        error: `Global directory file not found: ${cfg.gldPath}. Direct Mode requires a valid YottaDB global directory file. Please ensure the file exists or reconfigure in Connections panel.`
      };
    }

    const writeCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "echo ${commandsB64} | base64 -d > ${cmdFile}"`;
    const runCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "export gtm_dist=${cfg.ydbPath} && export gtmgbldir=${cfg.gldPath} && export gtmroutines='${(cfg.rpcRoutinesPath || cfg.routinesPath)}(${(cfg.rpcRoutinesPath || cfg.routinesPath)}) ${cfg.ydbPath}/libgtmutil.so ${cfg.ydbPath}' && export gtm_etrap='' && export gtm_ztrap='' && ${cfg.ydbPath}/mumps -direct < ${cmdFile} 2>&1; rm -f ${cmdFile}"`;

    return new Promise((resolve) => {
      exec(`${writeCmd} && ${runCmd}`, { timeout: 30000, maxBuffer: 10 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) {
          const output = stdout || stderr || err.message || '';
          // Detect ZGBLDIRACC error and provide helpful message
          if (/ZGBLDIRACC|Cannot access global directory/i.test(output)) {
            return resolve({
              ok: false,
              error: `Global directory file not accessible: ${cfg.gldPath}. The file may not exist or permissions are incorrect. Please check your YottaDB configuration in the Connections panel.`,
              stdout,
              stderr
            });
          }
          return resolve({ ok: false, error: err.message, stdout, stderr });
        }
        resolve({ ok: true, stdout: cleanOutput(stdout || stderr) });
      });
    });
  }
}

async function executeYDB(command) {
  await ensureVistaProfilePathsDiscovered();
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

    // Prefer configured gldPath; otherwise attempt discovery (vista-profile / find).
    const gldPath = cfg.gldPath || await discoverGldPath();
    const gldExport = gldPath ? `export gtmgbldir=${gldPath} && ` : '';

    const writeCmd = wrapDockerCmd(
      `docker exec ${cfg.containerId} bash -lc "echo ${codeB64} | base64 -d > ${routineFile}"`
    );
    const writeRun = wrapDockerCmd(
      `docker exec ${cfg.containerId} bash -lc "echo ${runCmdB64} | base64 -d > ${cmdFile}"`
    );
    const runCmd = wrapDockerCmd(
      `docker exec ${cfg.containerId} bash -lc "export gtm_dist=${cfg.ydbPath} && ${gldExport}export gtmroutines='${gtmroutines}' && export gtm_etrap='' && export gtm_ztrap='' && ${cfg.ydbPath}/mumps -direct < ${cmdFile} 2>&1; rm -f ${routineFile} ${cmdFile}"`
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

    // Prefer configured gldPath; otherwise attempt discovery (vista-profile / find).
    const gldPath = cfg.gldPath || await discoverGldPath();
    const gldExport = gldPath ? `export gtmgbldir=${gldPath} && ` : '';

    const writeCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "echo ${codeB64} | base64 -d > ${routineFile}"`;
    const writeRun = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "echo ${runCmdB64} | base64 -d > ${cmdFile}"`;
    const runCmd = `${sshPass} ssh -o StrictHostKeyChecking=no -p ${cfg.port} ${cfg.username}@${cfg.host} "export gtm_dist=${cfg.ydbPath} && ${gldExport}export gtmroutines='${gtmroutines}' && export gtm_etrap='' && export gtm_ztrap='' && ${cfg.ydbPath}/mumps -direct < ${cmdFile} 2>&1; rm -f ${routineFile} ${cmdFile}"`;

    return new Promise((resolve) => {
      exec(`${writeCmd} && ${writeRun} && ${runCmd}`, { timeout: 30000, maxBuffer: 10 * 1024 * 1024 }, (err, stdout, stderr) => {
        if (err) return resolve({ ok: false, error: err.message, stdout, stderr });
        resolve({ ok: true, stdout: cleanOutput(stdout || stderr) });
      });
    });
  }
}

module.exports = {
  executeYDBDirect,
  executeYDB
};
