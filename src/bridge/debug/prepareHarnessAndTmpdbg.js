async function prepareHarnessAndTmpdbg({
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
}) {
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

  return { ok: true };
}

module.exports = { prepareHarnessAndTmpdbg };
