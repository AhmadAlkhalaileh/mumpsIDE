// MUMPS bridge using local runtime (YottaDB/GT.M) via Docker or SSH (mirrors web backend defaults)
const { spawn, exec } = require('child_process');
const fs = require('fs');
const path = require('path');
const { Client: SSHClient } = require('ssh2');

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

  const lines = (command || '').split('\n');
  let routineSource = `${routineName} ; temp routine\nMAIN ; entry\n`;
  lines.forEach(l => {
    const trimmed = l.replace(/^\s+/, '');
    if (trimmed.length === 0) {
      routineSource += '\t;\n';
    } else if (/^\S/.test(l)) {
      // Line had no leading whitespace; treat as comment to avoid label/command errors inside MAIN
      routineSource += `\t; ${trimmed}\n`;
    } else if (trimmed.startsWith(';')) {
      routineSource += `\t${trimmed}\n`;
    } else {
      routineSource += `\t${trimmed}\n`;
    }
  });
  if (!/^\s*QUIT\b/i.test((lines[lines.length - 1] || '').trim())) {
    routineSource += '\tQUIT\n';
  }

  const codeB64 = Buffer.from(routineSource, 'utf8').toString('base64');
  const cmdFile = '/tmp/ahmad_cmd.txt';
  const runCmdB64 = Buffer.from(`D MAIN^${routineName}\n`, 'utf8').toString('base64');

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
  async debugStart(code, breakpoints = []) {
    const id = `dbg_${Date.now()}_${Math.random().toString(16).slice(2)}`;
    // Preserve leading whitespace so M commands keep their indentation (needed for execution)
    // but still drop empty/comment-only lines.
    const lines = code
      .split('\n')
      .map(l => l.replace(/\r/g, ''))
      .filter(l => l.trim() && !l.trimStart().startsWith(';'));

    debugSessions[id] = {
      code,
      lines,
      currentLine: 1,
      breakpoints,
      locals: {},
      stack: ['MAIN^TMP'],
      output: []
    };

    return {
      ok: true,
      sessionId: id,
      currentLine: 1,
      locals: {},
      stack: ['MAIN^TMP'],
      output: ''
    };
  },

  async debugStep(sessionId, stepType = 'into') {
    const session = debugSessions[sessionId];
    if (!session) return { ok: false, error: 'Session not found' };

    // Check if we've reached the end
    if (session.currentLine > session.lines.length) {
      return { ok: false, error: 'End of code reached' };
    }

    // IMPORTANT: Execute ALL lines from start to current line to preserve variables
    // This way variables persist across steps
    const linesToExecute = session.lines.slice(0, session.currentLine);

    // Build execution code: all lines up to current + capture variables
    // Strip labels while keeping inline commands so direct mode can run them.
    const normalizedLines = linesToExecute
      .map((line) => {
        if (!line) return '';
        // If the line starts with a label (no leading whitespace), preserve the command part
        if (/^\S/.test(line)) {
          const parts = line.split(/\s+/);
          if (parts.length > 1) {
            // Drop the label and keep the rest of the commands
            return parts.slice(1).join(' ');
          }
          // Label-only line – skip
          return '';
        }
        return line.trimStart();
      })
      .filter(Boolean);

    let codeToExecute = normalizedLines.join('\n');

    // Add debug output markers
    codeToExecute += '\nWRITE "<<<DEBUG_VARS_START>>>",!\n';
    codeToExecute += 'ZWRITE\n';  // This will output all variables
    codeToExecute += 'WRITE "<<<DEBUG_VARS_END>>>",!\n';

    // Execute using Docker/SSH direct mode to keep locals accurate
    const result = await executeYDBDirect(codeToExecute);

    if (!result.ok) {
      return { ok: false, error: result.error || 'Execution failed' };
    }

    // Parse output to get variables and execution output
    const output = result.stdout || result.output || '';
    const lines = output.split('\n');
    const locals = {};
    const execOutput = [];

    for (const line of lines) {
      const trimmedLine = line.trim();
      if (!trimmedLine) continue;

      // Parse ZWRITE output - handles both simple vars (X=10) and arrays (ARRAY(1,2)=10)
      // Match: VARNAME=value or VARNAME(subscripts)=value
      // More flexible regex to handle various MUMPS output formats
      const match = trimmedLine.match(/^([A-Za-z%][A-Za-z0-9]*)(\([^)]+\))?\s*=\s*(.*)$/);
      if (match) {
        const varName = match[1].toUpperCase();  // MUMPS vars are case-insensitive, store uppercase
        const subscript = match[2]; // Subscript if present (e.g., "(1,2)")
        let value = match[3].trim();

        // Remove surrounding quotes if present
        if ((value.startsWith('"') && value.endsWith('"')) ||
            (value.startsWith("'") && value.endsWith("'"))) {
          value = value.slice(1, -1);
        }

        if (subscript) {
          // Array element - group by array name
          if (!locals[varName]) {
            locals[varName] = { _isArray: true, _elements: {} };
          }
          locals[varName]._elements[subscript] = value;
        } else {
          // Simple variable
          locals[varName] = value;
        }
      } else if (
        !trimmedLine.includes('ZWRITE') &&
        !trimmedLine.includes('<<<DEBUG_VARS_START>>>') &&
        !trimmedLine.includes('<<<DEBUG_VARS_END>>>') &&
        !trimmedLine.includes('QUIT') &&
        !trimmedLine.startsWith('>')
      ) {
        // Don't include ZWRITE command, QUIT command, or MUMPS prompt in output
        execOutput.push(line);
      }
    }

    // Move to next line
    session.currentLine += 1;
    session.locals = locals;
    session.output.push(...execOutput);

    const returnValue = {
      ok: true,
      currentLine: session.currentLine,
      locals: locals,  // Return the newly parsed locals, not session.locals
      stack: session.stack,
      output: execOutput.join('\n')
    };

    return returnValue;
  },

  async debugContinue(sessionId) {
    const session = debugSessions[sessionId];
    if (!session) return { ok: false, error: 'Session not found' };

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
      locals: session.locals,
      stack: session.stack,
      output: session.output.join('\n')
    };
  },

  async debugStop(sessionId) {
    delete debugSessions[sessionId];
    return { ok: true };
  },

  async sshConnect(config) {
    return new Promise((resolve) => {
      const conn = new SSHClient();
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
