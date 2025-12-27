const path = require('path');
const { logger } = require('../../../utils/logger');
const { shellQuote } = require('../config/paths');
const { connectionConfig } = require('../config/connectionConfig');
const { hasActiveSshSession } = require('../state/sessions');
const { runHostCommand } = require('../util/runHostCommand');
const { writeRemoteFile } = require('../util/writeRemoteFile');
const { sanitizeRoutineName } = require('../util/ydb');
const { getRoutineDirs } = require('./fetchRoutineDirectoriesToLocal');
const { executeYDB } = require('../ydb/executeYDB');
const { discoverVistaProfilePaths, applyVistaProfilePathsToConfig } = require('../vista/vistaProfilePaths');

async function ensureVistaPathsDiscovered() {
  const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
  const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;

  const hasConfiguredDirs = !!(String(cfg?.routinesPath || '').trim() || String(cfg?.rpcRoutinesPath || '').trim());
  console.log('[ensureVistaPathsDiscovered] hasConfiguredDirs:', hasConfiguredDirs, 'routinesPath:', cfg?.routinesPath, 'rpcRoutinesPath:', cfg?.rpcRoutinesPath);
  if (hasConfiguredDirs) return;

  if (useDocker) {
    if (!cfg?.containerId) {
      console.log('[ensureVistaPathsDiscovered] No Docker container ID');
      return;
    }
  } else {
    if (!cfg?.host || !cfg?.username) {
      console.log('[ensureVistaPathsDiscovered] No SSH host/username');
      return;
    }
  }

  try {
    console.log('[ensureVistaPathsDiscovered] Calling discoverVistaProfilePaths...');
    const discovered = await discoverVistaProfilePaths({ timeoutMs: 4000 });
    console.log('[ensureVistaPathsDiscovered] Discovery result:', discovered);
    applyVistaProfilePathsToConfig(cfg, discovered, { override: true });
    console.log('[ensureVistaPathsDiscovered] Applied config:', { routinesPath: cfg?.routinesPath, rpcRoutinesPath: cfg?.rpcRoutinesPath });
  } catch (err) {
    console.error('[ensureVistaPathsDiscovered] Error:', err);
  }
}

module.exports = {
  async listRoutines(search = '') {
    console.log('[Routines API] listRoutines called, ensuring paths are discovered...');
    await ensureVistaPathsDiscovered();
    const routineDirs = getRoutineDirs();
    console.log('[Routines API] Discovered routine dirs:', routineDirs);
    if (!routineDirs.length) {
      console.log('[Routines API] No routine dirs configured');
      return { ok: false, error: 'No routines path configured' };
    }
    logger.info('ROUTINE_LIST', { search, dirs: routineDirs });

    const useDocker = connectionConfig.type !== 'ssh' || !hasActiveSshSession();
    const cfg = useDocker ? connectionConfig.docker : connectionConfig.ssh;
    console.log('[Routines API] Using config:', { type: connectionConfig.type, routinesPath: cfg?.routinesPath, rpcRoutinesPath: cfg?.rpcRoutinesPath });
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
    await ensureVistaPathsDiscovered();
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
    await ensureVistaPathsDiscovered();
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
    await ensureVistaPathsDiscovered();
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
    await ensureVistaPathsDiscovered();
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
  }
};
