const fs = require('fs');
const path = require('path');

const { fetchRoutineDirectoriesToLocal } = require('../routines/fetchRoutineDirectoriesToLocal');

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
};

