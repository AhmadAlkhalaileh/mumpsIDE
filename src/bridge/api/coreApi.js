const { executeYDB } = require('../ydb/executeYDB');
const { runHostCommand } = require('../util/runHostCommand');
const { runLocalGitCommand } = require('../git/localGit');
const { detectGitRepo, readGitConfig } = require('../git/repo');

module.exports = {
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

  async detectGitRepo(projectRoot, opts = {}) {
    return detectGitRepo(projectRoot, opts);
  },

  getGitConfig(projectPath) {
    return readGitConfig(projectPath);
  },
};

