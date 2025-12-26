const fs = require('fs');
const path = require('path');
const { runLocalGitCommand } = require('./localGit');

async function detectGitRepo(projectRoot, opts = {}) {
  try {
    const res = await runLocalGitCommand(`git -C "${String(projectRoot).replace(/"/g, '\\"')}" rev-parse --show-toplevel`);
    if (res.ok && res.stdout) {
      const repoRoot = res.stdout.trim();
      return { ok: true, repoRoot };
    }
    return { ok: false, error: res.error || 'Not a git repository' };
  } catch (err) {
    return { ok: false, error: err.message };
  }
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

module.exports = {
  detectGitRepo,
  readGitConfig
};
