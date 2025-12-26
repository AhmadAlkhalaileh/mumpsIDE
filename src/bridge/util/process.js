const { spawn } = require('child_process');
const path = require('path');

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

module.exports = {
  run,
  tmpFile,
  wrapDockerCmd
};
