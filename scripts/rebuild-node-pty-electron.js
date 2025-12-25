#!/usr/bin/env node
'use strict';

const path = require('path');
const { spawnSync } = require('child_process');

const projectRoot = path.join(__dirname, '..');

function readElectronVersion() {
    try {
        // eslint-disable-next-line global-require
        const pkg = require('electron/package.json');
        return String(pkg?.version || '').trim();
    } catch (err) {
        return '';
    }
}

const electronVersion = readElectronVersion();
if (!electronVersion) {
    console.error('[rebuild:pty] Could not read Electron version from node_modules/electron.');
    process.exit(1);
}

const npmCmd = process.platform === 'win32' ? 'npm.cmd' : 'npm';
const args = [
    'rebuild',
    'node-pty',
    '--runtime=electron',
    `--target=${electronVersion}`,
    '--dist-url=https://electronjs.org/headers'
];

console.log('[rebuild:pty] Rebuilding node-pty for Electron', electronVersion);
console.log('[rebuild:pty] Running:', npmCmd, args.join(' '));

const res = spawnSync(npmCmd, args, {
    cwd: projectRoot,
    stdio: 'inherit',
    env: process.env
});

process.exit(typeof res.status === 'number' ? res.status : 1);

