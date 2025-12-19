#!/usr/bin/env node
'use strict';

const fs = require('fs');
const path = require('path');

const repoRoot = process.cwd();

const scanRoots = [
  'index.html',
  'main.js',
  'preload.js',
  'renderer.js',
  'monaco-loader-init.js',
  'src',
  'styles',
  'libs',
  'assets',
  'utils'
];

const excludedDirNames = new Set([
  '.git',
  'node_modules',
  'dist',
  'out',
  'build',
  'release',
  'snap',
  'overlay',
  'perf'
]);

const allowedExtensions = new Set([
  '.html',
  '.css',
  '.js',
  '.mjs',
  '.cjs',
  '.ts',
  '.json',
  '.svg'
]);

const allowedUrlStrings = new Set([
  'http://www.w3.org/2000/svg',
  'https://www.w3.org/2000/svg',
  'http://www.w3.org/1999/xhtml',
  'https://www.w3.org/1999/xhtml',
  'http://www.w3.org/2000/xmlns/',
  'https://www.w3.org/2000/xmlns/'
]);

function toPosix(p) {
  return p.split(path.sep).join('/');
}

function exists(p) {
  try {
    fs.accessSync(p, fs.constants.F_OK);
    return true;
  } catch (_) {
    return false;
  }
}

function listFilesRecursively(entryPath, out) {
  const stat = fs.statSync(entryPath);
  if (stat.isFile()) {
    out.push(entryPath);
    return;
  }
  if (!stat.isDirectory()) return;

  const baseName = path.basename(entryPath);
  if (excludedDirNames.has(baseName)) return;

  const entries = fs.readdirSync(entryPath, { withFileTypes: true });
  for (const entry of entries) {
    const childPath = path.join(entryPath, entry.name);
    if (entry.isDirectory()) {
      if (excludedDirNames.has(entry.name)) continue;
      listFilesRecursively(childPath, out);
      continue;
    }
    if (entry.isFile()) out.push(childPath);
  }
}

function* findUrlMatches(line) {
  const patterns = [
    /\bhttps?:\/\/[^\s"'<>]+/g,
    /(?:src|href)\s*=\s*["']\/\/[^\s"'<>]+/gi,
    /url\(\s*["']?\/\/[^\s"')]+/gi,
    /@import\s+url\(\s*["']?\/\/[^\s"')]+/gi
  ];

  for (const pattern of patterns) {
    let match;
    while ((match = pattern.exec(line))) {
      const raw = match[0];
      const cleaned = raw
        .replace(/^(?:src|href)\s*=\s*["']?/i, '')
        .replace(/^url\(\s*["']?/i, '')
        .replace(/^@import\s+url\(\s*["']?/i, '')
        .replace(/[)"']+$/g, '');
      yield cleaned;
    }
  }
}

function scanFile(filePath) {
  const ext = path.extname(filePath);
  if (!allowedExtensions.has(ext)) return [];

  let content;
  try {
    content = fs.readFileSync(filePath, 'utf8');
  } catch (_) {
    return [];
  }

  const relPath = toPosix(path.relative(repoRoot, filePath));
  const issues = [];
  const lines = content.split(/\r?\n/);

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    for (const url of findUrlMatches(line)) {
      if (allowedUrlStrings.has(url)) continue;
      issues.push({
        file: relPath,
        line: i + 1,
        url
      });
    }
  }

  return issues;
}

function main() {
  const targets = [];
  for (const root of scanRoots) {
    const abs = path.join(repoRoot, root);
    if (!exists(abs)) continue;
    listFilesRecursively(abs, targets);
  }

  /** @type {{file:string,line:number,url:string}[]} */
  const issues = [];
  for (const filePath of targets) issues.push(...scanFile(filePath));

  if (!issues.length) {
    process.stdout.write('OK: no CDN/remote URLs found in runtime sources.\n');
    return;
  }

  issues.sort((a, b) => (a.file < b.file ? -1 : a.file > b.file ? 1 : a.line - b.line));
  process.stderr.write('ERROR: Remote URLs detected (CDN/offline policy violation):\n');
  for (const issue of issues) {
    process.stderr.write(`- ${issue.file}:${issue.line} -> ${issue.url}\n`);
  }
  process.exitCode = 1;
}

main();

