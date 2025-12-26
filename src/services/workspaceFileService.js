const fs = require('fs');
const path = require('path');

function createWorkspaceFileService({ logger } = {}) {
    const log = logger || { info: () => { }, warn: () => { }, error: () => { } };

    const IGNORE_DIRS = new Set([
        '.git',
        '.hg',
        '.svn',
        '.idea',
        '.vscode',
        '.cache',
        '.yarn',
        'node_modules',
        'dist',
        'build',
        'out',
        'target',
        'vendor'
    ]);

    const matchesPattern = (name, pattern) => {
        const n = String(name || '');
        const p = String(pattern || '').trim();
        if (!p || p === '*') return true;
        if (p.toLowerCase() === '*.m') return /\.m$/i.test(n);
        if (p.startsWith('*.') && p.length > 2) {
            const ext = p.slice(1); // ".m"
            return n.toLowerCase().endsWith(ext.toLowerCase());
        }
        return n === p;
    };

    const walk = async (rootDir, { pattern = '*', maxFiles = 250000, maxDepth = 50 } = {}) => {
        const root = path.resolve(String(rootDir || '').trim());
        if (!root) return { files: [], dirs: [] };

        let rootStat;
        try {
            rootStat = await fs.promises.stat(root);
        } catch (err) {
            return { files: [], dirs: [], error: err?.message || String(err) };
        }
        if (!rootStat.isDirectory()) {
            return { files: [], dirs: [], error: 'Root is not a directory' };
        }

        const files = [];
        const dirs = [];
        const queue = [{ dir: root, depth: 0 }];

        while (queue.length) {
            const { dir, depth } = queue.pop();
            dirs.push(dir);
            if (depth > maxDepth) continue;

            let entries;
            try {
                entries = await fs.promises.readdir(dir, { withFileTypes: true });
            } catch (_) {
                continue;
            }

            for (const ent of entries) {
                if (!ent || !ent.name) continue;
                const name = ent.name;
                const full = path.join(dir, name);

                if (ent.isDirectory()) {
                    if (IGNORE_DIRS.has(name)) continue;
                    queue.push({ dir: full, depth: depth + 1 });
                    continue;
                }

                if (ent.isFile()) {
                    if (!matchesPattern(name, pattern)) continue;
                    files.push(full);
                    if (files.length >= maxFiles) {
                        return { files, dirs, truncated: true };
                    }
                }
            }
        }

        return { files, dirs };
    };

    const listFiles = async (dir, pattern = '*', opts = {}) => {
        const res = await walk(dir, {
            pattern,
            maxFiles: opts?.maxFiles,
            maxDepth: opts?.maxDepth
        });
        if (res.error) {
            log.warn('WORKSPACE_LIST_FILES_ERROR', { dir, pattern, error: res.error });
            return { ok: false, error: res.error, files: [] };
        }
        return { ok: true, files: res.files, truncated: !!res.truncated };
    };

    const readFile = async (filePath, opts = {}) => {
        const p = path.resolve(String(filePath || '').trim());
        if (!p) return { ok: false, error: 'No file path provided' };
        let stat;
        try {
            stat = await fs.promises.stat(p);
        } catch (err) {
            const msg = err?.code === 'ENOENT' ? 'File not found' : (err?.message || String(err));
            return { ok: false, notFound: err?.code === 'ENOENT', error: msg };
        }
        if (!stat.isFile()) return { ok: false, error: 'Path is not a file' };

        const maxBytes = Number.isFinite(Number(opts?.maxBytes)) ? Number(opts.maxBytes) : 5 * 1024 * 1024;
        if (maxBytes > 0 && stat.size > maxBytes) {
            return { ok: false, tooLarge: true, error: `File too large (${stat.size} bytes)` };
        }

        let content;
        try {
            content = await fs.promises.readFile(p, 'utf8');
        } catch (err) {
            return { ok: false, error: err?.message || String(err) };
        }

        return { ok: true, content, mtime: stat.mtimeMs };
    };

    let nextWatchId = 1;
    const watches = new Map(); // watchId -> { watchers: FSWatcher[], root, pattern, sender }

    const watch = async ({ root, pattern = '*.m', sender, opts = {} } = {}) => {
        const resolvedRoot = path.resolve(String(root || '').trim());
        if (!resolvedRoot) return { ok: false, error: 'No root provided' };
        if (!sender || typeof sender.send !== 'function') return { ok: false, error: 'No IPC sender provided' };

        const scan = await walk(resolvedRoot, {
            pattern: '*',
            maxFiles: opts?.maxFiles,
            maxDepth: opts?.maxDepth
        });
        if (scan.error) return { ok: false, error: scan.error };

        const watchId = String(nextWatchId++);
        const watchers = [];
        const onFsEvent = (dir) => (eventType, filename) => {
            try {
                const raw = filename ? String(filename) : '';
                if (!raw) {
                    sender.send('workspace:filesChanged', { watchId, eventType, path: '', dir });
                    return;
                }
                if (!matchesPattern(raw, pattern)) return;
                const full = path.join(dir, raw);
                sender.send('workspace:filesChanged', { watchId, eventType, path: full });
            } catch (err) {
                sender.send('workspace:filesChanged', { watchId, eventType: 'error', path: '', error: err?.message || String(err) });
            }
        };

        for (const dir of scan.dirs || []) {
            try {
                const watcher = fs.watch(dir, { persistent: false }, onFsEvent(dir));
                watchers.push(watcher);
            } catch (_) { }
        }

        watches.set(watchId, { watchers, root: resolvedRoot, pattern, sender });
        log.info('WORKSPACE_WATCH_STARTED', { watchId, root: resolvedRoot, watcherCount: watchers.length });
        return { ok: true, watchId, watcherCount: watchers.length };
    };

    const unwatch = async (watchId) => {
        const id = String(watchId || '').trim();
        if (!id) return { ok: false, error: 'No watchId provided' };
        const entry = watches.get(id);
        if (!entry) return { ok: true, watchId: id, alreadyStopped: true };

        for (const w of entry.watchers || []) {
            try { w?.close?.(); } catch (_) { }
        }
        watches.delete(id);
        log.info('WORKSPACE_WATCH_STOPPED', { watchId: id });
        return { ok: true, watchId: id };
    };

    const dispose = () => {
        for (const id of Array.from(watches.keys())) {
            try { unwatch(id); } catch (_) { }
        }
    };

    return {
        listFiles,
        readFile,
        watch,
        unwatch,
        dispose
    };
}

module.exports = { createWorkspaceFileService };

