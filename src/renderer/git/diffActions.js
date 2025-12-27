(() => {
    function createDiffActions({ deps } = {}) {
        const getGitRepoManager = deps?.getGitRepoManager;
        const getCurrentProject = deps?.getCurrentProject;
        const showToast = deps?.showToast;
        const normalizeGitError = deps?.normalizeGitError;
        const createCustomTab = deps?.createCustomTab;
        const computeDiffAnchors = deps?.computeDiffAnchors;
        const syncDiffToolbar = deps?.syncDiffToolbar;
        const syncDiffTabViewer = deps?.syncDiffTabViewer;

        function toGitPathspec(inputPath) {
            const raw = String(inputPath || '').trim();
            if (!raw) return '';
            if (raw === '.') return '.';

            try {
                const gitRepoManager = getGitRepoManager ? getGitRepoManager() : null;
                const currentProject = getCurrentProject ? getCurrentProject() : null;
                const repoState = gitRepoManager?.getState ? gitRepoManager.getState() : null;
                const repoRoot = String(repoState?.repoRoot || '').trim();
                const projectRoot = String(currentProject?.projectPath || '').trim();
                if (!repoRoot || !projectRoot) return raw;

                const path = require('path');
                const abs = path.isAbsolute(raw) ? raw : path.join(projectRoot, raw);
                const rel = path.relative(repoRoot, abs);
                if (!rel || rel.startsWith('..')) return raw;
                return rel.split(path.sep).join('/');
            } catch (_) {
                return raw;
            }
        }

        async function openDiffTab(opts = {}) {
            const gitRepoManager = getGitRepoManager ? getGitRepoManager() : null;
            const tabIcon = (() => {
                try {
                    const icons = window.AhmadIDEModules?.ui?.icons?.map || {};
                    return icons.git || icons.format || '';
                } catch (_) {
                    return '';
                }
            })();

            const st = gitRepoManager?.getState ? gitRepoManager.getState() : null;
            if (!st) {
                showToast('error', 'Diff', 'Git state is not ready');
                return { ok: false, error: 'Git state not ready' };
            }
            if (st.gitDisabled) {
                showToast('info', 'Diff', 'Git is disabled for this project');
                return { ok: false, error: 'Git disabled' };
            }
            if (!st.gitAvailable) {
                showToast('error', 'Diff', st.lastError || 'Git is not available');
                return { ok: false, error: st.lastError || 'Git unavailable' };
            }
            if (!st.repoDetected || !st.repoRoot) {
                showToast('info', 'Diff', st.lastError || 'No Git repository detected');
                return { ok: false, error: st.lastError || 'No repo detected' };
            }

            const kind = String(opts.kind || '').trim() || 'worktree';
            const pathIn = String(opts.path || '').trim();
            if (!pathIn) {
                showToast('error', 'Diff', 'No file selected');
                return { ok: false, error: 'No path provided' };
            }

            const repoRoot = String(opts.repoRoot || st.repoRoot || '').trim();
            const staged = !!opts.staged;
            const status = String(opts.status || '').trim();
            const oldPath = String(opts.oldPath || '').trim();
            const commitHash = String(opts.commitHash || '').trim();
            const parents = Array.isArray(opts.parents) ? opts.parents.filter(Boolean) : [];
            const parentHash = String(opts.parentHash || parents[0] || '').trim();
            const ignoreWhitespace = !!opts.ignoreWhitespace;

            const escapeArg = (val) => String(val || '').replace(/"/g, '\\"');
            const q = (val) => `"${escapeArg(val)}"`;
            const baseName = (pathIn.split('/').pop() || pathIn).trim() || pathIn;

            const pathspecs = [];
            if (oldPath && oldPath !== pathIn) pathspecs.push(oldPath);
            pathspecs.push(pathIn);
            const pathSpecArgs = pathspecs.map(q).join(' ');
            const wsFlag = ignoreWhitespace ? ' -w' : '';

            let cmd = '';
            let key = '';
            let title = `${baseName} (Diff)`;

            if (kind === 'commit') {
                if (!commitHash) {
                    showToast('error', 'Diff', 'No commit selected');
                    return { ok: false, error: 'No commit hash provided' };
                }
                if (parentHash) {
                    cmd = `git diff --no-color -M${wsFlag} ${parentHash} ${commitHash} -- ${pathSpecArgs}`;
                } else {
                    // Root commit (no parent): git show produces a diff against the empty tree.
                    cmd = `git show --no-color -M${wsFlag} --format= ${commitHash} -- ${pathSpecArgs}`;
                }
                key = `diff:commit:${repoRoot}:${commitHash}:${pathIn}`;
            } else if (kind === 'worktree') {
                const stageKey = staged ? 'staged' : 'worktree';
                key = `diff:${stageKey}:${repoRoot}:${pathIn}`;
                if (status === '?' && !staged) {
                    // Untracked file: git diff won't show it; use a no-index diff from /dev/null.
                    try {
                        const pathMod = require('path');
                        const abs = pathMod.isAbsolute(pathIn) ? pathIn : pathMod.join(repoRoot, pathIn);
                        const nullDev = (typeof process !== 'undefined' && process.platform === 'win32') ? 'NUL' : '/dev/null';
                        cmd = `git diff --no-index --no-color${wsFlag} -- ${nullDev} ${q(abs)}`;
                    } catch (_) {
                        cmd = '';
                    }
                } else {
                    cmd = staged
                        ? `git diff --no-color -M${wsFlag} --staged -- ${pathSpecArgs}`
                        : `git diff --no-color -M${wsFlag} -- ${pathSpecArgs}`;
                }
            } else {
                showToast('error', 'Diff', `Unsupported diff kind: ${kind}`);
                return { ok: false, error: `Unsupported diff kind: ${kind}` };
            }

            if (!cmd) {
                showToast('error', 'Diff', 'Unable to build diff command');
                return { ok: false, error: 'No diff command' };
            }

            let res = null;
            try {
                res = await (gitRepoManager?.runGit ? gitRepoManager.runGit(cmd, { silent: true }) : window.ahmadIDE.git(cmd));
            } catch (err) {
                showToast('error', 'Diff', err?.message || 'Git diff failed');
                return { ok: false, error: err?.message || 'Git diff failed' };
            }

            if (!res?.ok) {
                const message = res?.error || res?.stderr || 'Git diff failed';
                let normalized = message;
                try {
                    if (typeof normalizeGitError === 'function') normalized = normalizeGitError(message, 'Git diff failed');
                } catch (_) { }
                showToast('error', 'Diff', normalized);
                return { ok: false, error: message };
            }

            let diffText = String(res.stdout || '');
            if (!diffText.trim()) {
                diffText = 'No changes.\n';
            }

            if (typeof createCustomTab !== 'function') {
                showToast('error', 'Diff', 'Tab system unavailable');
                return { ok: false, error: 'Tab system unavailable' };
            }

            const meta = {
                kind,
                repoRoot,
                path: pathIn,
                oldPath,
                status,
                staged,
                commitHash,
                parents,
                parentHash,
                ignoreWhitespace,
                source: String(opts.source || '').trim()
            };

            const tab = createCustomTab({
                key,
                title,
                content: diffText,
                language: 'plaintext',
                icon: tabIcon,
                kind: 'diff',
                readOnly: true,
                meta
            });

            if (tab) {
                tab.diffAnchors = computeDiffAnchors(diffText);
                // If this diff tab was already active, the tab system won't re-fire activation;
                // keep the diff toolbar + split viewer in sync anyway.
                try { syncDiffToolbar(tab); } catch (_) { }
                try { syncDiffTabViewer(tab); } catch (_) { }
                return { ok: true, tabId: tab.id };
            }

            return { ok: false, error: 'Failed to create diff tab' };
        }

        return { toGitPathspec, openDiffTab };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.git = window.AhmadIDEModules.renderer.git || {};
        window.AhmadIDEModules.renderer.git.createDiffActions = createDiffActions;
    }
})();
