(() => {
    function createGitQuickActions({ deps } = {}) {
        const logger = deps?.logger;
        const showToast = deps?.showToast;
        const gitRepoManager = deps?.gitRepoManager;
        const toGitPathspec = deps?.toGitPathspec;
        const openGitToolWindow = deps?.openGitToolWindow;
        const openCommitToolWindow = deps?.openCommitToolWindow;

        const normalizeGitError = (text, fallback = 'Git command failed') => {
            if (/not a git repository/i.test(text || '')) {
                return 'Git is not configured for this project';
            }
            return text || fallback;
        };

        const gitOutputGlobal = (text) => {
            const out = document.getElementById('gitOutput');
            if (out) {
                out.textContent += `${text}\n`;
                out.scrollTop = out.scrollHeight;
            }
        };

        async function runGitQuickCmd(cmd, { toastLabel = 'Git', silent = false } = {}) {
            logger.info('GIT_COMMAND', { cmd, toastLabel });
            if (!silent) gitOutputGlobal(`$ ${cmd}`);
            const res = await (gitRepoManager?.runGit ? gitRepoManager.runGit(cmd) : window.ahmadIDE.git(cmd));
            if (res.ok) {
                if (!silent) {
                    if (res.stdout) gitOutputGlobal(res.stdout);
                    if (res.stderr) gitOutputGlobal(res.stderr);
                }
                logger.info('GIT_COMMAND_SUCCESS', { cmd, stdout: res.stdout?.slice(0, 200), stderr: res.stderr?.slice(0, 200) });
            } else {
                const message = normalizeGitError(res.error || res.stderr);
                if (!silent) gitOutputGlobal(`✗ ${message}`);
                showToast('error', toastLabel, message);
                logger.error('GIT_COMMAND_FAIL', { cmd, error: message, stderr: res.stderr });
            }
            return res;
        }

        async function runGitContextAction(action, path) {
            const target = path || '.';
            const gitPath = toGitPathspec(target) || target;
            const safe = String(gitPath).replace(/"/g, '\\"');
            const setPath = (val) => {
                const input = document.getElementById('gitDiffPath');
                if (input) input.value = val;
            };
            const focusCommit = () => {
                const msg = document.getElementById('gitCommitMessage');
                msg?.focus();
            };
            const refresh = () => document.getElementById('gitStatusBtn')?.click();
            logger.info('GIT_CONTEXT_ACTION', { action, target });

            switch (action) {
                case 'add':
                    openGitToolWindow();
                    await runGitQuickCmd(`git add -- "${safe}"`, { toastLabel: 'Git Add' });
                    refresh();
                    return;
                case 'commit':
                    openCommitToolWindow();
                    setPath(gitPath);
                    await runGitQuickCmd(`git add -- "${safe}"`, { toastLabel: 'Commit File' });
                    showToast('info', 'Commit File', 'File staged. Enter a commit message in Git tool window.');
                    focusCommit();
                    refresh();
                    return;
                case 'history':
                    openGitToolWindow();
                    setPath(gitPath);
                    await runGitQuickCmd(`git log --oneline -- "${safe}"`, { toastLabel: 'Git History' });
                    document.getElementById('gitLogBtn')?.click();
                    return;
                case 'compare':
                    openGitToolWindow();
                    setPath(gitPath);
                    document.getElementById('gitDiffFileBtn')?.click();
                    return;
                case 'rollback':
                    openGitToolWindow();
                    await runGitQuickCmd(`git checkout -- "${safe}"`, { toastLabel: 'Rollback' });
                    refresh();
                    return;
                default:
                    showToast('info', 'UNKNOWN – NEED DESIGN DECISION', `Git action ${action} not wired`);
            }
        }

        return {
            normalizeGitError,
            gitOutputGlobal,
            runGitQuickCmd,
            runGitContextAction
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.git = window.AhmadIDEModules.renderer.git || {};
        window.AhmadIDEModules.renderer.git.createGitQuickActions = createGitQuickActions;
    }
})();
