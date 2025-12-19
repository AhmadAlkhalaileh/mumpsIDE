(() => {
    function createGitSettingsManager({ deps } = {}) {
        const $ = deps?.$ || (typeof window !== 'undefined' ? (window.$ || window.jQuery || null) : null);
        const showToast = deps?.showToast || (() => { });
        const getCurrentProject = deps?.getCurrentProject || (() => null);

        if (!$) {
            throw new Error('createGitSettingsManager requires jQuery ($)');
        }

        let lazyHooked = false;
        let bound = false;

        function bindGitSettingsPanel() {
            if (bound) return;
            const saveBtn = $('#gitSettingsSave');
            const testBtn = $('#gitSettingsTest');
            const statusEl = $('#gitSettingsStatus');
            const nameInput = $('#gitUserName');
            const emailInput = $('#gitUserEmail');
            const remoteInput = $('#gitRemoteUrl');
            if (!saveBtn.length || !statusEl.length) {
                // Settings panel may be lazy-mounted.
                if (!lazyHooked) {
                    const fr = window.AhmadIDEModules?.app?.featureRegistry;
                    fr?.onMounted?.('settingsPanel', () => {
                        lazyHooked = false;
                        bindGitSettingsPanel();
                    });
                    lazyHooked = true;
                }
                return;
            }
            lazyHooked = true;
            bound = true;

            // Prefill from project .git/config or global/home git config
            (async () => {
                // Try project .git/config first if a project is open
                const currentProject = getCurrentProject();
                if (currentProject && currentProject.projectPath) {
                    const gitConfig = await window.ahmadIDE.getGitConfig(currentProject.projectPath);
                    if (gitConfig?.ok) {
                        if (gitConfig.user?.name) nameInput.val(gitConfig.user.name);
                        if (gitConfig.user?.email) emailInput.val(gitConfig.user.email);
                        if (gitConfig.remote?.origin) remoteInput.val(gitConfig.remote.origin);
                        return; // Found project config, don't load global
                    }
                }

                // Fallback to global git config
                const resUser = await window.ahmadIDE.git('git config --global user.name');
                if (resUser?.ok && resUser.stdout) nameInput.val(resUser.stdout.trim());
                const resEmail = await window.ahmadIDE.git('git config --global user.email');
                if (resEmail?.ok && resEmail.stdout) emailInput.val(resEmail.stdout.trim());
            })();

            const runGit = async (cmd) => {
                try {
                    const res = await window.ahmadIDE.git(cmd);
                    if (!res.ok) {
                        showToast('error', 'Git', res.error || res.stderr || 'Git command failed');
                        statusEl.text(`Error: ${res.error || res.stderr || 'Git command failed'}`);
                        return false;
                    }
                    return res;
                } catch (e) {
                    showToast('error', 'Git', e.message || 'Git command failed');
                    statusEl.text(`Error: ${e.message || 'Git command failed'}`);
                    return false;
                }
            };

            saveBtn.on('click', async () => {
                const name = (nameInput.val() || '').trim();
                const email = (emailInput.val() || '').trim();
                const remote = (remoteInput.val() || '').trim();
                const cmds = [];
                if (name) cmds.push(`git config user.name "${name.replace(/"/g, '\\"')}"`);
                if (email) cmds.push(`git config user.email "${email.replace(/"/g, '\\"')}"`);
                if (remote) {
                    const escRemote = remote.replace(/"/g, '\\"');
                    cmds.push(`git remote set-url origin "${escRemote}" || git remote add origin "${escRemote}"`);
                }
                if (!cmds.length) {
                    statusEl.text('No changes to apply');
                    return;
                }
                statusEl.text('Applying...');
                for (const cmd of cmds) {
                    const ok = await runGit(cmd);
                    if (!ok) return;
                }
                statusEl.text('Git settings applied');
                showToast('info', 'Git', 'Git settings applied');
            });

            testBtn.on('click', async () => {
                statusEl.text('Testing...');
                const res = await runGit('git status --short');
                if (res && res.ok) {
                    statusEl.text(res.stdout ? res.stdout.trim() : 'Clean status');
                    showToast('info', 'Git', 'Status ok');
                }
            });
        }

        return {
            bindGitSettingsPanel
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.git = window.AhmadIDEModules.git || {};
        window.AhmadIDEModules.git.createGitSettingsManager = createGitSettingsManager;
    }
})();
