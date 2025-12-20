(() => {
    function renderVersionControlSection(ctx) {
        const primitives = ctx?.primitives || window.AhmadIDEModules?.ui?.primitives || {};
        const createButton = primitives.createButton || ((opts = {}) => {
            const btn = document.createElement('button');
            btn.type = 'button';
            btn.className = `ui-btn ui-btn--${opts.variant || 'default'} ui-btn--${opts.size || 'md'}`;
            btn.textContent = String(opts.label || '');
            if (typeof opts.onClick === 'function') btn.addEventListener('click', (e) => opts.onClick(e));
            return btn;
        });
        const createInput = primitives.createInput || ((opts = {}) => {
            const input = document.createElement('input');
            input.className = 'ui-input';
            input.type = opts.type || 'text';
            input.value = opts.value ?? '';
            input.placeholder = opts.placeholder ?? '';
            input.disabled = !!opts.disabled;
            if (typeof opts.onInput === 'function') input.addEventListener('input', (e) => opts.onInput(e));
            if (typeof opts.onChange === 'function') input.addEventListener('change', (e) => opts.onChange(e));
            return input;
        });

        const repoManager = window.AhmadIDEModules?.git?.repoManager || window.AhmadIDE?.gitRepoManager || null;
        const getState = () => {
            try {
                return repoManager?.getState?.() || {};
            } catch (_) {
                return {};
            }
        };

        const notify = (type, title, message) => {
            try { if (typeof showToast === 'function') { showToast(type, title, message); return; } } catch (_) { }
            try { alert(`${title}\n\n${message}`); } catch (_) { }
        };

        const showPrompt = async ({ title, message, placeholder = '', defaultValue = '' } = {}) => {
            try {
                const fn = window.AhmadIDEModules?.ui?.showPrompt;
                if (typeof fn === 'function') return await fn({ title, message, placeholder, defaultValue });
            } catch (_) { }
            try {
                const res = window.prompt(`${title}\n\n${message}`, defaultValue || '');
                if (res == null) return null;
                return String(res);
            } catch (_) {
                return null;
            }
        };

        const showConfirm = async ({ title, message, variant = 'default', confirmLabel = 'Yes', cancelLabel = 'No' } = {}) => {
            try {
                const fn = window.AhmadIDEModules?.ui?.showConfirm;
                if (typeof fn === 'function') return await fn({ title, message, variant, confirmLabel, cancelLabel });
            } catch (_) { }
            try {
                return window.confirm(`${title}\n\n${message}`);
            } catch (_) {
                return false;
            }
        };

        const escapeArg = (val) => String(val || '').replace(/"/g, '\\"');

        const runGit = async (cmd, { repoRoot } = {}) => {
            const st = getState();
            if (st.gitDisabled) return { ok: false, error: 'Git is disabled for this project' };

            if (repoManager?.runGit) {
                return repoManager.runGit(cmd, { repoRoot: repoRoot || st.repoRoot || '' });
            }

            const root = String(repoRoot || st.repoRoot || '').trim();
            const shouldScope = root && /^git(\s|$)/i.test(String(cmd || '')) && !/^git\s+-C\s+/i.test(String(cmd || ''));
            const finalCmd = shouldScope
                ? `git -C "${escapeArg(root)}" ${String(cmd || '').replace(/^git\s+/i, '')}`
                : String(cmd || '');
            if (!window.ahmadIDE?.git) return { ok: false, error: 'Git API unavailable' };
            return window.ahmadIDE.git(finalCmd);
        };

        const root = document.createElement('div');
        root.dataset.filterText = 'version control vcs git repository remote origin user email name';

        const makeGroup = (title, hint, filterText) => {
            const group = document.createElement('div');
            group.className = 'ui-settings-group';
            group.dataset.filterText = String(filterText || '').trim();
            const t = document.createElement('div');
            t.className = 'ui-settings-group__title';
            t.textContent = title;
            const h = document.createElement('div');
            h.className = 'ui-settings-group__hint';
            h.textContent = hint || '';
            group.appendChild(t);
            group.appendChild(h);
            return group;
        };

        const makeRow = (labelText, controlNode, filterText) => {
            const row = document.createElement('div');
            row.className = 'ui-settings-row';
            row.dataset.filterText = String(filterText || '').trim();
            const label = document.createElement('div');
            label.className = 'ui-settings-row__label';
            label.textContent = labelText;
            const control = document.createElement('div');
            control.className = 'ui-settings-row__control';
            control.appendChild(controlNode);
            row.appendChild(label);
            row.appendChild(control);
            return row;
        };

        const stateGroup = makeGroup(
            'Git',
            'Configure repository connection, author identity, and origin remote.',
            'git version control vcs repository remote origin repo root'
        );

        const repoRootInput = createInput({ value: '', placeholder: 'Not detected', disabled: true });
        repoRootInput.style.flex = '1';
        const projectRootInput = createInput({ value: '', placeholder: 'No project', disabled: true });
        projectRootInput.style.flex = '1';

        const btnReconnect = createButton({
            label: 'Reconnect',
            variant: 'ghost',
            onClick: async () => {
                try {
                    await repoManager?.actions?.reconnect?.();
                    hydrateFromState();
                    notify('success', 'Git', 'Reconnected.');
                } catch (err) {
                    notify('error', 'Git', err?.message || 'Reconnect failed');
                }
            }
        });

        const btnChooseRoot = createButton({
            label: 'Choose Repo Root…',
            variant: 'ghost',
            onClick: async () => {
                try {
                    const res = await repoManager?.actions?.chooseRepoRoot?.();
                    hydrateFromState();
                    if (res?.ok) notify('success', 'Git', 'Repository root updated.');
                } catch (err) {
                    notify('error', 'Git', err?.message || 'Choose root failed');
                }
            }
        });

        const btnToggleGit = createButton({
            label: 'Disable Git',
            variant: 'danger',
            onClick: async () => {
                const st = getState();
                const disable = !st.gitDisabled;
                const ok = await showConfirm({
                    title: disable ? 'Disable Git' : 'Enable Git',
                    message: disable
                        ? 'Disable Git integration for this project?'
                        : 'Enable Git integration for this project?',
                    variant: disable ? 'danger' : 'default',
                    confirmLabel: disable ? 'Disable' : 'Enable',
                    cancelLabel: 'Cancel'
                });
                if (!ok) return;
                try {
                    if (disable) await repoManager?.actions?.disableGit?.();
                    else await repoManager?.actions?.enableGit?.();
                    hydrateFromState();
                } catch (err) {
                    notify('error', 'Git', err?.message || 'Toggle failed');
                }
            }
        });

        const btnInit = createButton({
            label: 'Init Repo',
            variant: 'ghost',
            onClick: async () => {
                try {
                    const st = getState();
                    const root = st.projectRoot || st.repoRoot || '';
                    const res = await repoManager?.actions?.initializeRepo?.({ root });
                    hydrateFromState();
                    if (res?.ok) notify('success', 'Git', 'Repository initialized.');
                    else notify('error', 'Git', res?.error || 'git init failed');
                } catch (err) {
                    notify('error', 'Git', err?.message || 'git init failed');
                }
            }
        });

        const btnClone = createButton({
            label: 'Clone…',
            variant: 'ghost',
            onClick: async () => {
                const url = await showPrompt({
                    title: 'Clone Repository',
                    message: 'Enter repository URL',
                    placeholder: 'https://github.com/user/repo.git',
                    defaultValue: ''
                });
                if (!url) return;
                const directory = await showPrompt({
                    title: 'Clone Repository',
                    message: 'Enter target directory (will be created if needed)',
                    placeholder: '/path/to/clone/repo',
                    defaultValue: ''
                });
                if (!directory) return;
                try {
                    const res = await repoManager?.actions?.cloneRepo?.({ url, directory });
                    hydrateFromState();
                    if (res?.ok) notify('success', 'Git', 'Clone completed.');
                    else notify('error', 'Git', res?.error || 'Clone failed');
                } catch (err) {
                    notify('error', 'Git', err?.message || 'Clone failed');
                }
            }
        });

        const stateActions = document.createElement('div');
        stateActions.style.display = 'flex';
        stateActions.style.flexWrap = 'wrap';
        stateActions.style.gap = '6px';
        stateActions.appendChild(btnReconnect);
        stateActions.appendChild(btnChooseRoot);
        stateActions.appendChild(btnInit);
        stateActions.appendChild(btnClone);
        stateActions.appendChild(btnToggleGit);

        const statusText = document.createElement('div');
        statusText.className = 'ui-settings-group__hint';
        statusText.style.marginTop = '8px';

        stateGroup.appendChild(makeRow('Project root', projectRootInput, 'project root'));
        stateGroup.appendChild(makeRow('Repo root', repoRootInput, 'repository root'));
        stateGroup.appendChild(makeRow('Actions', stateActions, 'reconnect choose root init clone disable'));
        stateGroup.appendChild(statusText);

        const cfgGroup = makeGroup('Git Config', 'Applies to the current repository (not global).', 'git config user name email remote origin');

        const nameInput = createInput({ value: '', placeholder: 'user.name' });
        nameInput.style.flex = '1';
        const emailInput = createInput({ value: '', placeholder: 'user.email' });
        emailInput.style.flex = '1';
        const remoteInput = createInput({ value: '', placeholder: 'origin URL' });
        remoteInput.style.flex = '1';

        const cfgActions = document.createElement('div');
        cfgActions.style.display = 'flex';
        cfgActions.style.gap = '6px';
        cfgActions.style.flexWrap = 'wrap';

        const btnSave = createButton({
            label: 'Save',
            variant: 'primary',
            onClick: async () => {
                const st = getState();
                const repoRoot = String(st.repoRoot || '').trim();
                if (!repoRoot) {
                    notify('error', 'Git', 'No repository detected. Choose a repo root first.');
                    return;
                }

                const name = String(nameInput.value || '').trim();
                const email = String(emailInput.value || '').trim();
                const remote = String(remoteInput.value || '').trim();

                const cmds = [];
                if (name) cmds.push(`git config user.name "${escapeArg(name)}"`);
                if (email) cmds.push(`git config user.email "${escapeArg(email)}"`);
                if (remote) {
                    const escRemote = escapeArg(remote);
                    cmds.push(`git remote set-url origin "${escRemote}" || git remote add origin "${escRemote}"`);
                }

                if (!cmds.length) {
                    notify('info', 'Git', 'No changes to apply.');
                    return;
                }

                for (const cmd of cmds) {
                    const res = await runGit(cmd, { repoRoot });
                    if (!res?.ok) {
                        notify('error', 'Git', res?.error || res?.stderr || 'Git command failed');
                        return;
                    }
                }

                await repoManager?.actions?.reconnect?.();
                hydrateFromState();
                notify('success', 'Git', 'Git settings applied.');
            }
        });

        const btnTest = createButton({
            label: 'Test',
            variant: 'ghost',
            onClick: async () => {
                const st = getState();
                const repoRoot = String(st.repoRoot || '').trim();
                if (!repoRoot) {
                    notify('info', 'Git', 'No repository detected.');
                    return;
                }
                const res = await runGit('git status --short', { repoRoot });
                if (!res?.ok) {
                    notify('error', 'Git', res?.error || res?.stderr || 'git status failed');
                    return;
                }
                notify('success', 'Git', res.stdout ? res.stdout.trim() : 'Status OK');
            }
        });

        cfgActions.appendChild(btnSave);
        cfgActions.appendChild(btnTest);

        cfgGroup.appendChild(makeRow('User name', nameInput, 'user name git config'));
        cfgGroup.appendChild(makeRow('User email', emailInput, 'user email git config'));
        cfgGroup.appendChild(makeRow('Remote (origin)', remoteInput, 'remote origin url'));
        cfgGroup.appendChild(makeRow('Actions', cfgActions, 'save test'));

        const hydrateFromState = async () => {
            const st = getState();
            const projectRoot = String(st.projectRoot || '').trim();
            const repoRoot = String(st.repoRoot || '').trim();
            const remote = String(st.lastKnownRemote || '').trim();

            projectRootInput.value = projectRoot || '';
            repoRootInput.value = repoRoot || '';

            try {
                const label = btnToggleGit.querySelector('.ui-btn__label') || null;
                if (label) label.textContent = st.gitDisabled ? 'Enable Git' : 'Disable Git';
                else btnToggleGit.textContent = st.gitDisabled ? 'Enable Git' : 'Disable Git';
            } catch (_) { }
            btnToggleGit.className = `ui-btn ui-btn--${st.gitDisabled ? 'ghost' : 'danger'} ui-btn--md`;

            const flags = [
                st.gitDisabled ? 'disabled' : null,
                st.gitAvailable ? 'gitAvailable' : 'gitMissing',
                st.repoDetected ? 'repoDetected' : 'noRepo'
            ].filter(Boolean);
            statusText.textContent = st.lastError
                ? `${flags.join(' · ')} · ${st.lastError}`
                : flags.join(' · ');

            if (!repoRoot) {
                remoteInput.value = remote || '';
                return;
            }

            // Fast path: parse project config if bridge is available.
            try {
                if (window.ahmadIDE?.getGitConfig) {
                    const cfg = await window.ahmadIDE.getGitConfig(repoRoot);
                    if (cfg?.ok) {
                        if (cfg.user?.name) nameInput.value = String(cfg.user.name);
                        if (cfg.user?.email) emailInput.value = String(cfg.user.email);
                        if (cfg.remote?.origin) remoteInput.value = String(cfg.remote.origin);
                        return;
                    }
                }
            } catch (_) { }

            // Fallback: git commands (scoped).
            try {
                const resName = await runGit('git config user.name', { repoRoot });
                if (resName?.ok && resName.stdout) nameInput.value = resName.stdout.trim();
                const resEmail = await runGit('git config user.email', { repoRoot });
                if (resEmail?.ok && resEmail.stdout) emailInput.value = resEmail.stdout.trim();
                if (!remoteInput.value) {
                    const resRemote = await runGit('git remote get-url origin', { repoRoot });
                    if (resRemote?.ok && resRemote.stdout) remoteInput.value = resRemote.stdout.trim();
                    else remoteInput.value = remote || '';
                }
            } catch (_) { }
        };

        root.appendChild(stateGroup);
        root.appendChild(cfgGroup);

        hydrateFromState().catch(() => { });

        return root;
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.settings = window.AhmadIDEModules.features.settings || {};
        window.AhmadIDEModules.features.settings.sections = window.AhmadIDEModules.features.settings.sections || {};
        window.AhmadIDEModules.features.settings.sections.renderVersionControlSection = renderVersionControlSection;
    }
})();
