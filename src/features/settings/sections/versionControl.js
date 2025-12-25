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
        const createToggle = primitives.createToggle || ((opts = {}) => {
            const label = String(opts.label || '');
            const row = document.createElement('label');
            row.className = 'ui-toggle';
            const input = document.createElement('input');
            input.type = 'checkbox';
            input.checked = !!opts.checked;
            input.disabled = !!opts.disabled;
            input.className = 'ui-toggle__input';
            const track = document.createElement('span');
            track.className = 'ui-toggle__track';
            const thumb = document.createElement('span');
            thumb.className = 'ui-toggle__thumb';
            track.appendChild(thumb);
            const text = document.createElement('span');
            text.className = 'ui-toggle__label';
            text.textContent = label;
            row.appendChild(input);
            row.appendChild(track);
            row.appendChild(text);
            if (typeof opts.onChange === 'function') {
                input.addEventListener('change', (e) => opts.onChange(e, input.checked));
            }
            return { root: row, input };
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
        root.dataset.filterText = 'version control vcs git repository repo root project root remote origin user email name identity author config enable disable refresh reconnect choose init clone apply status';

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

        const makeSubsection = (title, hint, filterText) => {
            const wrap = document.createElement('div');
            wrap.className = 'ui-settings-subsection';
            wrap.dataset.filterText = String(filterText || '').trim();
            const t = document.createElement('div');
            t.className = 'ui-settings-subsection__title';
            t.textContent = String(title || '');
            wrap.appendChild(t);
            if (hint) {
                const h = document.createElement('div');
                h.className = 'ui-settings-subsection__hint';
                h.textContent = String(hint || '');
                wrap.appendChild(h);
            }
            return wrap;
        };

        const repoGroup = makeGroup(
            'Repository',
            'Choose the Git repository used by this project.',
            'git repository repo root project root vcs enable disable integration refresh reconnect choose root'
        );

        const repoRootInput = createInput({ value: '', placeholder: 'Not detected', disabled: true });
        repoRootInput.style.flex = '1';
        const projectRootInput = createInput({ value: '', placeholder: 'No project', disabled: true });
        projectRootInput.style.flex = '1';

        const doReconnect = async () => {
            try {
                await repoManager?.actions?.reconnect?.();
                hydrateFromState();
                notify('success', 'Git', 'Refreshed.');
            } catch (err) {
                notify('error', 'Git', err?.message || 'Refresh failed');
            }
        };

        const doChooseRoot = async () => {
            try {
                const res = await repoManager?.actions?.chooseRepoRoot?.();
                hydrateFromState();
                if (res?.ok) notify('success', 'Git', 'Repository root updated.');
            } catch (err) {
                notify('error', 'Git', err?.message || 'Choose root failed');
            }
        };

        const doToggleGit = async ({ disable } = {}) => {
            const st = getState();
            const shouldDisable = typeof disable === 'boolean' ? disable : !st.gitDisabled;
            const ok = await showConfirm({
                title: shouldDisable ? 'Disable Git' : 'Enable Git',
                message: shouldDisable
                    ? 'Disable Git integration for this project?'
                    : 'Enable Git integration for this project?',
                variant: shouldDisable ? 'danger' : 'default',
                confirmLabel: shouldDisable ? 'Disable' : 'Enable',
                cancelLabel: 'Cancel'
            });
            if (!ok) return false;
            try {
                if (shouldDisable) await repoManager?.actions?.disableGit?.();
                else await repoManager?.actions?.enableGit?.();
                hydrateFromState();
                return true;
            } catch (err) {
                notify('error', 'Git', err?.message || 'Toggle failed');
                return false;
            }
        };

        const doInitRepo = async () => {
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
        };

        const doCloneRepo = async () => {
            const url = await showPrompt({
                title: 'Clone Repository',
                message: 'Enter repository URL',
                placeholder: 'git@github.com:user/repo.git',
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
        };

        const btnRefresh = createButton({ label: 'Refresh', variant: 'ghost', onClick: doReconnect });
        const btnChooseRoot = createButton({ label: 'Choose Repo Root…', variant: 'ghost', onClick: doChooseRoot });

        const repoActions = document.createElement('div');
        repoActions.style.display = 'flex';
        repoActions.style.flexWrap = 'wrap';
        repoActions.style.gap = '6px';
        repoActions.appendChild(btnChooseRoot);
        repoActions.appendChild(btnRefresh);

        const statusText = document.createElement('div');
        statusText.className = 'ui-settings-group__hint';
        statusText.style.marginTop = '8px';

        const gitToggle = createToggle({
            label: 'Enable Git integration',
            checked: true,
            disabled: false,
            onChange: async (_e, checked) => {
                const ok = await doToggleGit({ disable: !checked });
                if (!ok) {
                    try { gitToggle.input.checked = !checked; } catch (_) { }
                }
            }
        });

        repoGroup.appendChild(makeRow('Git', gitToggle.root, 'enable disable git integration'));
        repoGroup.appendChild(makeRow('Project root', projectRootInput, 'project root'));
        repoGroup.appendChild(makeRow('Repo root', repoRootInput, 'repository root'));
        repoGroup.appendChild(makeRow('Actions', repoActions, 'refresh reconnect choose root'));
        repoGroup.appendChild(statusText);

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
            label: 'Apply',
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
            label: 'Check Status',
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

        cfgGroup.appendChild(makeSubsection('Identity', 'Used for commits in this repository.', 'identity author user name email'));
        cfgGroup.appendChild(makeRow('User name', nameInput, 'user name git config'));
        cfgGroup.appendChild(makeRow('User email', emailInput, 'user email git config'));
        cfgGroup.appendChild(makeSubsection('Remote', 'Origin URL used for push/pull.', 'remote origin url'));
        cfgGroup.appendChild(makeRow('Origin URL', remoteInput, 'remote origin url'));
        cfgGroup.appendChild(makeRow('Actions', cfgActions, 'save test'));

        const advancedGroup = makeGroup(
            'Advanced',
            'Repository setup actions (optional).',
            'advanced init clone repository setup'
        );

        const advancedActions = document.createElement('div');
        advancedActions.style.display = 'flex';
        advancedActions.style.flexWrap = 'wrap';
        advancedActions.style.gap = '6px';

        const btnInit = createButton({ label: 'Init Repo', variant: 'ghost', onClick: doInitRepo });
        const btnClone = createButton({ label: 'Clone…', variant: 'ghost', onClick: doCloneRepo });
        advancedActions.appendChild(btnInit);
        advancedActions.appendChild(btnClone);
        advancedGroup.appendChild(makeRow('Actions', advancedActions, 'init clone'));

        const hydrateFromState = async () => {
            const st = getState();
            const projectRoot = String(st.projectRoot || '').trim();
            const repoRoot = String(st.repoRoot || '').trim();
            const remote = String(st.lastKnownRemote || '').trim();

            projectRootInput.value = projectRoot || '';
            repoRootInput.value = repoRoot || '';

            try {
                gitToggle.input.checked = !st.gitDisabled;
                gitToggle.input.disabled = !st.gitAvailable;
            } catch (_) { }

            const disableOps = !st.gitAvailable || st.gitDisabled;
            try { btnRefresh.disabled = disableOps; } catch (_) { }
            try { btnChooseRoot.disabled = !st.gitAvailable; } catch (_) { }
            try { btnInit.disabled = disableOps; } catch (_) { }
            try { btnClone.disabled = disableOps; } catch (_) { }
            try { btnSave.disabled = disableOps || !repoRoot; } catch (_) { }
            try { btnTest.disabled = disableOps || !repoRoot; } catch (_) { }
            try { nameInput.disabled = disableOps; } catch (_) { }
            try { emailInput.disabled = disableOps; } catch (_) { }
            try { remoteInput.disabled = disableOps; } catch (_) { }

            const flags = [
                st.gitDisabled ? 'Git disabled' : 'Git enabled',
                st.gitAvailable ? 'Git available' : 'Git not available',
                st.repoDetected ? 'Repository detected' : 'No repository detected'
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

        root.appendChild(repoGroup);
        root.appendChild(cfgGroup);
        root.appendChild(advancedGroup);

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
