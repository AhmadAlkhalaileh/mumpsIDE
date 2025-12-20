(() => {
    function createGitSettingsManager({ deps } = {}) {
        const $ =
            deps?.$ ||
            (typeof window !== "undefined" ? window.$ || window.jQuery || null : null);

        const showToast = deps?.showToast || (() => { });
        const getCurrentProject = deps?.getCurrentProject || (() => null);

        // FIX: fallback runGit supports opts.repoRoot (your original fallback ignored it)
        const runGitApi =
            deps?.runGit ||
            ((cmd, opts = {}) => {
                if (!window.ahmadIDE?.git) {
                    return Promise.resolve({ ok: false, error: "Git API unavailable" });
                }

                const repoRoot = String(opts.repoRoot || "").trim();
                let command = String(cmd || "").trim();
                if (!command) return Promise.resolve({ ok: false, error: "No git command provided" });

                // Ensure command starts with git
                if (!/^git(\s|$)/i.test(command)) command = `git ${command}`;

                // Scope with -C if repoRoot provided and command not already scoped
                const hasC = /^git\s+-C\s+/i.test(command);
                const finalCmd =
                    repoRoot && !hasC
                        ? `git -C "${repoRoot.replace(/"/g, '\\"')}" ${command.replace(/^git\s+/i, "")}`
                        : command;

                return window.ahmadIDE.git(finalCmd);
            });

        const getGitRepoState = deps?.getGitRepoState || (() => null);

        if (!$) {
            throw new Error("createGitSettingsManager requires jQuery ($)");
        }

        let lazyHooked = false;
        let bound = false;

        function bindGitSettingsPanel() {
            if (bound) return;

            const saveBtn = $("#gitSettingsSave");
            const testBtn = $("#gitSettingsTest");
            const statusEl = $("#gitSettingsStatus");
            const nameInput = $("#gitUserName");
            const emailInput = $("#gitUserEmail");
            const remoteInput = $("#gitRemoteUrl");

            if (!saveBtn.length || !statusEl.length) {
                // Settings panel may be lazy-mounted.
                if (!lazyHooked) {
                    const fr = window.AhmadIDEModules?.app?.featureRegistry;
                    fr?.onMounted?.("settingsPanel", () => {
                        // allow rebinding when panel mounts
                        lazyHooked = false;
                        bindGitSettingsPanel();
                    });
                    lazyHooked = true;
                }
                return;
            }

            bound = true;

            // Determine repoRoot once and reuse for all repo-scoped commands
            const currentProject = getCurrentProject();
            const repoState = getGitRepoState?.() || {};
            const repoRoot = String(repoState.repoRoot || currentProject?.projectPath || "").trim();

            // Wrapper that ALWAYS passes repoRoot unless caller overrides it
            const runGit = async (cmd, opts = {}) => {
                try {
                    const res = await runGitApi(cmd, { repoRoot, ...opts });

                    // Normalize success in case bridge returns exitCode/code instead of ok
                    const ok = res?.ok === true || res?.exitCode === 0 || res?.code === 0;
                    if (!ok) {
                        const msg = String(res?.error || res?.stderr || "Git command failed").trim();
                        showToast("error", "Git", msg || "Git command failed");
                        statusEl.text(`Error: ${msg || "Git command failed"}`);
                        return false;
                    }

                    return { ...res, ok: true };
                } catch (e) {
                    const msg = String(e?.message || e || "Git command failed").trim();
                    showToast("error", "Git", msg);
                    statusEl.text(`Error: ${msg}`);
                    return false;
                }
            };

            // Prefill from project .git/config or global/home git config
            (async () => {
                // Try project config first (if project open and repoRoot known)
                if (repoRoot) {
                    let hydrated = false;

                    try {
                        if (window.ahmadIDE?.getGitConfig) {
                            const gitConfig = await window.ahmadIDE.getGitConfig(repoRoot);
                            if (gitConfig?.ok) {
                                if (gitConfig.user?.name) nameInput.val(gitConfig.user.name);
                                if (gitConfig.user?.email) emailInput.val(gitConfig.user.email);
                                if (gitConfig.remote?.origin) remoteInput.val(gitConfig.remote.origin);
                                hydrated = true;
                            }
                        }
                    } catch (_) { }

                    // Fallback using git commands (repo-scoped)
                    try {
                        if (!String(nameInput.val() || "").trim()) {
                            const r = await runGit("git config user.name");
                            if (r?.ok && r.stdout) nameInput.val(String(r.stdout).trim());
                        }
                        if (!String(emailInput.val() || "").trim()) {
                            const r = await runGit("git config user.email");
                            if (r?.ok && r.stdout) emailInput.val(String(r.stdout).trim());
                        }
                        if (!String(remoteInput.val() || "").trim()) {
                            const remote = String(repoState.lastKnownRemote || "").trim();
                            if (remote) remoteInput.val(remote);
                            else {
                                const r = await runGit("git remote get-url origin");
                                if (r?.ok && r.stdout) remoteInput.val(String(r.stdout).trim());
                            }
                        }

                        // If we have a repo detected or hydrated from project config, don't load global.
                        if (repoState.repoDetected || hydrated) return;
                    } catch (_) {
                        // fall through to global
                    }
                }

                // Global git config fallback
                const resUser = await runGitApi("git config --global user.name");
                if ((resUser?.ok === true || resUser?.exitCode === 0 || resUser?.code === 0) && resUser.stdout) {
                    nameInput.val(String(resUser.stdout).trim());
                }

                const resEmail = await runGitApi("git config --global user.email");
                if ((resEmail?.ok === true || resEmail?.exitCode === 0 || resEmail?.code === 0) && resEmail.stdout) {
                    emailInput.val(String(resEmail.stdout).trim());
                }
            })();

            saveBtn.on("click", async () => {
                const name = String(nameInput.val() || "").trim();
                const email = String(emailInput.val() || "").trim();
                const remote = String(remoteInput.val() || "").trim();

                const cmds = [];

                if (name) cmds.push(`git config user.name "${name.replace(/"/g, '\\"')}"`);
                if (email) cmds.push(`git config user.email "${email.replace(/"/g, '\\"')}"`);

                statusEl.text("Applying...");

                // Apply name/email
                for (const cmd of cmds) {
                    const ok = await runGit(cmd);
                    if (!ok) return;
                }

                // FIX: do NOT use shell operator "||" (breaks when main uses spawn shell:false)
                if (remote) {
                    const escRemote = remote.replace(/"/g, '\\"');

                    let r = await runGit(`git remote set-url origin "${escRemote}"`);
                    if (!r) return;

                    if (!r.ok) {
                        // If origin doesn't exist, add it
                        r = await runGit(`git remote add origin "${escRemote}"`);
                        if (!r) return;
                    }
                }

                statusEl.text("Git settings applied");
                showToast("info", "Git", "Git settings applied");
            });

            testBtn.on("click", async () => {
                statusEl.text("Testing...");
                const res = await runGit("git status --short");
                if (res && res.ok) {
                    statusEl.text(res.stdout ? String(res.stdout).trim() : "Clean status");
                    showToast("info", "Git", "Status ok");
                }
            });
        }

        return {
            bindGitSettingsPanel
        };
    }

    if (typeof window !== "undefined") {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.git = window.AhmadIDEModules.git || {};
        window.AhmadIDEModules.git.createGitSettingsManager = createGitSettingsManager;
    }
})();
