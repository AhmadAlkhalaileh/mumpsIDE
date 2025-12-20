(() => {
    function createGitRepoManager({ deps } = {}) {
        const showToast = deps?.showToast || (() => { });
        const logger =
            deps?.logger || { debug: () => { }, info: () => { }, warn: () => { }, error: () => { } };
        const getCurrentProject = deps?.getCurrentProject || (() => null);

        const fs = (() => {
            try {
                return require("fs");
            } catch (_) {
                return null;
            }
        })();
        const path = (() => {
            try {
                return require("path");
            } catch (_) {
                return null;
            }
        })();

        const STORAGE_KEY = "ahmadIDE:gitRepoStateByProject";

        const listeners = new Set();
        const watches = [];
        let debounceHandle = null;
        let refreshToken = 0;
        let cachedGitAvailable = null;
        let lastGitAvailableError = "";

        const state = {
            projectRoot: "",
            repoRoot: "",
            repoDetected: false,
            gitAvailable: false,
            lastKnownRemote: "",
            lastError: "",
            gitDisabled: false,
            repoRootOverride: ""
        };

        const emit = () => {
            const snapshot = { ...state };
            listeners.forEach((fn) => {
                try {
                    fn(snapshot);
                } catch (_) { }
            });
        };

        const readAllPersisted = () => {
            try {
                const raw = localStorage.getItem(STORAGE_KEY);
                return raw ? JSON.parse(raw) : {};
            } catch (_) {
                return {};
            }
        };

        const writeAllPersisted = (map) => {
            try {
                localStorage.setItem(STORAGE_KEY, JSON.stringify(map || {}));
            } catch (_) { }
        };

        const persistStateForProject = () => {
            const projectRoot = String(state.projectRoot || "").trim();
            if (!projectRoot) return;
            const all = readAllPersisted();
            all[projectRoot] = {
                repoRoot: String(state.repoRoot || ""),
                repoDetected: !!state.repoDetected,
                gitAvailable: !!state.gitAvailable,
                lastKnownRemote: String(state.lastKnownRemote || ""),
                lastError: String(state.lastError || ""),
                gitDisabled: !!state.gitDisabled,
                repoRootOverride: String(state.repoRootOverride || "")
            };
            writeAllPersisted(all);
        };

        const loadPersistedForProject = (projectRoot) => {
            const all = readAllPersisted();
            const key = String(projectRoot || "").trim();
            const value = all?.[key] || null;
            return value && typeof value === "object" ? value : null;
        };

        const update = (patch) => {
            Object.assign(state, patch || {});
            persistStateForProject();
            emit();
        };

        const safeStat = (p) => {
            if (!fs) return null;
            try {
                return fs.statSync(p);
            } catch (_) {
                return null;
            }
        };

        const safeReaddir = (p) => {
            if (!fs) return null;
            try {
                return fs.readdirSync(p, { withFileTypes: true });
            } catch (_) {
                return null;
            }
        };

        const safeReadText = (p) => {
            if (!fs) return null;
            try {
                return fs.readFileSync(p, "utf8");
            } catch (_) {
                return null;
            }
        };

        const parseGitdirPointer = (text) => {
            const first = String(text || "").split(/\r?\n/).find(Boolean) || "";
            const m = first.match(/^\s*gitdir:\s*(.+)\s*$/i);
            if (!m) return null;
            return String(m[1] || "").trim();
        };

        const resolveGitDirFromDotGitFile = (worktreeRoot, dotGitFilePath) => {
            if (!path) return null;
            const raw = safeReadText(dotGitFilePath);
            const gitdir = parseGitdirPointer(raw);
            if (!gitdir) return null;
            const resolved = path.isAbsolute(gitdir) ? gitdir : path.resolve(worktreeRoot, gitdir);
            const st = safeStat(resolved);
            if (!st || !st.isDirectory()) return null;
            return resolved;
        };

        const resolveCommonGitDir = (gitDir) => {
            if (!path) return null;
            const commonFile = path.join(gitDir, "commondir");
            const raw = safeReadText(commonFile);
            if (!raw) return gitDir;
            const rel = String(raw).trim();
            if (!rel) return gitDir;
            const resolved = path.resolve(gitDir, rel);
            const st = safeStat(resolved);
            return st && st.isDirectory() ? resolved : gitDir;
        };

        // DEBUG: Check fs availability
        if (!fs) console.warn("[GitRepoManager] fs module is NOT available. Git detection will fail.");
        if (!path) console.warn("[GitRepoManager] path module is NOT available.");

        const detectRepoFromRoot = (candidateRoot) => {
            if (!fs || !path) {
                console.warn("[GitRepoManager] detectRepoFromRoot aborted: fs or path missing");
                return null;
            }
            const root = path.resolve(String(candidateRoot || "").trim());
            if (!root) return null;

            const dotGitPath = path.join(root, ".git");
            console.log("[GitRepoManager] Checking for .git at:", dotGitPath);
            const st = safeStat(dotGitPath);
            if (!st) {
                // console.log("[GitRepoManager] .git not found at:", dotGitPath);
                return null;
            }

            if (st.isDirectory()) {
                console.log("[GitRepoManager] Found .git directory at:", dotGitPath);
                return {
                    repoRoot: root,
                    dotGitPath,
                    dotGitType: "dir",
                    gitDir: dotGitPath,
                    commonGitDir: dotGitPath
                };
            }
            if (st.isFile()) {
                console.log("[GitRepoManager] Found .git file (worktree/submodule) at:", dotGitPath);
                const gitDir = resolveGitDirFromDotGitFile(root, dotGitPath);
                if (!gitDir) return null;
                const commonGitDir = resolveCommonGitDir(gitDir);
                return {
                    repoRoot: root,
                    dotGitPath,
                    dotGitType: "file",
                    gitDir,
                    commonGitDir
                };
            }
            return null;
        };

        const buildCandidateRoots = (projectRoot, maxDepth) => {
            if (!path) return [];
            const start = path.resolve(String(projectRoot || "").trim());
            if (!start) return [];
            const depth = Math.max(0, Number.isFinite(Number(maxDepth)) ? Number(maxDepth) : 0);
            const roots = [];
            let cur = start;
            for (let i = 0; i <= depth; i += 1) {
                roots.push(cur);
                const parent = path.dirname(cur);
                if (!parent || parent === cur) break;
                cur = parent;
            }
            return roots;
        };

        const detectRepo = (projectRoot, opts = {}) => {
            const maxUp = Number.isFinite(Number(opts.maxUpDepth)) ? Number(opts.maxUpDepth) : 60;
            const maxDown = Number.isFinite(Number(opts.maxDownDepth)) ? Number(opts.maxDownDepth) : 2;
            const maxDownDirs =
                Number.isFinite(Number(opts.maxDownDirs)) ? Number(opts.maxDownDirs) : 400;

            // Manual override (if set) wins; we still validate it has a .git.
            if (state.repoRootOverride) {
                const forced = detectRepoFromRoot(state.repoRootOverride);
                if (forced) return forced;
            }

            const roots = buildCandidateRoots(projectRoot, maxUp);
            for (const root of roots) {
                const info = detectRepoFromRoot(root);
                if (info) return info;
            }

            // Optional limited-depth downward scan (performance safe) for nested repos.
            if (fs && path && maxDown > 0 && maxDownDirs > 0) {
                const skipNames = new Set([
                    ".git",
                    ".hg",
                    ".svn",
                    ".idea",
                    ".vscode",
                    ".cache",
                    "node_modules",
                    "dist",
                    "build",
                    "out",
                    "target",
                    "vendor"
                ]);
                const start = path.resolve(String(projectRoot || "").trim());
                const queue = [{ dir: start, depth: 0 }];
                let visited = 0;
                while (queue.length) {
                    const { dir, depth } = queue.shift();
                    visited += 1;
                    if (visited > maxDownDirs) break;
                    if (depth > maxDown) continue;

                    if (dir && dir !== start) {
                        const info = detectRepoFromRoot(dir);
                        if (info) return info;
                    }
                    if (depth === maxDown) continue;

                    const entries = safeReaddir(dir) || [];
                    for (const ent of entries) {
                        if (!ent?.isDirectory?.()) continue;
                        const name = String(ent.name || "").trim();
                        if (!name) continue;
                        if (skipNames.has(name)) continue;
                        if (name.startsWith(".")) continue;
                        queue.push({ dir: path.join(dir, name), depth: depth + 1 });
                    }
                }
            }
            return null;
        };

        const detectRepoAsync = async (projectRoot, opts = {}) => {
            // 1. Try sync fs detection
            const syncResult = detectRepo(projectRoot, opts);
            if (syncResult) return syncResult;

            // 2. Try CLI detection
            try {
                if (window.ahmadIDE?.git) {
                    const cmd = `git -C "${String(projectRoot).replace(/"/g, '\\"')}" rev-parse --show-toplevel`;
                    const res = await window.ahmadIDE.git(cmd);
                    if (res && (res.ok || res.code === 0) && res.stdout) {
                        const root = String(res.stdout).trim();
                        if (root) {
                            console.log("[GitRepoManager] Detected repo via CLI at:", root);
                            return {
                                repoRoot: root,
                                dotGitPath: path ? path.join(root, '.git') : '',
                                dotGitType: 'unknown',
                                gitDir: '',
                                commonGitDir: ''
                            };
                        }
                    }
                }
            } catch (e) {
                console.warn("[GitRepoManager] CLI detection failed:", e);
            }
            return null;
        };

        // ===================== FIX 1: runGit prefixes "git" + always scopes correctly
        const runGit = async (cmd, opts = {}) => {
            let command = String(cmd || "").trim();
            if (!command) return { ok: false, error: "No git command provided" };
            if (state.gitDisabled) return { ok: false, error: "Git is disabled for this project" };
            if (!window.ahmadIDE?.git) return { ok: false, error: "Git API unavailable" };

            // Prefix if missing
            if (!/^git(\s|$)/i.test(command)) command = `git ${command}`;

            const repoRoot = String(opts.repoRoot || state.repoRoot || "").trim();

            // Add -C if we have repoRoot and command doesn't already include it
            const hasC = /^git\s+-C\s+/i.test(command);
            const finalCmd =
                repoRoot && !hasC
                    ? `git -C "${repoRoot.replace(/"/g, '\\"')}" ${command.replace(/^git\s+/i, "")}`
                    : command;

            const res = await window.ahmadIDE.git(finalCmd);

            // Normalize result (FIX 2: accept code/exitCode)
            const ok = res?.ok === true || res?.code === 0 || res?.exitCode === 0;
            if (ok) return { ...res, ok: true };

            const msg = String(res?.error || res?.stderr || "Git command failed").trim();
            return { ...res, ok: false, error: msg };
        };

        // ===================== FIX 2: checkGitAvailable accepts ok/code/exitCode
        const checkGitAvailable = async () => {
            if (cachedGitAvailable === true) return { ok: true, error: "" };
            try {
                if (!window.ahmadIDE?.git) {
                    lastGitAvailableError = "Git API unavailable";
                    return { ok: false, error: "Git API unavailable" };
                }
                const res = await window.ahmadIDE.git("git --version");
                const ok = res?.ok === true || res?.code === 0 || res?.exitCode === 0;
                if (ok) {
                    cachedGitAvailable = true;
                    lastGitAvailableError = "";
                    return { ok: true, error: "" };
                }
                const msg = String(res?.error || res?.stderr || "git --version failed").trim();
                lastGitAvailableError = msg;
                return { ok: false, error: msg };
            } catch (err) {
                const msg = String(err?.message || err || "git --version failed").trim();
                lastGitAvailableError = msg;
                return { ok: false, error: msg };
            }
        };

        const detectRemote = async (repoRoot) => {
            if (!repoRoot) return "";
            try {
                if (window.ahmadIDE?.getGitConfig) {
                    const cfg = await window.ahmadIDE.getGitConfig(repoRoot);
                    if (cfg?.ok && cfg.remote?.origin) return String(cfg.remote.origin).trim();
                }
            } catch (_) { }

            const res = await runGit("git remote get-url origin", { repoRoot });
            if (res?.ok && res.stdout) return String(res.stdout).trim();
            return "";
        };

        const stopWatching = () => {
            if (debounceHandle) {
                clearTimeout(debounceHandle);
                debounceHandle = null;
            }
            while (watches.length) {
                const w = watches.pop();
                try {
                    w?.();
                } catch (_) { }
            }
        };

        const scheduleReconnect = (reason) => {
            if (state.gitDisabled) return;
            if (debounceHandle) clearTimeout(debounceHandle);
            debounceHandle = setTimeout(() => {
                debounceHandle = null;
                reconnect({ reason }).catch(() => { });
            }, 250);
        };

        const watchFs = (targetPath, opts, handler) => {
            if (!fs || typeof fs.watch !== "function") return;
            try {
                const watcher = fs.watch(targetPath, opts, handler);
                watches.push(() => {
                    try {
                        watcher.close();
                    } catch (_) { }
                });
            } catch (err) {
                logger.warn("GIT_WATCH_FAIL", { targetPath, message: err?.message });
            }
        };

        const startWatching = (repoInfo) => {
            stopWatching();
            if (!fs || !path) return;
            const projectRoot = String(state.projectRoot || "").trim();
            const repoRoot = String(repoInfo?.repoRoot || "").trim();
            if (!projectRoot) return;

            watchFs(projectRoot, { persistent: false }, (_evt, filename) => {
                const name = filename ? String(filename) : "";
                if (!name || name === ".git") scheduleReconnect("projectRoot:.git");
            });

            if (repoRoot && repoRoot !== projectRoot) {
                watchFs(repoRoot, { persistent: false }, (_evt, filename) => {
                    const name = filename ? String(filename) : "";
                    if (!name || name === ".git") scheduleReconnect("repoRoot:.git");
                });
            }

            if (!repoInfo) return;

            const dotGitPath = repoInfo.dotGitPath;
            const gitDir = repoInfo.gitDir;
            const commonGitDir = repoInfo.commonGitDir;

            if (repoInfo.dotGitType === "file" && dotGitPath) {
                watchFs(dotGitPath, { persistent: false }, () => scheduleReconnect("dotgit:file"));
            }

            const headPath = gitDir ? path.join(gitDir, "HEAD") : "";
            if (headPath) watchFs(headPath, { persistent: false }, () => scheduleReconnect("HEAD"));

            const packedRefs = commonGitDir ? path.join(commonGitDir, "packed-refs") : "";
            if (packedRefs) watchFs(packedRefs, { persistent: false }, () => scheduleReconnect("packed-refs"));

            const refsDir = commonGitDir ? path.join(commonGitDir, "refs") : "";
            if (refsDir && safeStat(refsDir)?.isDirectory()) {
                watchFs(refsDir, { persistent: false }, () => scheduleReconnect("refs"));
                ["heads", "remotes", "tags"].forEach((sub) => {
                    const p = path.join(refsDir, sub);
                    if (safeStat(p)?.isDirectory()) watchFs(p, { persistent: false }, () => scheduleReconnect(`refs/${sub}`));
                });
            }
        };

        const refresh = async ({ reason } = {}) => {
            const token = ++refreshToken;
            const projectRoot = String(state.projectRoot || "").trim();
            if (!projectRoot) return;

            try {
                if (state.gitDisabled) {
                    update({ repoDetected: false, repoRoot: "", lastKnownRemote: "", lastError: "Git is disabled for this project" });
                    stopWatching();
                    return;
                }

                const gitCheck = await checkGitAvailable();
                if (token !== refreshToken) return;

                if (!gitCheck.ok) {
                    update({
                        gitAvailable: false,
                        repoDetected: false,
                        repoRoot: "",
                        lastKnownRemote: "",
                        lastError: gitCheck.error || lastGitAvailableError || "Git is not available (git --version failed)"
                    });
                    stopWatching();
                    return;
                }

                const repoInfo = await detectRepoAsync(projectRoot, { maxUpDepth: 60, maxDownDepth: 2, maxDownDirs: 400 });
                if (token !== refreshToken) return;

                if (!repoInfo) {
                    update({
                        gitAvailable: true,
                        repoDetected: false,
                        repoRoot: "",
                        lastKnownRemote: "",
                        lastError: "No Git repository detected"
                    });
                    startWatching(null);
                    return;
                }

                // If we found it via CLI, we might skip the internal probe or assume it's good.
                // But let's run the probe anyway to be sure.
                const probe = await runGit("git rev-parse --is-inside-work-tree", { repoRoot: repoInfo.repoRoot });
                if (token !== refreshToken) return;

                if (!probe?.ok) {
                    const msg = String(probe?.error || probe?.stderr || "Git repo detected but unusable").trim();
                    update({
                        gitAvailable: true,
                        repoDetected: false,
                        repoRoot: "",
                        lastKnownRemote: "",
                        lastError: msg || "Git repo detected but unusable"
                    });
                    startWatching(repoInfo); // still watch for changes
                    return;
                }

                const lastKnownRemote = await detectRemote(repoInfo.repoRoot);
                if (token !== refreshToken) return;

                update({
                    gitAvailable: true,
                    repoDetected: true,
                    repoRoot: repoInfo.repoRoot,
                    lastKnownRemote: lastKnownRemote || state.lastKnownRemote || "",
                    lastError: ""
                });

                startWatching(repoInfo);
                logger.info("GIT_REPO_REFRESH", { reason: reason || "manual", repoRoot: repoInfo.repoRoot, remote: lastKnownRemote });
            } catch (err) {
                if (token !== refreshToken) return;
                const msg = String(err?.message || err || "Git refresh failed").trim() || "Git refresh failed";
                update({
                    gitAvailable: false,
                    repoDetected: false,
                    repoRoot: "",
                    lastKnownRemote: "",
                    lastError: msg
                });
                stopWatching();
            }
        };

        const setProject = async (projectRoot) => {
            console.log("[GitRepoManager] setProject called with:", projectRoot);
            const root = String(projectRoot || "").trim();
            stopWatching();
            update({
                projectRoot: root,
                repoRoot: "",
                repoDetected: false,
                lastKnownRemote: "",
                lastError: "",
                gitDisabled: false,
                repoRootOverride: ""
            });

            const persisted = loadPersistedForProject(root);
            if (persisted) {
                update({
                    repoRoot: String(persisted.repoRoot || ""),
                    repoDetected: !!persisted.repoDetected,
                    gitAvailable: !!persisted.gitAvailable,
                    lastKnownRemote: String(persisted.lastKnownRemote || ""),
                    lastError: String(persisted.lastError || ""),
                    gitDisabled: !!persisted.gitDisabled,
                    repoRootOverride: String(persisted.repoRootOverride || "")
                });
            }

            await refresh({ reason: "project-open" });
        };

        const reconnect = async ({ reason } = {}) => {
            await refresh({ reason: reason || "reconnect" });
        };

        // ===================== FIX 4: don't persist repoRootOverride when folder isn't a repo
        const connectExisting = async ({ repoRoot } = {}) => {
            const root = String(repoRoot || "").trim();
            if (!root) return { ok: false, error: "No repo root provided" };
            if (!fs || !path) return { ok: false, error: "Filesystem API unavailable" };

            const resolved = path.resolve(root);
            const info = detectRepoFromRoot(resolved);

            if (!info) {
                const msg = "Selected folder is not a Git repository (no .git found)";
                update({ repoDetected: false, repoRoot: "", lastError: msg });
                return { ok: false, error: msg };
            }

            update({ repoRootOverride: resolved });
            await reconnect({ reason: "connect-existing" });
            return { ok: true, repoRoot: resolved };
        };

        const chooseRepoRoot = async () => {
            if (!window.ahmadIDE?.openFolderDialog) return { ok: false, error: "Folder picker unavailable" };
            const res = await window.ahmadIDE.openFolderDialog();
            if (!res?.ok || !res.path) return { ok: false, canceled: true };
            return connectExisting({ repoRoot: res.path });
        };

        const disableGit = async () => {
            update({ gitDisabled: true });
            stopWatching();
            showToast("info", "Git", "Git disabled for this project");
            return { ok: true };
        };

        const enableGit = async () => {
            update({ gitDisabled: false });
            await reconnect({ reason: "enable" });
            showToast("info", "Git", "Git enabled for this project");
            return { ok: true };
        };

        const initializeRepo = async ({ root } = {}) => {
            const projectRoot = String(state.projectRoot || "").trim();
            const target = String(root || projectRoot).trim();
            if (!target) return { ok: false, error: "No target directory" };
            const cmd = `git -C "${target.replace(/"/g, '\\"')}" init`;
            const res = await runGit(cmd, { repoRoot: "" });
            if (res?.ok) {
                showToast("info", "Git", "Repository initialized");
                await reconnect({ reason: "init" });
                return { ok: true };
            }
            update({ lastError: res?.error || res?.stderr || "git init failed" });
            return { ok: false, error: res?.error || res?.stderr || "git init failed" };
        };

        const cloneRepo = async ({ url, directory } = {}) => {
            const u = String(url || "").trim();
            const dir = String(directory || "").trim();
            if (!u) return { ok: false, error: "Clone URL required" };
            if (!dir) return { ok: false, error: "Target directory required" };
            const cmd = `git clone "${u.replace(/"/g, '\\"')}" "${dir.replace(/"/g, '\\"')}"`;
            const res = await runGit(cmd, { repoRoot: "" });
            if (res?.ok) return { ok: true, stdout: res.stdout, stderr: res.stderr };
            return { ok: false, error: res?.error || res?.stderr || "git clone failed" };
        };

        const getState = () => ({ ...state });

        const subscribe = (fn) => {
            if (typeof fn !== "function") return () => { };
            listeners.add(fn);
            try {
                fn({ ...state });
            } catch (_) { }
            return () => listeners.delete(fn);
        };

        const boot = async () => {
            const project = getCurrentProject();
            const root = project?.projectPath ? String(project.projectPath) : "";
            if (root) await setProject(root);
        };

        boot().catch(() => { });

        return {
            getState,
            subscribe,
            setProject,
            refresh,
            reconnect,
            runGit,
            actions: {
                initializeRepo,
                cloneRepo,
                connectExisting,
                reconnect,
                chooseRepoRoot,
                disableGit,
                enableGit
            }
        };
    }

    if (typeof window !== "undefined") {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.git = window.AhmadIDEModules.git || {};
        window.AhmadIDEModules.git.createGitRepoManager = createGitRepoManager;
    }
})();
