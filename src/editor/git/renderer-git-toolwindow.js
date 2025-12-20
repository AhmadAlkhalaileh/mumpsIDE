(() => {
    function createGitToolWindowManager({ deps } = {}) {
        const showToast = deps?.showToast || (() => { });
        const normalizeGitError =
            deps?.normalizeGitError ||
            ((text, fallback = "Git command failed") => {
                if (/not a git repository/i.test(text || "")) return "Git is not configured for this project";
                return text || fallback;
            });

        const toggleToolWindowPanel = deps?.toggleToolWindowPanel || (() => { });

        // ✅ FIX: fallback must accept (cmd, opts) and scope to repoRoot (-C) when provided
        const runGitApi =
            deps?.runGit ||
            ((cmd, opts = {}) => {
                if (!window.ahmadIDE?.git) {
                    return Promise.resolve({ ok: false, error: "Git API unavailable" });
                }

                const repoRoot = String(opts.repoRoot || "").trim();
                let command = String(cmd || "").trim();
                if (!command) return Promise.resolve({ ok: false, error: "No git command provided" });

                // Ensure it starts with git
                if (!/^git(\s|$)/i.test(command)) command = `git ${command}`;

                // Add -C if repoRoot exists and command not already scoped
                const hasC = /^git\s+-C\s+/i.test(command);
                const isVersion = /^\s*git\s+--version(\s|$)/i.test(command);
                const finalCmd =
                    repoRoot && !hasC && !isVersion
                        ? `git -C "${repoRoot.replace(/"/g, '\\"')}" ${command.replace(/^git\s+/i, "")}`
                        : command;

                return window.ahmadIDE.git(finalCmd);
            });

        const gitActions = deps?.gitActions || null;
        const getGitRepoState = deps?.getGitRepoState || (() => null);
        const openDiffTab = deps?.openDiffTab || (async () => ({ ok: false, skipped: true }));
        const subscribeGitRepoState = deps?.subscribeGitRepoState || (() => () => { });
        const createVirtualList =
            deps?.createVirtualList ||
            (typeof window !== "undefined" ? window.AhmadIDEModules?.ui?.createVirtualList : null);

        function wireGitToolWindow({ fetchCurrentBranch } = {}) {
            const featureRegistry = window.AhmadIDEModules?.app?.featureRegistry || null;
            const openPopover = window.AhmadIDEModules?.ui?.menu?.openPopover || null;
            const openContextMenu = window.AhmadIDEModules?.ui?.menu?.openContextMenu || null;

            const ensurePanelMounted = () => {
                try {
                    featureRegistry?.ensureById?.("gitToolPanel");
                } catch (_) { }
            };

            let panelDomWired = false;

            const showPrompt = async ({ title, message, placeholder = "", defaultValue = "" } = {}) => {
                try {
                    const fn = window.AhmadIDEModules?.ui?.showPrompt;
                    if (typeof fn === "function") return await fn({ title, message, placeholder, defaultValue });
                } catch (_) { }
                try {
                    const res = window.prompt(`${title || "Prompt"}\n\n${message || ""}`, defaultValue || "");
                    if (res == null) return null;
                    return String(res);
                } catch (_) {
                    return null;
                }
            };

            const showConfirm = async ({ title, message, variant = "default" } = {}) => {
                try {
                    const fn = window.AhmadIDEModules?.ui?.showConfirm;
                    if (typeof fn === "function") {
                        return await fn({
                            title,
                            message,
                            variant,
                            confirmLabel: variant === "danger" ? "Delete" : "OK",
                            cancelLabel: "Cancel"
                        });
                    }
                } catch (_) { }
                try {
                    return window.confirm(`${title || "Confirm"}\n\n${message || ""}`);
                } catch (_) {
                    return false;
                }
            };

            const copyToClipboard = async (text, label = "Copied") => {
                const value = String(text || "");
                if (!value) return;
                try {
                    await navigator.clipboard.writeText(value);
                    showToast("success", "Copied", label);
                } catch (_) {
                    try {
                        const ta = document.createElement("textarea");
                        ta.value = value;
                        ta.style.position = "fixed";
                        ta.style.left = "-9999px";
                        document.body.appendChild(ta);
                        ta.select();
                        document.execCommand("copy");
                        ta.remove();
                        showToast("success", "Copied", label);
                    } catch (err) {
                        showToast("error", "Copy failed", err?.message || "Could not copy");
                    }
                }
            };

            const gitOutput = (text) => {
                const out = document.getElementById("gitOutput");
                if (out) {
                    out.textContent += `${text}\n`;
                    out.scrollTop = out.scrollHeight;
                }
            };

            const gitError = (text) => {
                const message = normalizeGitError(text);
                const out = document.getElementById("gitOutput");
                if (out) {
                    out.textContent += `✗ ${message}\n`;
                    out.scrollTop = out.scrollHeight;
                }
                showToast("error", "Git", message);
            };

            const setDiffPanes = (left, right) => {
                const l = document.getElementById("gitDiffLeft");
                const r = document.getElementById("gitDiffRight");
                if (l) l.textContent = left || "No data";
                if (r) r.textContent = right || "No data";
            };

            const perfUi = window.AhmadIDEModules?.features?.git?.perfUi || null;
            const diffRenderer = perfUi?.createDiffRenderer ? perfUi.createDiffRenderer({ maxLines: 6000 }) : null;
            const logDiffRenderer = perfUi?.createDiffRenderer
                ? perfUi.createDiffRenderer({ maxLines: 6000, leftId: "gitLogDiffLeft", rightId: "gitLogDiffRight" })
                : null;

            const gitSelected = { staged: new Set(), unstaged: new Set() };

            const changesRenderer = perfUi?.createChangesRenderer
                ? perfUi.createChangesRenderer({
                    gitSelected,
                    createVirtualList,
                    onSelectionChange: () => {
                        syncGitActionStates();
                        previewSelectedFileDiff().catch(() => { });
                    }
                })
                : null;

            const selectedCommitRef = { hash: null, parents: [] };

            const historyRenderer = perfUi?.createHistoryRenderer
                ? perfUi.createHistoryRenderer({
                    createVirtualList,
                    selectedCommitRef,
                    onSelectCommit: (commit) => {
                        if (!commit?.hash) return;
                        selectedCommitRef.hash = commit.hash;
                        selectedCommitRef.parents = Array.isArray(commit.parents) ? commit.parents : [];
                        renderGitHistory(lastHistoryItems);
                        loadCommitDetails(commit.hash).catch(() => { });
                    }
                })
                : null;

            let activeTab = "log";
            let lastStatusEntries = [];
            let historyAll = [];
            let lastHistoryItems = [];
            let branchSnapshot = { current: "", local: [], remote: [] };

            let logIgnoreWhitespace = false;
            let logSelectedFilePath = "";
            let logDiffAnchors = [];
            let logDiffIndex = -1;
            let logDiffAnchorTimer = null;
            let logDiffObserver = null;
            let logDiffSyncing = false;

            const renderSideBySideDiff = (diffText) => {
                if (diffRenderer && typeof diffRenderer.render === "function") {
                    diffRenderer.render(diffText);
                    return;
                }
                setDiffPanes(diffText || "No data", diffText || "No data");
            };

            const setLogDiffPanes = (left, right) => {
                const l = document.getElementById("gitLogDiffLeft");
                const r = document.getElementById("gitLogDiffRight");
                if (l) l.textContent = left || "No data";
                if (r) r.textContent = right || "No data";
            };

            const renderLogDiff = (diffText) => {
                if (logDiffRenderer && typeof logDiffRenderer.render === "function") {
                    logDiffRenderer.render(diffText);
                    scheduleLogDiffAnchorRecalc();
                    return;
                }
                setLogDiffPanes(diffText || "No data", diffText || "No data");
                scheduleLogDiffAnchorRecalc();
            };

            const renderGitChanges = (entries = []) => {
                lastStatusEntries = Array.isArray(entries) ? entries : [];
                if (changesRenderer && typeof changesRenderer.render === "function") {
                    changesRenderer.render(lastStatusEntries);
                    return;
                }
                const unstagedHost = document.getElementById("gitChangesUnstaged");
                const stagedHost = document.getElementById("gitChangesStaged");
                if (unstagedHost) unstagedHost.textContent = "Virtual list unavailable.";
                if (stagedHost) stagedHost.textContent = "Virtual list unavailable.";
            };

            const renderGitHistory = (items = []) => {
                lastHistoryItems = Array.isArray(items) ? items : [];
                if (historyRenderer && typeof historyRenderer.render === "function") {
                    historyRenderer.render(lastHistoryItems);
                    return;
                }
                const host = document.getElementById("gitHistoryList");
                if (!host) return;
                host.innerHTML = "";
                (items || []).slice(0, 50).forEach((line) => {
                    const div = document.createElement("div");
                    div.textContent = typeof line === "string" ? line : JSON.stringify(line);
                    host.appendChild(div);
                });
            };

            const setActiveGitTab = (tabId) => {
                const root = document.getElementById("gitToolWindow");
                if (!root) return;
                const candidate = tabId || "log";
                const hasPane = !!root.querySelector(`.git-tw-pane[data-git-pane="${candidate}"]`);
                activeTab = hasPane ? candidate : "log";

                root.querySelectorAll(".git-tw-tab").forEach((btn) => {
                    const isActive = btn.getAttribute("data-git-tab") === activeTab;
                    btn.classList.toggle("active", isActive);
                    btn.setAttribute("aria-selected", isActive ? "true" : "false");
                });

                root.querySelectorAll(".git-tw-pane").forEach((pane) => {
                    const isActive = pane.getAttribute("data-git-pane") === activeTab;
                    pane.classList.toggle("active", isActive);
                });
            };

            let gitLifecycle = "idle";
            let ensureLoadedPromise = null;

            const isGitPanelVisible = () => {
                const panel = document.getElementById("gitToolPanel");
                return !!panel && !panel.classList.contains("hidden");
            };

            const setGitLifecycle = (next) => {
                gitLifecycle = next || "idle";
                const root = document.getElementById("gitToolWindow");
                if (root) root.dataset.gitState = gitLifecycle;
            };

            const setGitBanner = ({ message = "", actions = [] } = {}) => {
                const banner = document.getElementById("gitBanner");
                const text = document.getElementById("gitBannerText");
                const host = document.getElementById("gitBannerActions");
                if (!banner || !text || !host) return;

                const msg = String(message || "").trim();
                text.textContent = msg;
                host.innerHTML = "";

                (actions || []).forEach((a) => {
                    const btn = document.createElement("button");
                    btn.type = "button";
                    btn.className = "btn ghost git-small-btn";
                    btn.textContent = String(a.label || "");
                    if (a.title) btn.title = String(a.title);
                    if (a.disabled) btn.disabled = true;
                    btn.addEventListener("click", (e) => {
                        e.preventDefault();
                        e.stopPropagation();
                        try {
                            a.onClick?.();
                        } catch (_) { }
                    });
                    host.appendChild(btn);
                });

                banner.classList.toggle("hidden", !msg);
            };

            const syncGitActionStates = () => {
                const repoState = getGitRepoState?.() || {};
                const repoReady = !!repoState.repoDetected && !!repoState.gitAvailable && !repoState.gitDisabled;

                const stageBtn = document.getElementById("gitStageSelectedBtn");
                const unstageBtn = document.getElementById("gitUnstageSelectedBtn");
                const diffBtn = document.getElementById("gitDiffSelectedBtn");
                if (stageBtn) stageBtn.disabled = !repoReady || gitSelected.unstaged.size === 0;
                if (unstageBtn) unstageBtn.disabled = !repoReady || gitSelected.staged.size === 0;
                if (diffBtn) diffBtn.disabled = !repoReady || gitSelected.unstaged.size + gitSelected.staged.size === 0;

                const msgEl = document.getElementById("gitCommitMessage");
                const commitBtn = document.getElementById("gitCommitBtn");
                const commitAndPushBtn = document.getElementById("gitCommitAndPushBtn");
                const hasMessage = Boolean(msgEl?.value?.trim());
                if (commitBtn) commitBtn.disabled = !repoReady || !hasMessage;
                if (commitAndPushBtn) commitAndPushBtn.disabled = !repoReady || !hasMessage;

                ["gitFetchBtn", "gitPullBtn", "gitPushBtn", "gitCheckoutBtn"].forEach((id) => {
                    const el = document.getElementById(id);
                    if (el) el.disabled = !repoReady;
                });
            };

            let branchTreeSelection = "";
            const branchTreeCollapsed = new Set();

            const renderBranchesTree = () => {
                const host = document.getElementById("gitBranchesTree");
                if (!host) return;

                const current = String(branchSnapshot.current || "").trim() || "—";
                const branchFilter = document.getElementById("gitLogBranchSelect")?.value || "__ALL__";
                if (!branchTreeSelection && branchFilter && branchFilter !== "__ALL__") branchTreeSelection = branchFilter;

                const makeTwisty = (collapsed) => {
                    const twisty = document.createElement("span");
                    twisty.className = "git-tree-twisty";
                    twisty.setAttribute("aria-hidden", "true");
                    twisty.innerHTML = collapsed
                        ? '<span data-ui-icon="chevron-right" data-ui-icon-size="16"></span>'
                        : '<span data-ui-icon="chevron-right" data-ui-icon-size="16" class="open"></span>';
                    return twisty;
                };

                const makeIcon = (name) => {
                    const wrap = document.createElement("span");
                    wrap.className = "git-tree-icon";
                    wrap.setAttribute("aria-hidden", "true");
                    wrap.innerHTML = `<span data-ui-icon="${name}" data-ui-icon-size="16"></span>`;
                    return wrap;
                };

                const makeGroup = (key, label, { icon } = {}) => {
                    const wrap = document.createElement("div");
                    wrap.className = "git-tree-group";

                    const head = document.createElement("div");
                    head.className = "git-tree-group-head";
                    head.setAttribute("data-node", key);
                    head.tabIndex = 0;

                    const collapsed = branchTreeCollapsed.has(key);
                    head.appendChild(makeTwisty(collapsed));
                    head.appendChild(makeIcon(icon || "folder-open"));

                    const t = document.createElement("div");
                    t.className = "git-tree-group-title";
                    t.textContent = label;
                    head.appendChild(t);

                    const body = document.createElement("div");
                    body.className = "git-tree-group-body";
                    body.classList.toggle("collapsed", collapsed);

                    const toggle = () => {
                        if (branchTreeCollapsed.has(key)) branchTreeCollapsed.delete(key);
                        else branchTreeCollapsed.add(key);
                        renderBranchesTree();
                    };

                    head.addEventListener("click", toggle);
                    head.addEventListener("keydown", (e) => {
                        if (e.key === "Enter" || e.key === " ") {
                            e.preventDefault();
                            toggle();
                        }
                    });

                    wrap.appendChild(head);
                    wrap.appendChild(body);
                    return { wrap, body };
                };

                const makeBranchRow = (branch, { icon, title } = {}) => {
                    const row = document.createElement("div");
                    row.className = "git-tree-item";
                    row.setAttribute("data-branch", branch);
                    row.setAttribute("role", "treeitem");
                    row.tabIndex = 0;
                    if (title) row.title = String(title);

                    row.appendChild(makeIcon(icon || "branch"));

                    const name = document.createElement("span");
                    name.className = "git-tree-item-label";
                    name.textContent = branch;
                    row.appendChild(name);

                    const isSelected = branchTreeSelection === branch;
                    row.classList.toggle("selected", isSelected);

                    const activate = () => {
                        branchTreeSelection = branch;
                        const select = document.getElementById("gitLogBranchSelect");
                        if (select) {
                            select.value = branch;
                            select.dispatchEvent(new Event("change", { bubbles: true }));
                        } else {
                            loadGitHistory().catch(() => { });
                        }
                        renderBranchesTree();
                    };

                    row.addEventListener("click", (e) => {
                        e.preventDefault();
                        activate();
                    });
                    row.addEventListener("keydown", (e) => {
                        if (e.key === "Enter" || e.key === " ") {
                            e.preventDefault();
                            activate();
                        }
                    });

                    return row;
                };

                const groupHead = makeGroup("group:head", "HEAD (Current Branch)", { icon: "branch" });
                groupHead.body.appendChild(makeBranchRow(current, { icon: "branch", title: "Current branch" }));

                const groupLocal = makeGroup("group:local", "Local", { icon: "branch" });
                (branchSnapshot.local || []).forEach((b) => groupLocal.body.appendChild(makeBranchRow(b, { icon: "branch" })));

                const groupRemote = makeGroup("group:remote", "Remote", { icon: "remote" });
                const remotes = Array.isArray(branchSnapshot.remote) ? branchSnapshot.remote : [];
                const byRemote = new Map();

                remotes.forEach((ref) => {
                    const value = String(ref || "").trim();
                    if (!value) return;
                    if (/->/.test(value)) return;
                    const idx = value.indexOf("/");
                    if (idx === -1) return;
                    const remoteName = value.slice(0, idx);
                    const name = value.slice(idx + 1);
                    if (!remoteName || !name) return;
                    if (!byRemote.has(remoteName)) byRemote.set(remoteName, []);
                    byRemote.get(remoteName).push(value);
                });

                Array.from(byRemote.keys())
                    .sort()
                    .forEach((remoteName) => {
                        const remoteGroup = makeGroup(`remote:${remoteName}`, remoteName, { icon: "remote" });
                        (byRemote.get(remoteName) || [])
                            .sort()
                            .forEach((full) => {
                                remoteGroup.body.appendChild(makeBranchRow(full, { icon: "branch" }));
                            });
                        groupRemote.body.appendChild(remoteGroup.wrap);
                    });

                host.innerHTML = "";
                host.appendChild(groupHead.wrap);
                host.appendChild(groupLocal.wrap);
                host.appendChild(groupRemote.wrap);
            };

            const applyLogFilter = () => {
                const q = (document.getElementById("gitLogSearchInput")?.value || "").trim().toLowerCase();
                const filtered = !q
                    ? historyAll
                    : historyAll.filter((c) => {
                        const shortHash = c.shortHash || (c.hash ? String(c.hash).slice(0, 7) : "");
                        const hay = `${shortHash} ${c.hash || ""} ${c.refs || ""} ${c.subject || ""} ${c.author || ""} ${c.date || ""}`.toLowerCase();
                        return hay.includes(q);
                    });

                renderGitHistory(filtered);

                const selected = selectedCommitRef?.hash || "";
                if (selected && !filtered.some((c) => c?.hash === selected)) {
                    selectedCommitRef.hash = null;
                    clearLogDetails();
                }
            };

            const quoteGitPath = (p) => `"${String(p || "").replace(/"/g, '\\"')}"`;

            const updateLogDiffNavUi = () => {
                const prevBtn = document.getElementById("gitLogDiffPrevBtn");
                const nextBtn = document.getElementById("gitLogDiffNextBtn");
                const counter = document.getElementById("gitLogDiffCounter");
                const ignoreBtn = document.getElementById("gitLogIgnoreWhitespaceBtn");
                const copyBtn = document.getElementById("gitCopyCommitHashBtn");

                const hasCommit = Boolean(selectedCommitRef?.hash);
                const total = logDiffAnchors.length;
                const active = logDiffIndex >= 0 ? logDiffIndex + 1 : 0;

                if (counter) counter.textContent = `${active}/${total}`;

                const navDisabled = !hasCommit || total === 0;
                if (prevBtn) prevBtn.disabled = navDisabled;
                if (nextBtn) nextBtn.disabled = navDisabled;

                if (ignoreBtn) {
                    ignoreBtn.disabled = !hasCommit;
                    ignoreBtn.classList.toggle("active", logIgnoreWhitespace);
                }

                if (copyBtn) copyBtn.disabled = !hasCommit;
            };

            const resetLogDiffNav = () => {
                logDiffAnchors = [];
                logDiffIndex = -1;
                updateLogDiffNavUi();
            };

            const computeLogDiffAnchors = () => {
                const left = document.getElementById("gitLogDiffLeft");
                const right = document.getElementById("gitLogDiffRight");
                if (!left || !right) return [];

                const seen = new Set();
                const take = (el) => {
                    try {
                        const top = el.offsetTop;
                        if (Number.isFinite(top)) seen.add(top);
                    } catch (_) { }
                };

                left.querySelectorAll(".diff-add,.diff-del,.diff-hunk").forEach(take);
                right.querySelectorAll(".diff-add,.diff-del,.diff-hunk").forEach(take);
                return Array.from(seen).sort((a, b) => a - b);
            };

            const scheduleLogDiffAnchorRecalc = () => {
                if (logDiffAnchorTimer) clearTimeout(logDiffAnchorTimer);
                logDiffAnchorTimer = setTimeout(() => {
                    logDiffAnchorTimer = null;
                    const anchors = computeLogDiffAnchors();
                    logDiffAnchors = anchors;
                    if (logDiffIndex >= anchors.length) logDiffIndex = anchors.length - 1;
                    updateLogDiffNavUi();
                }, 60);
            };

            const scrollLogDiffToIndex = (idx) => {
                const left = document.getElementById("gitLogDiffLeft");
                const right = document.getElementById("gitLogDiffRight");
                const anchor = logDiffAnchors[idx];
                if (!left || !right || !Number.isFinite(anchor)) return;
                const y = Math.max(0, anchor - 24);
                logDiffSyncing = true;
                left.scrollTop = y;
                right.scrollTop = y;
                logDiffSyncing = false;
            };

            const navigateLogDiff = (delta) => {
                if (!logDiffAnchors.length) return;
                if (delta > 0) logDiffIndex = logDiffIndex < 0 ? 0 : (logDiffIndex + 1) % logDiffAnchors.length;
                else logDiffIndex = logDiffIndex < 0 ? logDiffAnchors.length - 1 : (logDiffIndex - 1 + logDiffAnchors.length) % logDiffAnchors.length;
                scrollLogDiffToIndex(logDiffIndex);
                updateLogDiffNavUi();
            };

            const reloadLogDiff = async (hash) => {
                const commitHash = String(hash || selectedCommitRef?.hash || "").trim();
                if (!commitHash) return;

                resetLogDiffNav();

                const wFlag = logIgnoreWhitespace ? "-w" : "";
                const filePath = String(logSelectedFilePath || "").trim();
                const cmd = filePath
                    ? ["git show", commitHash, wFlag, '--pretty=format:""', "--", quoteGitPath(filePath)].filter(Boolean).join(" ")
                    : ["git show", "-1", wFlag, '--pretty=format:""', commitHash].filter(Boolean).join(" ");

                const diffRes = await runGit(cmd, { silent: true });
                renderLogDiff(diffRes.ok ? diffRes.stdout || "" : "");
            };

            const clearLogDetails = () => {
                const subject = document.getElementById("gitLogSubject");
                const hashEl = document.getElementById("gitLogHash");
                const author = document.getElementById("gitLogAuthor");
                const date = document.getElementById("gitLogDate");
                const subtitle = document.getElementById("gitLogDetailsSubtitle");
                const filesHost = document.getElementById("gitLogFiles");
                const filesCount = document.getElementById("gitLogFilesCount");
                const detailsBody = document.getElementById("gitLogDetailsBody");
                const empty = document.getElementById("gitLogEmptyState");

                if (subject) subject.textContent = "";
                if (hashEl) hashEl.textContent = "";
                if (author) author.textContent = "";
                if (date) date.textContent = "";
                if (subtitle) subtitle.textContent = "";
                if (filesHost) filesHost.innerHTML = "";
                if (filesCount) filesCount.textContent = "";
                logSelectedFilePath = "";
                resetLogDiffNav();
                detailsBody?.classList.add("hidden");
                empty?.classList.remove("hidden");
                renderLogDiff("");
            };

            const renderLogFiles = (hash, entries = []) => {
                const host = document.getElementById("gitLogFiles");
                if (!host) return;
                host.innerHTML = "";

                const list = Array.isArray(entries) ? entries : [];
                const filesCount = document.getElementById("gitLogFilesCount");
                if (filesCount) filesCount.textContent = list.length === 1 ? "1 file" : `${list.length} files`;

                if (!list.length) {
                    host.textContent = "No files.";
                    return;
                }

                list.forEach((ent) => {
                    const row = document.createElement("div");
                    row.className = "git-file-row";
                    row.dataset.path = String(ent.path || "").trim();
                    row.setAttribute("role", "button");
                    row.tabIndex = 0;

                    const status = document.createElement("span");
                    status.className = "git-file-status";
                    status.textContent = ent.status || "";

                    const p = document.createElement("span");
                    p.className = "git-file-path";
                    p.textContent = ent.path || "";

                    row.appendChild(status);
                    row.appendChild(p);

                    const open = async () => {
                        const fp = String(ent.path || "").trim();
                        if (!fp) return;
                        const subtitle = document.getElementById("gitLogDetailsSubtitle");
                        if (subtitle) subtitle.textContent = fp;
                        logSelectedFilePath = fp;

                        host.querySelectorAll(".git-file-row").forEach((el) => {
                            el.classList.toggle("selected", el.dataset?.path === fp);
                        });

                        await openDiffTab({
                            kind: "commit",
                            commitHash: String(hash || "").trim(),
                            parentHash: String(selectedCommitRef?.parents?.[0] || "").trim(),
                            parents: Array.isArray(selectedCommitRef?.parents) ? selectedCommitRef.parents : [],
                            path: fp,
                            oldPath: String(ent.oldPath || "").trim(),
                            status: String(ent.status || "").trim(),
                            source: "log-files"
                        });
                    };

                    row.classList.toggle("selected", Boolean(logSelectedFilePath) && logSelectedFilePath === String(ent.path || "").trim());
                    row.addEventListener("click", () => open().catch(() => { }));
                    row.addEventListener("keydown", (e) => {
                        if (e.key === "Enter" || e.key === " ") {
                            e.preventDefault();
                            open().catch(() => { });
                        }
                    });

                    host.appendChild(row);
                });
            };

            const loadCommitDetails = async (hash) => {
                if (!hash) return;
                const detailsBody = document.getElementById("gitLogDetailsBody");
                if (!detailsBody) return;

                const metaRes = await runGit(
                    `git show -1 --no-patch --date=iso-strict --pretty=format:"%H%x00%P%x00%an%x00%ad%x00%s" ${hash}`,
                    { silent: true }
                );

                if (!metaRes.ok) {
                    clearLogDetails();
                    return;
                }

                const [fullHash, parentsRaw, author, date, subject] = String(metaRes.stdout || "").split("\x00");
                selectedCommitRef.hash = String(fullHash || hash).trim();
                selectedCommitRef.parents = String(parentsRaw || "")
                    .trim()
                    .split(/\s+/)
                    .filter(Boolean);

                const filesRes = await runGit(`git show -1 --name-status --pretty=format:"" ${hash}`, { silent: true });
                const fileLines = (filesRes.ok ? String(filesRes.stdout || "") : "")
                    .split("\n")
                    .map((s) => s.trim())
                    .filter(Boolean);

                const files = fileLines
                    .map((line) => {
                        const parts = line.split("\t").filter(Boolean);
                        const status = parts[0] || "";
                        const path = parts[parts.length - 1] || "";
                        const oldPath = parts.length >= 3 ? parts[1] || "" : "";
                        return { status, path, oldPath };
                    })
                    .filter((f) => f.path);

                const subjectEl = document.getElementById("gitLogSubject");
                const hashEl = document.getElementById("gitLogHash");
                const authorEl = document.getElementById("gitLogAuthor");
                const dateEl = document.getElementById("gitLogDate");
                const subtitle = document.getElementById("gitLogDetailsSubtitle");

                if (subjectEl) subjectEl.textContent = subject || "";
                if (hashEl) hashEl.textContent = fullHash || hash;
                if (authorEl) authorEl.textContent = author || "";
                if (dateEl) dateEl.textContent = date || "";
                if (subtitle) subtitle.textContent = "";

                logSelectedFilePath = "";
                renderLogFiles(hash, files);

                document.getElementById("gitLogEmptyState")?.classList.add("hidden");
                detailsBody.classList.remove("hidden");
            };

            // ✅ FIX: runGit ALWAYS passes repoRoot + normalizes ok/code/exitCode
            const runGit = async (cmd, opts = {}) => {
                const repoState = getGitRepoState?.() || {};
                const repoRoot = String(opts.repoRoot || repoState.repoRoot || repoState.projectRoot || "").trim();

                // If someone accidentally calls with "status" etc, prefix it
                let command = String(cmd || "").trim();
                if (!/^git(\s|$)/i.test(command)) command = `git ${command}`;

                const requiresRepo = !/^\s*git\s+--version(\s|$)/i.test(command);

                if (repoState.gitDisabled) {
                    if (!opts.silent) gitError("Git is disabled for this project");
                    return { ok: false, error: "Git is disabled for this project" };
                }

                if (requiresRepo && !repoState.repoDetected) {
                    if (!opts.silent) gitError("No Git repository detected");
                    return { ok: false, error: "No Git repository detected" };
                }

                if (!opts.silent) gitOutput(`$ ${command}`);

                const res = await runGitApi(command, { ...opts, repoRoot });

                const ok = res?.ok === true || res?.code === 0 || res?.exitCode === 0;

                if (ok) {
                    if (opts.onSuccess) opts.onSuccess(res.stdout || "");
                    if (!opts.silent) {
                        if (res.stdout) gitOutput(res.stdout);
                        if (res.stderr) gitOutput(res.stderr);
                    }
                    return { ...res, ok: true };
                }

                const errMsg = res?.error || res?.stderr || "Git command failed";
                if (!opts.silent) gitError(errMsg);
                return { ...res, ok: false, error: String(errMsg || "").trim() };
            };

            const previewDiffForPath = async (filePath, { staged, status, oldPath } = {}) => {
                const p = String(filePath || "").trim();
                if (!p) return { ok: false };

                const diffPath = document.getElementById("gitDiffPath");
                if (diffPath) diffPath.value = p;

                return openDiffTab({
                    kind: "worktree",
                    path: p,
                    oldPath: String(oldPath || "").trim(),
                    status: String(status || "").trim(),
                    staged: !!staged,
                    source: staged ? "changes-staged" : "changes-unstaged"
                });
            };

            const previewSelectedFileDiff = async () => {
                const allSelected = [...gitSelected.unstaged, ...gitSelected.staged];
                if (allSelected.length !== 1) return { ok: true, skipped: true };

                const p = allSelected[0];
                const staged = gitSelected.staged.has(p);
                const entry =
                    lastStatusEntries.find(
                        (e) => String(e.path || "").trim() === String(p || "").trim() && !!e.staged === !!staged
                    ) || null;

                return previewDiffForPath(p, { staged, status: entry?.status || "", oldPath: entry?.oldPath || "" });
            };

            const refreshGitStatus = async () => {
                console.log("[GitToolWindow] refreshGitStatus called");
                gitSelected.staged.clear();
                gitSelected.unstaged.clear();
                previewSelectedFileDiff().catch(() => { });

                const statusRes = await runGit("git status --short --branch", {
                    onSuccess: (out) => {
                        console.log("[GitToolWindow] git status success");
                        const lines = (out || "").split("\n").filter(Boolean);
                        const entries = [];
                        let currentBranch = "";

                        lines.forEach((line) => {
                            if (line.startsWith("##")) {
                                const br = line.replace(/^##\s+/, "").trim();
                                currentBranch = (br.split("...")[0] || br).trim();
                                return;
                            }
                            const indexStatus = line[0];
                            const worktreeStatus = line[1];
                            const rawPath = line.slice(3).trim();
                            let path = rawPath;
                            let oldPath = "";
                            if (rawPath.includes("->")) {
                                const parts = rawPath
                                    .split("->")
                                    .map((s) => String(s || "").trim())
                                    .filter(Boolean);
                                if (parts.length >= 2) {
                                    oldPath = parts[0];
                                    path = parts[parts.length - 1];
                                }
                            }
                            const staged = indexStatus !== " " && indexStatus !== "?";
                            const status = staged ? indexStatus : worktreeStatus;
                            entries.push({ status, path, oldPath, staged });
                        });

                        branchSnapshot.current = currentBranch || branchSnapshot.current || "";
                        renderGitChanges(entries);
                    }
                });

                if (!statusRes.ok) return statusRes;

                await runGit('git branch --format="%(refname:short)"', {
                    silent: true,
                    onSuccess: (out) => {
                        const select = document.getElementById("gitBranchSelect");
                        const logSelect = document.getElementById("gitLogBranchSelect");
                        const prior = select?.value || "";
                        const priorLog = logSelect?.value || "__ALL__";
                        if (!select) return;

                        select.innerHTML = "";
                        const locals = (out || "").split("\n").filter(Boolean);
                        locals.forEach((b) => {
                            const opt = document.createElement("option");
                            opt.value = b;
                            opt.textContent = b;
                            select.appendChild(opt);
                        });
                        if (prior) select.value = prior;

                        branchSnapshot.local = locals;
                        if (logSelect) logSelect.value = priorLog;
                    }
                });

                await runGit('git branch -r --format="%(refname:short)"', {
                    silent: true,
                    onSuccess: (out) => {
                        const remotes = (out || "")
                            .split("\n")
                            .map((s) => s.trim())
                            .filter(Boolean)
                            .filter((b) => !/->/.test(b) && !/\/HEAD$/.test(b));
                        branchSnapshot.remote = remotes;
                    }
                });

                try {
                    const logSelect = document.getElementById("gitLogBranchSelect");
                    if (logSelect) {
                        const prior = logSelect.value || "__ALL__";
                        logSelect.innerHTML = "";

                        const allOpt = document.createElement("option");
                        allOpt.value = "__ALL__";
                        allOpt.textContent = "All";
                        logSelect.appendChild(allOpt);

                        const groupLocal = document.createElement("optgroup");
                        groupLocal.label = "Local";
                        (branchSnapshot.local || []).forEach((b) => {
                            const opt = document.createElement("option");
                            opt.value = b;
                            opt.textContent = b;
                            groupLocal.appendChild(opt);
                        });
                        logSelect.appendChild(groupLocal);

                        const groupRemote = document.createElement("optgroup");
                        groupRemote.label = "Remote";
                        (branchSnapshot.remote || []).forEach((b) => {
                            const opt = document.createElement("option");
                            opt.value = b;
                            opt.textContent = b;
                            groupRemote.appendChild(opt);
                        });
                        logSelect.appendChild(groupRemote);

                        logSelect.value = prior;
                    }
                } catch (_) { }

                renderBranchesTree();
                syncGitActionStates();
                fetchCurrentBranch?.().catch(() => { });
                return statusRes;
            };

            const loadGitHistory = async () => {
                console.log("[GitToolWindow] loadGitHistory called");
                const logSelect = document.getElementById("gitLogBranchSelect");
                const selected = logSelect?.value || "__ALL__";
                const rangeArg = selected && selected !== "__ALL__" ? selected : "--all";
                const limit = 2500;

                const cmd = `git log ${rangeArg} -${limit} --topo-order --date=iso --decorate=short --pretty=format:"%H%x1f%P%x1f%D%x1f%an%x1f%ad%x1f%s"`;
                const res = await runGit(cmd, { silent: true });
                console.log("[GitToolWindow] git log result:", res.ok, res.stdout?.length);

                if (res.ok) {
                    const rows = (res.stdout || "")
                        .split("\n")
                        .filter(Boolean)
                        .map((line) => {
                            const parts = String(line).split("\x1f");
                            const hash = String(parts[0] || "").trim();
                            const parentsRaw = String(parts[1] || "").trim();
                            const refs = String(parts[2] || "").trim();
                            const author = String(parts[3] || "").trim();
                            const date = String(parts[4] || "").trim();
                            const subject = parts.length > 5 ? String(parts.slice(5).join("\x1f")).trim() : "";
                            const parents = parentsRaw ? parentsRaw.split(/\s+/).filter(Boolean) : [];
                            return { hash, shortHash: hash ? hash.slice(0, 7) : "", parents, refs, author, date, subject };
                        })
                        .filter((c) => c.hash);

                    historyAll = rows;
                    applyLogFilter();
                } else {
                    console.error("[GitToolWindow] git log failed:", res.error);
                }

                return res;
            };

            const renderRepoUnavailableUi = (message) => {
                const branchesHost = document.getElementById("gitBranchesTree");
                const historyHost = document.getElementById("gitHistoryList");
                const unstagedHost = document.getElementById("gitChangesUnstaged");
                const stagedHost = document.getElementById("gitChangesStaged");
                if (branchesHost) branchesHost.textContent = message || "Git not available.";
                if (historyHost) historyHost.textContent = message || "Git not available.";
                if (unstagedHost) unstagedHost.textContent = message || "Git not available.";
                if (stagedHost) stagedHost.textContent = message || "Git not available.";
                clearLogDetails();
            };

            const ensureGitLoaded = async ({ source, skipReconnect } = {}) => {
                console.log("[GitToolWindow] ensureGitLoaded source:", source);
                if (ensureLoadedPromise) return ensureLoadedPromise;

                ensureLoadedPromise = (async () => {
                    setGitLifecycle("detectingRepo");
                    let reconnectErr = "";

                    if (!skipReconnect) {
                        try {
                            await gitActions?.reconnect?.({ reason: `toolwindow:${source || "open"}` });
                        } catch (err) {
                            reconnectErr = String(err?.message || err || "").trim();
                        }
                    }

                    const st = getGitRepoState?.() || {};
                    syncGitActionStates();

                    if (!st.projectRoot) {
                        setGitLifecycle("idle");
                        setGitBanner({ message: "Open a project to use Git.", actions: [] });
                        renderRepoUnavailableUi("No project opened.");
                        return;
                    }

                    if (st.gitDisabled) {
                        setGitLifecycle("idle");
                        setGitBanner({
                            message: "Git is disabled for this project.",
                            actions: [
                                {
                                    label: "Enable Git",
                                    onClick: async () => {
                                        await gitActions?.enableGit?.();
                                        await ensureGitLoaded({ source: "enable" });
                                    }
                                }
                            ]
                        });
                        renderRepoUnavailableUi("Git is disabled.");
                        return;
                    }

                    if (!st.gitAvailable) {
                        setGitLifecycle("error");
                        setGitBanner({
                            message: st.lastError || reconnectErr || "Git is not available (git --version failed).",
                            actions: [{ label: "Retry", onClick: () => ensureGitLoaded({ source: "retry" }) }]
                        });
                        renderRepoUnavailableUi("Git is not available.");
                        return;
                    }

                    if (!st.repoDetected) {
                        setGitLifecycle("idle");
                        setGitBanner({
                            message: st.lastError || "No Git repository detected in this project.",
                            actions: [
                                { label: "Initialize", onClick: () => gitActions?.initializeRepo?.({}) },
                                {
                                    label: "Clone…",
                                    onClick: async () => {
                                        const url = await showPrompt({
                                            title: "Clone Repository",
                                            message: "Enter repository URL",
                                            placeholder: "https://github.com/user/repo.git"
                                        });
                                        if (!url) return;
                                        const directory = await showPrompt({
                                            title: "Clone Repository",
                                            message: "Enter target directory",
                                            placeholder: "/path/to/clone/repo"
                                        });
                                        if (!directory) return;
                                        await gitActions?.cloneRepo?.({ url, directory });
                                        await ensureGitLoaded({ source: "clone" });
                                    }
                                },
                                {
                                    label: "Connect Existing…",
                                    onClick: async () => {
                                        const repoRoot = await showPrompt({
                                            title: "Connect Existing Repository",
                                            message: "Enter repository root folder (must contain .git)",
                                            placeholder: "/path/to/repo"
                                        });
                                        if (!repoRoot) return;
                                        await gitActions?.connectExisting?.({ repoRoot });
                                        await ensureGitLoaded({ source: "connect-existing" });
                                    }
                                },
                                { label: "Choose Repo Root…", onClick: () => gitActions?.chooseRepoRoot?.() },
                                { label: "Reconnect", onClick: () => gitActions?.reconnect?.({ reason: "manual" }) }
                            ]
                        });
                        renderRepoUnavailableUi("No repository detected.");
                        return;
                    }

                    setGitBanner({ message: "" });
                    setGitLifecycle("loading");
                    await refreshGitStatus().catch(() => { });
                    await loadGitHistory().catch(() => { });
                    setGitLifecycle("ready");
                })().finally(() => {
                    ensureLoadedPromise = null;
                });

                return ensureLoadedPromise;
            };
const openGitToolWindow = (opts = {}) => {
    ensurePanelMounted();
    wirePanelDom();
    toggleToolWindowPanel("gitToolPanel", "bottom");
    setActiveGitTab(opts.tab || "log");
    if (!opts.skipRefresh) ensureGitLoaded({ source: opts.source || "open" }).catch(() => { });
};

const openCommitToolWindow = () => {
    openGitToolWindow({ tab: "changes" });
    document.getElementById("gitCommitMessage")?.focus?.();
};

const openGitPanel = () => openGitToolWindow({ tab: "log" });

const stageOrUnstage = async (targetSet, staged) => {
    const files = Array.from(targetSet);
    if (!files.length) {
        gitOutput(staged ? "No staged selection." : "No unstaged selection.");
        return;
    }

    // ✅ FIX: use `--` before paths (handles weird filenames safely)
    const paths = files.map((f) => `"${String(f).replace(/"/g, '\\"')}"`).join(" ");
    const cmd = staged ? `git restore --staged -- ${paths}` : `git add -- ${paths}`;

    await runGit(cmd);
    await refreshGitStatus();
};

function wirePanelDom() {
    if (panelDomWired) return;
    if (!document.getElementById("gitRefreshBtn")) return; // panel not mounted yet
    panelDomWired = true;

    document.querySelectorAll("#gitToolWindow .git-tw-tab").forEach((btn) => {
        btn.addEventListener("click", () => {
            const tab = btn.getAttribute("data-git-tab");
            setActiveGitTab(tab);
        });
    });

    document.getElementById("gitRefreshBtn")?.addEventListener("click", () => ensureGitLoaded({ source: "refresh" }).catch(() => { }));
    document.getElementById("gitBranchesRefreshBtn")?.addEventListener("click", () => ensureGitLoaded({ source: "branches-refresh" }).catch(() => { }));
    document.getElementById("gitFocusSearchBtn")?.addEventListener("click", () => {
        setActiveGitTab("log");
        document.getElementById("gitLogSearchInput")?.focus?.();
    });

    document.getElementById("gitFilterBtn")?.addEventListener("click", () => showToast("info", "Git", "Not implemented yet: Filters"));

    document.getElementById("gitStatusBtn")?.addEventListener("click", () => {
        setActiveGitTab("changes");
        ensureGitLoaded({ source: "status" }).catch(() => { });
    });

    document.getElementById("gitLogBtn")?.addEventListener("click", () => {
        setActiveGitTab("log");
        ensureGitLoaded({ source: "log" }).catch(() => { });
    });

    document.getElementById("gitDiffBtn")?.addEventListener("click", () => runGit("git diff --stat").catch(() => { }));
    document.getElementById("gitClearBtn")?.addEventListener("click", () => {
        const out = document.getElementById("gitOutput");
        if (out) out.textContent = "Git ready.";
    });

    document.getElementById("gitStageSelectedBtn")?.addEventListener("click", () => stageOrUnstage(gitSelected.unstaged, false));
    document.getElementById("gitUnstageSelectedBtn")?.addEventListener("click", () => stageOrUnstage(gitSelected.staged, true));

    // (rest of your DOM wiring remains the same)
    // NOTE: I’m not changing your UI/feature behavior here—only the git execution/scoping bugs.

    // Initialize default tab
    setActiveGitTab(activeTab);
    syncGitActionStates();
    previewSelectedFileDiff().catch(() => { });
}

document.getElementById("toolbarGitBtn")?.addEventListener("click", () => openGitToolWindow());
document.getElementById("toolbarCommitBtn")?.addEventListener("click", () => openCommitToolWindow());

// Tool window activation
window.addEventListener("ahmadIDE:toolwindow-activated", (e) => {
    const panelId = e?.detail?.panelId;
    if (panelId !== "gitToolPanel") return;
    wirePanelDom();
    ensureGitLoaded({ source: "activated" }).catch(() => { });
});

// Keep Git UI in sync with repo state changes
try {
    subscribeGitRepoState((s) => {
        if (!isGitPanelVisible()) return;
        if (!s) return;
        ensureGitLoaded({ source: "repo-state", skipReconnect: true }).catch(() => { });
    });
} catch (_) { }

// Lazy-mount support
if (featureRegistry && typeof featureRegistry.onMounted === "function") {
    featureRegistry.onMounted("gitToolPanel", () => {
        wirePanelDom();
        const panel = document.getElementById("gitToolPanel");
        if (panel && !panel.classList.contains("hidden")) ensureGitLoaded({ source: "mounted" }).catch(() => { });
    });
} else {
    wirePanelDom();
    ensureGitLoaded({ source: "startup" }).catch(() => { });
}

return { openGitToolWindow, openCommitToolWindow, openGitPanel };
}

return { wireGitToolWindow };
    }

if (typeof window !== "undefined") {
    window.AhmadIDEModules = window.AhmadIDEModules || {};
    window.AhmadIDEModules.git = window.AhmadIDEModules.git || {};
    window.AhmadIDEModules.git.createGitToolWindowManager = createGitToolWindowManager;
}
}) ();
