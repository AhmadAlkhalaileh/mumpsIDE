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

        const GLOBAL_REPO_ROOT_KEY = "ahmadIDE:gitRepoRootGlobal";
        const VISTA_REPO_ROOT_KEY = "ahmadIDE:vistaRoutinesRepoPath";
        const readGlobalRepoRoot = () => {
            try {
                const v = String(localStorage.getItem(GLOBAL_REPO_ROOT_KEY) || "").trim();
                if (v) return v;
            } catch (_) { }
            try {
                const v = String(localStorage.getItem(VISTA_REPO_ROOT_KEY) || "").trim();
                if (v) return v;
            } catch (_) { }
            return "";
        };

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

            const ensureCommitPanelMounted = () => {
                try {
                    featureRegistry?.ensureById?.("commitPanel");
                } catch (_) { }
            };

            let panelDomWired = false;
            let commitPanelDomWired = false;

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
            console.log('[GitToolWindow] perfUi:', !!perfUi, 'createHistoryRenderer:', !!perfUi?.createHistoryRenderer);
            const diffRenderer = perfUi?.createDiffRenderer ? perfUi.createDiffRenderer({ maxLines: 6000 }) : null;
            const logDiffRenderer = perfUi?.createDiffRenderer
                ? perfUi.createDiffRenderer({ maxLines: 6000, leftId: "gitLogDiffLeft", rightId: "gitLogDiffRight" })
                : null;

            const gitSelected = { staged: new Set(), unstaged: new Set(), untracked: new Set() };

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
            console.log('[GitToolWindow] historyRenderer:', !!historyRenderer, 'render method:', !!historyRenderer?.render);

            let activeTab = "log";
            let lastStatusEntries = [];
            let historyAll = [];
            let lastHistoryItems = [];
            let branchSnapshot = { current: "", local: [], remote: [] };

            let logAuthorFilter = "";
            let logSinceFilter = "";
            let logUntilFilter = "";
            let logPathFilter = "";

            let commitPanelSelected = { path: "", staged: false, status: "", oldPath: "" };

            let logIgnoreWhitespace = false;
            let logSelectedFilePath = "";
            let logDiffAnchors = [];
            let logDiffIndex = -1;
            let logDiffAnchorTimer = null;
            let logDiffObserver = null;
            let logDiffSyncing = false;

            const ensureLogDiffLayout = () => {
                const host = document.getElementById("gitLogDiffGrid");
                if (!host) return;
                // Already mounted
                if (host.querySelector("#gitLogDiffLeft") && host.querySelector("#gitLogDiffRight")) return;

                host.innerHTML = `
                    <div class="git-diff-grid" role="region" aria-label="Commit diff">
                        <div class="git-diff-pane" id="gitLogDiffLeft" aria-label="Original"></div>
                        <div class="git-diff-pane" id="gitLogDiffRight" aria-label="Modified"></div>
                    </div>
                `;

                // Keep panes scrolled together (IntelliJ-style)
                const left = document.getElementById("gitLogDiffLeft");
                const right = document.getElementById("gitLogDiffRight");
                if (!left || !right) return;
                const sync = (src, dst) => () => {
                    if (logDiffSyncing) return;
                    logDiffSyncing = true;
                    try {
                        dst.scrollTop = src.scrollTop;
                        dst.scrollLeft = src.scrollLeft;
                    } finally {
                        logDiffSyncing = false;
                    }
                };
                left.addEventListener("scroll", sync(left, right), { passive: true });
                right.addEventListener("scroll", sync(right, left), { passive: true });
            };

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
                ensureLogDiffLayout();
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
                    syncChangesUI();
                    return;
                }

                const unstaged = lastStatusEntries.filter(e => !e.staged);
                const staged = lastStatusEntries.filter(e => e.staged);

                renderFileList("gitChangesUnstaged", unstaged, false);
                renderFileList("gitChangesStaged", staged, true);
                syncChangesUI();
            };

            const renderFileList = (hostId, files, isStaged) => {
                const host = document.getElementById(hostId);
                if (!host) return;
                host.innerHTML = "";

                if (!files.length) {
                    const empty = document.createElement("div");
                    empty.className = "git-changes-empty";
                    empty.textContent = isStaged ? "No staged files" : "No changes";
                    host.appendChild(empty);
                    return;
                }

                files.forEach((file) => {
                    const item = document.createElement("div");
                    item.className = "git-file-item";
                    item.tabIndex = 0;
                    item.dataset.path = file.path;
                    item.dataset.staged = isStaged;

                    const checkbox = document.createElement("input");
                    checkbox.type = "checkbox";
                    checkbox.className = "git-file-checkbox";
                    checkbox.checked = (isStaged ? gitSelected.staged : gitSelected.unstaged).has(file.path);

                    const status = document.createElement("span");
                    status.className = `git-file-status ${file.status}`;
                    status.textContent = file.status;

                    const name = document.createElement("span");
                    name.className = "git-file-name";
                    name.textContent = file.path.split("/").pop();

                    const pathParts = file.path.split("/");
                    if (pathParts.length > 1) {
                        const pathEl = document.createElement("span");
                        pathEl.className = "git-file-path";
                        pathEl.textContent = pathParts.slice(0, -1).join("/");
                        item.appendChild(checkbox);
                        item.appendChild(status);
                        item.appendChild(name);
                        item.appendChild(pathEl);
                    } else {
                        item.appendChild(checkbox);
                        item.appendChild(status);
                        item.appendChild(name);
                    }

                    checkbox.addEventListener("change", () => {
                        const targetSet = isStaged ? gitSelected.staged : gitSelected.unstaged;
                        if (checkbox.checked) targetSet.add(file.path);
                        else targetSet.delete(file.path);
                        syncGitActionStates();
                    });

                    item.addEventListener("click", (e) => {
                        if (e.target === checkbox) return;
                        checkbox.checked = !checkbox.checked;
                        checkbox.dispatchEvent(new Event("change"));
                    });

                    item.addEventListener("dblclick", () => {
                        previewDiffForPath(file.path, { staged: isStaged, status: file.status, oldPath: file.oldPath }).catch(() => { });
                    });

                    host.appendChild(item);
                });
            };

            const syncChangesUI = () => {
                const unstaged = lastStatusEntries.filter(e => !e.staged);
                const staged = lastStatusEntries.filter(e => e.staged);

                const changesCount = document.getElementById("gitChangesCount");
                const stagedCount = document.getElementById("gitStagedCount");
                const stagedSection = document.getElementById("gitStagedSection");

                if (changesCount) changesCount.textContent = unstaged.length === 1 ? "1 file" : `${unstaged.length} files`;
                if (stagedCount) stagedCount.textContent = staged.length === 1 ? "1 file" : `${staged.length} files`;
                if (stagedSection) stagedSection.classList.toggle("hidden", staged.length === 0);
            };

            const renderGitHistory = (items = []) => {
                console.log('[GitToolWindow] renderGitHistory called, items:', items.length, 'hasHistoryRenderer:', !!historyRenderer);
                lastHistoryItems = Array.isArray(items) ? items : [];
                if (historyRenderer && typeof historyRenderer.render === "function") {
                    console.log('[GitToolWindow] Calling historyRenderer.render with', lastHistoryItems.length, 'items');
                    historyRenderer.render(lastHistoryItems);
                    return;
                }
                console.warn('[GitToolWindow] No historyRenderer available - falling back to basic render');
                const host = document.getElementById("gitHistoryList");
                if (!host) return;
                host.innerHTML = "";

                if (!items.length) {
                    const empty = document.createElement("div");
                    empty.className = "git-log-empty";
                    empty.textContent = "No commits found";
                    host.appendChild(empty);
                    return;
                }

                const ROW_HEIGHT = 32;
                const LANE_WIDTH = 16;
                const NODE_RADIUS = 5;

                items.forEach((commit, idx) => {
                    const row = document.createElement("div");
                    row.className = "git-log-row";
                    row.dataset.hash = commit.hash;
                    row.tabIndex = 0;

                    const graphCell = document.createElement("div");
                    graphCell.className = "git-log-graph";

                    const maxLane = Math.max(commit.lane || 0, ...(commit.edges || []).map(e => Math.max(e.from, e.to)));
                    const graphWidth = (maxLane + 1) * LANE_WIDTH + 16;

                    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
                    svg.setAttribute("width", graphWidth);
                    svg.setAttribute("height", ROW_HEIGHT);
                    svg.setAttribute("class", "git-graph-svg");

                    const centerX = (commit.lane || 0) * LANE_WIDTH + LANE_WIDTH / 2;
                    const centerY = ROW_HEIGHT / 2;

                    (commit.edges || []).forEach(edge => {
                        const fromX = edge.from * LANE_WIDTH + LANE_WIDTH / 2;
                        const toX = edge.to * LANE_WIDTH + LANE_WIDTH / 2;
                        const path = document.createElementNS("http://www.w3.org/2000/svg", "path");

                        if (edge.type === "direct") {
                            path.setAttribute("d", `M ${fromX} ${centerY} L ${toX} ${ROW_HEIGHT}`);
                            path.setAttribute("class", "git-edge git-edge-direct");
                        } else if (edge.type === "merge") {
                            const ctrlY = centerY + 12;
                            path.setAttribute("d", `M ${fromX} ${centerY} Q ${fromX} ${ctrlY}, ${toX} ${ROW_HEIGHT}`);
                            path.setAttribute("class", "git-edge git-edge-merge");
                        } else {
                            path.setAttribute("d", `M ${fromX} 0 L ${fromX} ${ROW_HEIGHT}`);
                            path.setAttribute("class", "git-edge git-edge-passthrough");
                        }

                        path.setAttribute("stroke", commit.color || "#d19a66");
                        svg.appendChild(path);
                    });

                    const isMerge = (commit.parents || []).length > 1;
                    if (isMerge) {
                        const diamond = document.createElementNS("http://www.w3.org/2000/svg", "polygon");
                        const size = NODE_RADIUS + 1;
                        diamond.setAttribute("points", `${centerX},${centerY - size} ${centerX + size},${centerY} ${centerX},${centerY + size} ${centerX - size},${centerY}`);
                        diamond.setAttribute("fill", commit.color || "#d19a66");
                        diamond.setAttribute("class", "git-node git-node-diamond");
                        svg.appendChild(diamond);
                    } else {
                        const circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
                        circle.setAttribute("cx", centerX);
                        circle.setAttribute("cy", centerY);
                        circle.setAttribute("r", NODE_RADIUS);
                        circle.setAttribute("fill", commit.color || "#d19a66");
                        circle.setAttribute("class", "git-node git-node-circle");
                        svg.appendChild(circle);
                    }

                    graphCell.appendChild(svg);

                    const content = document.createElement("div");
                    content.className = "git-log-content";

                    const message = document.createElement("div");
                    message.className = "git-log-message";
                    message.textContent = commit.subject || "";

                    if (commit.refs) {
                        const refs = commit.refs.split(",").map(r => r.trim()).filter(Boolean);
                        refs.forEach(ref => {
                            const badge = document.createElement("span");
                            badge.className = "git-log-ref";
                            badge.textContent = ref;
                            message.appendChild(badge);
                        });
                    }

                    const meta = document.createElement("div");
                    meta.className = "git-log-meta";
                    meta.innerHTML = `<span class="git-log-author">${commit.author || ""}</span> · <span class="git-log-date">${commit.date || ""}</span>`;

                    content.appendChild(message);
                    content.appendChild(meta);

                    row.appendChild(graphCell);
                    row.appendChild(content);

                    row.addEventListener("click", () => {
                        host.querySelectorAll(".git-log-row").forEach(r => r.classList.remove("selected"));
                        row.classList.add("selected");
                        if (selectedCommitRef) {
                            selectedCommitRef.hash = commit.hash;
                            selectedCommitRef.parents = commit.parents || [];
                        }
                        loadCommitDetails(commit.hash).catch(() => { });
                    });

                    host.appendChild(row);
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

            const isCommitPanelVisible = () => {
                const panel = document.getElementById("commitPanel");
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
                const hasStagedFiles = lastStatusEntries.some(e => e.staged);
                if (commitBtn) commitBtn.disabled = !repoReady || !hasMessage || !hasStagedFiles;
                if (commitAndPushBtn) commitAndPushBtn.disabled = !repoReady || !hasMessage || !hasStagedFiles;

                ["gitFetchBtn", "gitPullBtn", "gitPushBtn", "gitCheckoutBtn"].forEach((id) => {
                    const el = document.getElementById(id);
                    if (el) el.disabled = !repoReady;
                });
            };

            const performCommit = async (andPush = false) => {
                const msgEl = document.getElementById("gitCommitMessage");
                const amendCb = document.getElementById("gitAmendCheckbox");
                const message = String(msgEl?.value || "").trim();

                if (!message) {
                    showToast("error", "Git", "Commit message is required");
                    return;
                }

                const isAmend = amendCb?.checked || false;
                const flags = [];
                if (isAmend) flags.push("--amend");

                const quotedMsg = message.replace(/'/g, "'\\''");
                const cmd = `git commit ${flags.join(" ")} -m '${quotedMsg}'`.trim();

                const res = await runGit(cmd);
                if (!res.ok) return;

                showToast("success", "Git", isAmend ? "Commit amended" : "Committed successfully");
                if (msgEl) msgEl.value = "";
                if (amendCb) amendCb.checked = false;

                await refreshGitStatus();
                await loadGitHistory();

                if (andPush) {
                    const pushRes = await runGit("git push");
                    if (pushRes.ok) showToast("success", "Git", "Pushed to remote");
                }
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
                const q = (
                    document.getElementById("gitSearchInput")?.value ||
                    document.getElementById("gitLogSearchInput")?.value ||
                    ""
                )
                    .trim()
                    .toLowerCase();
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

            const quoteGitArg = (value) => `"${String(value || "").replace(/"/g, '\\"')}"`;
            const quoteGitPath = (p) => quoteGitArg(p);

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
                const diffText = diffRes.ok ? diffRes.stdout || "" : "";

                updateDiffHeader(commitHash, filePath, diffText);
                renderLogDiff(diffText);
            };

            const updateDiffHeader = (commitHash, filePath, diffText) => {
                const header = document.getElementById("gitDiffHeader");
                const leftHash = document.getElementById("gitDiffLeftHash");
                const rightHash = document.getElementById("gitDiffRightHash");
                const filePathEl = document.getElementById("gitDiffFilePath");
                const countEl = document.getElementById("gitDiffCount");

                if (!header) return;

                const parent = selectedCommitRef?.parents?.[0] || "";
                const shortParent = parent ? parent.slice(0, 8) : "";
                const shortCommit = commitHash ? commitHash.slice(0, 8) : "";

                if (leftHash) leftHash.textContent = shortParent || "—";
                if (rightHash) rightHash.textContent = shortCommit;
                if (filePathEl) filePathEl.textContent = filePath || "";

                const addCount = (diffText.match(/^\+[^+]/gm) || []).length;
                const delCount = (diffText.match(/^-[^-]/gm) || []).length;
                const total = addCount + delCount;
                if (countEl) countEl.textContent = total === 1 ? "1 difference" : `${total} differences`;

                header.classList.toggle("hidden", !filePath && !diffText.trim());
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

                        await reloadLogDiff(String(hash || "").trim());
                    };

                    row.classList.toggle("selected", Boolean(logSelectedFilePath) && logSelectedFilePath === String(ent.path || "").trim());
                    row.addEventListener("click", () => open().catch(() => { }));
                    row.addEventListener("dblclick", async () => {
                        const fp = String(ent.path || "").trim();
                        if (!fp) return;
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
                    });
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

                ensureLogDiffLayout();
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

                // Default: show the full commit diff until a file is selected.
                reloadLogDiff(hash).catch(() => { });
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
                        const locals = (out || "").split("\n").filter(Boolean);
                        branchSnapshot.local = locals;

                        // Optional legacy select (if present in some layouts)
                        const select = document.getElementById("gitBranchSelect");
                        if (select) {
                            const prior = select.value || "";
                            select.innerHTML = "";
                            locals.forEach((b) => {
                                const opt = document.createElement("option");
                                opt.value = b;
                                opt.textContent = b;
                                select.appendChild(opt);
                            });
                            if (prior) select.value = prior;
                        }
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
                syncCommitPanelChanges();
                syncCommitPanelActions();
                syncGitActionStates();
                fetchCurrentBranch?.().catch(() => { });
                return statusRes;
            };

            const calculateGraphLayout = (commits) => {
                if (!commits.length) return commits;

                const hashToIndex = new Map();
                commits.forEach((c, i) => hashToIndex.set(c.hash, i));

                const lanes = [];
                const colors = ["#d19a66", "#56b6c2", "#98c379", "#c678dd", "#e5c07b", "#61afef"];

                commits.forEach((commit, idx) => {
                    let lane = -1;
                    const myHash = commit.hash;
                    const myParents = commit.parents || [];

                    for (let i = 0; i < lanes.length; i++) {
                        if (lanes[i] === myHash) {
                            lane = i;
                            break;
                        }
                    }

                    if (lane === -1) {
                        lane = lanes.findIndex(l => !l);
                        if (lane === -1) lane = lanes.length;
                    }

                    commit.lane = lane;
                    commit.color = colors[lane % colors.length];
                    commit.edges = [];

                    if (myParents.length === 0) {
                        lanes[lane] = null;
                    } else if (myParents.length === 1) {
                        lanes[lane] = myParents[0];
                        commit.edges.push({ from: lane, to: lane, toHash: myParents[0], type: "direct" });
                    } else {
                        lanes[lane] = myParents[0];
                        commit.edges.push({ from: lane, to: lane, toHash: myParents[0], type: "direct" });
                        for (let i = 1; i < myParents.length; i++) {
                            let targetLane = lanes.findIndex(l => !l);
                            if (targetLane === -1) targetLane = lanes.length;
                            lanes[targetLane] = myParents[i];
                            commit.edges.push({ from: lane, to: targetLane, toHash: myParents[i], type: "merge" });
                        }
                    }

                    for (let i = 0; i < lanes.length; i++) {
                        if (i !== lane && lanes[i]) {
                            const nextIdx = hashToIndex.get(lanes[i]);
                            if (nextIdx !== undefined && nextIdx > idx) {
                                const nextCommit = commits[nextIdx];
                                if (!commit.edges.some(e => e.from === i)) {
                                    commit.edges.push({ from: i, to: i, toHash: lanes[i], type: "passthrough" });
                                }
                            }
                        }
                    }
                });

                return commits;
            };

            const loadGitHistory = async () => {
                try {
                    console.log("[GitToolWindow] loadGitHistory called");
                    const logSelect = document.getElementById("gitLogBranchSelect");
                    const selected = logSelect?.value || "__ALL__";
                    const rangeArg = selected && selected !== "__ALL__" ? selected : "--all";
                    const limit = 2500;

                    const extraArgs = [];
                    if (logAuthorFilter) extraArgs.push(`--author=${quoteGitArg(logAuthorFilter)}`);
                    if (logSinceFilter) extraArgs.push(`--since=${quoteGitArg(logSinceFilter)}`);
                    if (logUntilFilter) extraArgs.push(`--until=${quoteGitArg(logUntilFilter)}`);

                    const pathSpec = logPathFilter ? ` -- ${quoteGitPath(logPathFilter)}` : "";

                    const cmd = `git log ${rangeArg} ${extraArgs.join(" ")} -${limit} --topo-order --date=iso --decorate=short --pretty=format:"%H%x1f%P%x1f%D%x1f%an%x1f%ad%x1f%s"${pathSpec}`;
                    console.log("[GitToolWindow] Running git log command:", cmd);
                    const res = await runGit(cmd, { silent: true });
                    console.log("[GitToolWindow] git log result:", {
                        ok: res.ok,
                        stdoutLength: res.stdout?.length,
                        error: res.error
                    });

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

                        console.log("[GitToolWindow] Parsed commits:", rows.length);
                        const withGraph = calculateGraphLayout(rows);
                        historyAll = withGraph;
                        console.log("[GitToolWindow] Calling applyLogFilter, historyAll length:", historyAll.length);
                        applyLogFilter();
                    } else {
                        console.error("[GitToolWindow] git log failed:", res.error);
                        showToast("error", "Git", `Failed to load history: ${res.error || 'Unknown error'}`);
                    }

                    return res;
                } catch (err) {
                    console.error("[GitToolWindow] loadGitHistory exception:", err);
                    showToast("error", "Git", `History load error: ${err.message || 'Unknown error'}`);
                    return { ok: false, error: err.message };
                }
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
                try {
                    console.log("[GitToolWindow] ensureGitLoaded called, source:", source, "skipReconnect:", skipReconnect);
                    if (ensureLoadedPromise) {
                        console.log("[GitToolWindow] ensureLoadedPromise already exists, returning it");
                        return ensureLoadedPromise;
                    }

                    ensureLoadedPromise = (async () => {
                        setGitLifecycle("detectingRepo");
                        let reconnectErr = "";

                        // If no project is open, but we have a configured repo root, use it for Git tool window.
                        try {
                            const st0 = getGitRepoState?.() || {};
                            if (!st0.projectRoot) {
                                const savedRoot = readGlobalRepoRoot();
                                if (savedRoot) {
                                    const repoManager =
                                        window.AhmadIDEModules?.git?.repoManager || window.AhmadIDE?.gitRepoManager || null;
                                    if (repoManager?.setProject) {
                                        console.log("[GitToolWindow] No project open; setting Git project root to saved repo:", savedRoot);
                                        await repoManager.setProject(savedRoot);
                                    }
                                }
                            }
                        } catch (_) { }

                        if (!skipReconnect) {
                            try {
                                console.log("[GitToolWindow] Calling gitActions.reconnect");
                                await gitActions?.reconnect?.({ reason: `toolwindow:${source || "open"}` });
                                console.log("[GitToolWindow] reconnect completed successfully");
                            } catch (err) {
                                reconnectErr = String(err?.message || err || "").trim();
                                console.error("[GitToolWindow] reconnect failed:", reconnectErr);
                            }
                        }

                        const st = getGitRepoState?.() || {};
                        console.log("[GitToolWindow] Git repo state:", {
                            projectRoot: st.projectRoot,
                            repoRoot: st.repoRoot,
                            repoDetected: st.repoDetected,
                            gitAvailable: st.gitAvailable,
                            gitDisabled: st.gitDisabled
                        });
                        syncGitActionStates();

                        if (!st.projectRoot) {
                            setGitLifecycle("idle");
                            const repoManager =
                                window.AhmadIDEModules?.git?.repoManager || window.AhmadIDE?.gitRepoManager || null;
                            setGitBanner({
                                message: "No project open. Choose a Git repository to use the Git tool window.",
                                actions: [
                                    {
                                        label: "Choose Repo…",
                                        onClick: async () => {
                                            try {
                                                if (!window.ahmadIDE?.openFolderDialog || !repoManager?.setProject) return;
                                                const res = await window.ahmadIDE.openFolderDialog();
                                                if (!res?.ok || !res.path) return;
                                                const selected = String(res.path).trim();
                                                if (!selected) return;
                                                try {
                                                    localStorage.setItem(GLOBAL_REPO_ROOT_KEY, selected);
                                                    localStorage.setItem(VISTA_REPO_ROOT_KEY, selected);
                                                } catch (_) { }
                                                await repoManager.setProject(selected);
                                                await ensureGitLoaded({ source: "choose-repo" });
                                            } catch (_) { }
                                        }
                                    }
                                ]
                            });
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
                                                placeholder: "git@github.com:user/repo.git"
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
                } catch (err) {
                    console.error("[GitToolWindow] ensureGitLoaded outer exception:", err);
                    ensureLoadedPromise = null;
                    return { ok: false, error: err.message };
                }
            };

            const openGitToolWindow = (opts = {}) => {
                ensurePanelMounted();
                wirePanelDom();
                toggleToolWindowPanel("gitToolPanel", "bottom");
                setActiveGitTab(opts.tab || "log");
                if (!opts.skipRefresh) ensureGitLoaded({ source: opts.source || "open" }).catch(() => { });
            };

            const openCommitToolWindow = (opts = {}) => {
                ensureCommitPanelMounted();
                wireCommitPanelDom();
                toggleToolWindowPanel("commitPanel", "left");
                if (!opts.skipRefresh) refreshGitStatus().catch(() => { });
                setTimeout(() => document.getElementById("commitMessageInput")?.focus?.(), 50);
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

                const historyHost = document.getElementById("gitHistoryList");
                const branchSelect = document.getElementById("gitLogBranchSelect");
                const searchInput =
                    document.getElementById("gitSearchInput") ||
                    document.getElementById("gitLogSearchInput");

                // New panel doesn't have the legacy controls; consider it "mounted" when the history host exists.
                if (!historyHost && !branchSelect && !searchInput) return;
                panelDomWired = true;

                const refreshBtn = document.getElementById("gitLogRefreshBtn") || document.getElementById("gitRefreshBtn");
                refreshBtn?.addEventListener("click", () => ensureGitLoaded({ source: "refresh" }).catch(() => { }));

                // Branch scope (All / Local / Remote)
                branchSelect?.addEventListener("change", () => {
                    const value = String(branchSelect.value || "__ALL__");
                    branchTreeSelection = value && value !== "__ALL__" ? value : "";
                    renderBranchesTree();
                    loadGitHistory().catch(() => { });
                });

                // Search filter (client-side)
                if (searchInput) {
                    let t = 0;
                    const apply = () => {
                        if (t) clearTimeout(t);
                        t = setTimeout(() => applyLogFilter(), 80);
                    };
                    searchInput.addEventListener("input", apply);
                    searchInput.addEventListener("change", apply);
                }

                // Graph toggle
                const GRAPH_PREF_KEY = "ahmadIDE:gitGraphVisible";
                const graphBtn = document.getElementById("gitShowGraphBtn");
                const applyGraphUi = (visible) => {
                    if (historyHost) historyHost.classList.toggle("git-graph-hidden", !visible);
                    if (graphBtn) {
                        graphBtn.classList.toggle("active", visible);
                        graphBtn.setAttribute("aria-pressed", visible ? "true" : "false");
                    }
                };

                try {
                    const stored = localStorage.getItem(GRAPH_PREF_KEY);
                    if (stored === "0") applyGraphUi(false);
                } catch (_) { }

                graphBtn?.addEventListener("click", () => {
                    const isVisible = historyHost ? !historyHost.classList.contains("git-graph-hidden") : true;
                    const nextVisible = !isVisible;
                    applyGraphUi(nextVisible);
                    try {
                        localStorage.setItem(GRAPH_PREF_KEY, nextVisible ? "1" : "0");
                    } catch (_) { }
                    renderGitHistory(lastHistoryItems);
                });

                // Toolbar actions
                document.getElementById("gitOpenCommitPanelBtn")?.addEventListener("click", () => openCommitToolWindow({ source: "git-toolbar" }));

                const syncFilterButtons = () => {
                    document.getElementById("gitFilterAuthorBtn")?.classList.toggle("active", !!logAuthorFilter);
                    document.getElementById("gitFilterDateBtn")?.classList.toggle("active", !!(logSinceFilter || logUntilFilter));
                    document.getElementById("gitFilterPathBtn")?.classList.toggle("active", !!logPathFilter);
                };
                syncFilterButtons();

                document.getElementById("gitFilterAuthorBtn")?.addEventListener("click", async () => {
                    const next = await showPrompt({
                        title: "Filter by Author",
                        message: "Enter an author name/email substring (leave empty to clear).",
                        placeholder: "e.g. Ahmad",
                        defaultValue: logAuthorFilter
                    });
                    if (next == null) return;
                    logAuthorFilter = String(next || "").trim();
                    syncFilterButtons();
                    loadGitHistory().catch(() => { });
                });

                document.getElementById("gitFilterDateBtn")?.addEventListener("click", async () => {
                    const since = await showPrompt({
                        title: "Filter by Date",
                        message: "Since (leave empty to clear). Examples: 2025-01-01, \"2 weeks ago\"",
                        placeholder: "e.g. 2025-01-01",
                        defaultValue: logSinceFilter
                    });
                    if (since == null) return;
                    const until = await showPrompt({
                        title: "Filter by Date",
                        message: "Until (optional). Examples: 2025-12-31, \"yesterday\"",
                        placeholder: "e.g. 2025-12-31",
                        defaultValue: logUntilFilter
                    });
                    if (until == null) return;
                    logSinceFilter = String(since || "").trim();
                    logUntilFilter = String(until || "").trim();
                    syncFilterButtons();
                    loadGitHistory().catch(() => { });
                });

                document.getElementById("gitFilterPathBtn")?.addEventListener("click", async () => {
                    const next = await showPrompt({
                        title: "Filter by Path",
                        message: "Enter a file/folder path (leave empty to clear).",
                        placeholder: "e.g. src/app/main.js",
                        defaultValue: logPathFilter
                    });
                    if (next == null) return;
                    logPathFilter = String(next || "").trim();
                    syncFilterButtons();
                    loadGitHistory().catch(() => { });
                });

                document.getElementById("gitOpenGitSettingsBtn")?.addEventListener("click", () => {
                    const dialogRegistry = window.AhmadIDEModules?.app?.dialogRegistry;
                    if (dialogRegistry?.show?.("settings")) return;
                    showToast("info", "Settings", "Settings panel is not available.");
                });
            }

            function wireCommitPanelDom() {
                if (commitPanelDomWired) return;

                const msgEl = document.getElementById("commitMessageInput");
                const commitBtn = document.getElementById("commitBtnMain");
                const commitPushBtn = document.getElementById("commitAndPushBtn");
                if (!msgEl || !commitBtn || !commitPushBtn) return;
                commitPanelDomWired = true;

                const amendCb = document.getElementById("commitAmend");

                const perform = async (andPush) => {
                    const message = String(msgEl.value || "").trim();
                    if (!message) {
                        showToast("error", "Commit", "Commit message is required");
                        return;
                    }

                    const repoState = getGitRepoState?.() || {};
                    if (repoState.gitDisabled) {
                        showToast("error", "Git", "Git is disabled for this project");
                        return;
                    }
                    if (!repoState.repoDetected) {
                        showToast("error", "Git", "No Git repository detected");
                        return;
                    }

                    // Stage everything (commit panel currently has no per-file staging UI)
                    await runGit("git add -A", { silent: true });

                    const isAmend = amendCb?.checked || false;
                    const flags = [];
                    if (isAmend) flags.push("--amend");

                    const quotedMsg = message.replace(/'/g, "'\\''");
                    const cmd = `git commit ${flags.join(" ")} -m '${quotedMsg}'`.trim();

                    const res = await runGit(cmd, { silent: true });
                    if (!res.ok) {
                        showToast("error", "Commit", res.error || "Commit failed");
                        return;
                    }

                    showToast("success", "Commit", isAmend ? "Commit amended" : "Committed successfully");
                    msgEl.value = "";
                    if (amendCb) amendCb.checked = false;

                    await refreshGitStatus().catch(() => { });
                    await loadGitHistory().catch(() => { });

                    if (andPush) {
                        const pushRes = await runGit("git push", { silent: true });
                        if (pushRes.ok) showToast("success", "Git", "Pushed to remote");
                        else showToast("error", "Git", pushRes.error || "Push failed");
                    }
                };

                const sync = () => syncCommitPanelActions();

                msgEl.addEventListener("input", sync);
                amendCb?.addEventListener("change", sync);
                commitBtn.addEventListener("click", () => perform(false).catch(() => { }));
                commitPushBtn.addEventListener("click", () => perform(true).catch(() => { }));

                document.getElementById("commitRefreshBtn")?.addEventListener("click", () => {
                    refreshGitStatus().catch(() => { });
                });

                document.getElementById("commitExpandBtn")?.addEventListener("click", () => {
                    document.getElementById("commitFileList")?.classList.remove("hidden");
                });

                document.getElementById("commitCollapseBtn")?.addEventListener("click", () => {
                    document.getElementById("commitFileList")?.classList.add("hidden");
                });

                document.getElementById("commitCloseBtn")?.addEventListener("click", () => {
                    toggleToolWindowPanel("projectPanel", "left");
                });

                document.getElementById("commitDiffBtn")?.addEventListener("click", async () => {
                    const list = Array.isArray(lastStatusEntries) ? lastStatusEntries : [];
                    const fallback = list.length === 1 ? list[0] : null;
                    const pick = commitPanelSelected?.path ? commitPanelSelected : null;
                    const entry = pick?.path
                        ? {
                            path: pick.path,
                            oldPath: pick.oldPath,
                            status: pick.status,
                            staged: pick.staged
                        }
                        : fallback;
                    const fp = String(entry?.path || "").trim();
                    if (!fp) {
                        showToast("info", "Diff", "Select a file in the Commit panel to view its diff.");
                        return;
                    }
                    await openDiffTab({
                        kind: "worktree",
                        path: fp,
                        oldPath: String(entry?.oldPath || "").trim(),
                        status: String(entry?.status || "").trim(),
                        staged: !!entry?.staged,
                        source: "commit-panel-toolbar"
                    });
                });

                document.getElementById("commitRollbackBtn")?.addEventListener("click", async () => {
                    const fp = String(commitPanelSelected?.path || "").trim();
                    if (!fp) {
                        showToast("info", "Rollback", "Select a file in the Commit panel first.");
                        return;
                    }

                    const ok = await showConfirm({
                        title: "Discard Changes",
                        message: `Discard changes for:\n\n${fp}\n\nThis cannot be undone.`,
                        variant: "danger"
                    });
                    if (!ok) return;

                    if (commitPanelSelected?.status === "?") {
                        showToast("error", "Rollback", "Cannot rollback an untracked file.");
                        return;
                    }

                    const quoted = quoteGitPath(fp);
                    if (commitPanelSelected?.staged) {
                        await runGit(`git restore --staged -- ${quoted}`, { silent: true });
                    }
                    await runGit(`git restore -- ${quoted}`, { silent: true });
                    await refreshGitStatus().catch(() => { });
                });

                document.getElementById("commitDownloadBtn")?.addEventListener("click", async () => {
                    const staged = await runGit("git diff --cached", { silent: true });
                    const unstaged = await runGit("git diff", { silent: true });

                    const chunks = [];
                    const stagedText = String(staged?.stdout || "").trim();
                    const unstagedText = String(unstaged?.stdout || "").trim();
                    if (stagedText) chunks.push(`# Staged\n${stagedText}`);
                    if (unstagedText) chunks.push(`# Unstaged\n${unstagedText}`);

                    const patch = chunks.join("\n\n").trim();
                    if (!patch) {
                        showToast("info", "Patch", "No changes to export.");
                        return;
                    }

                    await copyToClipboard(patch, "Patch copied to clipboard");
                });

                document.getElementById("commitChangesToggle")?.addEventListener("click", () => {
                    const list = document.getElementById("commitFileList");
                    if (!list) return;
                    list.classList.toggle("hidden");
                });

                document.getElementById("commitHeaderMinimizeBtn")?.addEventListener("click", () => {
                    toggleToolWindowPanel("projectPanel", "left");
                });

                document.getElementById("commitHeaderMenuBtn")?.addEventListener("click", () => {
                    showToast("info", "Commit", "Menu not implemented yet");
                });

                document.getElementById("commitAmendOptionsBtn")?.addEventListener("click", () => {
                    showToast("info", "Commit", "Amend options not implemented yet");
                });
                document.getElementById("commitTimeBtn")?.addEventListener("click", () => {
                    showToast("info", "Commit", "Commit time not implemented yet");
                });
                document.getElementById("commitCleanupBtn")?.addEventListener("click", () => {
                    showToast("info", "Commit", "Cleanup not implemented yet");
                });
                document.getElementById("commitSettingsBtn")?.addEventListener("click", () => {
                    const dialogRegistry = window.AhmadIDEModules?.app?.dialogRegistry;
                    if (dialogRegistry?.show?.("settings")) return;
                    showToast("info", "Settings", "Settings panel is not available.");
                });

                syncCommitPanelActions();
                syncCommitPanelChanges();
            }

            function syncCommitPanelActions() {
                const msgEl = document.getElementById("commitMessageInput");
                const commitBtn = document.getElementById("commitBtnMain");
                const commitPushBtn = document.getElementById("commitAndPushBtn");

                if (!msgEl || !commitBtn || !commitPushBtn) return;

                const repoState = getGitRepoState?.() || {};
                const repoReady = !!repoState.repoDetected && !!repoState.gitAvailable && !repoState.gitDisabled;
                const hasMessage = Boolean(String(msgEl.value || "").trim());
                const hasChanges = Array.isArray(lastStatusEntries) && lastStatusEntries.length > 0;
                const allowNoChanges = !!(document.getElementById("commitAmend")?.checked);
                const canCommit = repoReady && hasMessage && (hasChanges || allowNoChanges);

                commitBtn.disabled = !canCommit;
                commitPushBtn.disabled = !canCommit;
            }

            function syncCommitPanelChanges() {
                const listHost = document.getElementById("commitFileList");
                const countEl = document.getElementById("commitChangesCount");
                if (!listHost && !countEl) return;

                const list = Array.isArray(lastStatusEntries) ? lastStatusEntries : [];
                if (
                    commitPanelSelected?.path &&
                    !list.some(
                        (e) => String(e?.path || "").trim() === commitPanelSelected.path && !!e?.staged === !!commitPanelSelected.staged
                    )
                ) {
                    commitPanelSelected = { path: "", staged: false, status: "", oldPath: "" };
                }
                if (countEl) countEl.textContent = list.length === 1 ? "1 file" : `${list.length} files`;

                if (!listHost) return;
                listHost.innerHTML = "";

                if (!list.length) {
                    listHost.textContent = "No changes";
                    return;
                }

                list.forEach((ent) => {
                    const fp = String(ent.path || "").trim();
                    const row = document.createElement("div");
                    row.className = "git-file-row";
                    row.tabIndex = 0;
                    row.dataset.path = fp;
                    row.dataset.staged = ent.staged ? "1" : "0";

                    const status = document.createElement("span");
                    status.className = "git-file-status";
                    status.textContent = String(ent.status || "").trim() || "M";

                    const p = document.createElement("span");
                    p.className = "git-file-path";
                    p.textContent = String(ent.path || "").trim();

                    row.appendChild(status);
                    row.appendChild(p);

                    const open = async () => {
                        if (!fp) return;
                        commitPanelSelected = {
                            path: fp,
                            staged: !!ent.staged,
                            status: String(ent.status || "").trim(),
                            oldPath: String(ent.oldPath || "").trim()
                        };
                        listHost.querySelectorAll(".git-file-row").forEach((el) => {
                            const samePath = String(el.dataset?.path || "") === fp;
                            const sameStaged = String(el.dataset?.staged || "0") === (ent.staged ? "1" : "0");
                            el.classList.toggle("selected", samePath && sameStaged);
                        });
                        await openDiffTab({
                            kind: "worktree",
                            path: fp,
                            oldPath: String(ent.oldPath || "").trim(),
                            status: String(ent.status || "").trim(),
                            staged: !!ent.staged,
                            source: "commit-panel"
                        });
                    };

                    row.classList.toggle(
                        "selected",
                        !!fp && commitPanelSelected.path === fp && !!commitPanelSelected.staged === !!ent.staged
                    );
                    row.addEventListener("click", () => open().catch(() => { }));
                    row.addEventListener("keydown", (e) => {
                        if (e.key === "Enter" || e.key === " ") {
                            e.preventDefault();
                            open().catch(() => { });
                        }
                    });

                    listHost.appendChild(row);
                });
            }

            document.getElementById("toolbarGitBtn")?.addEventListener("click", () => openGitToolWindow());
            document.getElementById("toolbarCommitBtn")?.addEventListener("click", () => openCommitToolWindow());

            // Tool window activation
            window.addEventListener("ahmadIDE:toolwindow-activated", (e) => {
                const panelId = e?.detail?.panelId;
                if (panelId === "gitToolPanel") {
                    wirePanelDom();
                    ensureGitLoaded({ source: "activated" }).catch(() => { });
                    return;
                }
                if (panelId === "commitPanel") {
                    wireCommitPanelDom();
                    refreshGitStatus().catch(() => { });
                }
            });

            // Keep Git UI in sync with repo state changes
            try {
                subscribeGitRepoState((s) => {
                    if (!s) return;
                    if (isGitPanelVisible()) {
                        ensureGitLoaded({ source: "repo-state", skipReconnect: true }).catch(() => { });
                    } else if (isCommitPanelVisible()) {
                        refreshGitStatus().catch(() => { });
                    }
                });
            } catch (_) { }

            // Lazy-mount support
            if (featureRegistry && typeof featureRegistry.onMounted === "function") {
                featureRegistry.onMounted("gitToolPanel", () => {
                    wirePanelDom();
                    const panel = document.getElementById("gitToolPanel");
                    if (panel && !panel.classList.contains("hidden")) ensureGitLoaded({ source: "mounted" }).catch(() => { });
                });
                featureRegistry.onMounted("commitPanel", () => {
                    wireCommitPanelDom();
                    const panel = document.getElementById("commitPanel");
                    if (panel && !panel.classList.contains("hidden")) refreshGitStatus().catch(() => { });
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
})();
