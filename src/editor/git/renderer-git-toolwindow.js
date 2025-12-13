(() => {
    function createGitToolWindowManager({ deps } = {}) {
        const showToast = deps?.showToast || (() => { });
        const normalizeGitError = deps?.normalizeGitError || ((text, fallback = 'Git command failed') => {
            if (/not a git repository/i.test(text || '')) {
                return 'Git is not configured for this project';
            }
            return text || fallback;
        });
        const toggleToolWindowPanel = deps?.toggleToolWindowPanel || (() => { });

        function wireGitToolWindow({ fetchCurrentBranch } = {}) {
            const gitOutput = (text) => {
                const out = document.getElementById('gitOutput');
                if (out) {
                    out.textContent += `${text}\n`;
                    out.scrollTop = out.scrollHeight;
                }
            };
            const gitError = (text) => {
                const message = normalizeGitError(text);
                const out = document.getElementById('gitOutput');
                if (out) {
                    out.textContent += `✗ ${message}\n`;
                    out.scrollTop = out.scrollHeight;
                }
                showToast('error', 'Git', message);
            };
            const setDiffPanes = (left, right) => {
                const l = document.getElementById('gitDiffLeft');
                const r = document.getElementById('gitDiffRight');
                if (l) l.textContent = left || 'No data';
                if (r) r.textContent = right || 'No data';
            };

            const renderSideBySideDiff = (diffText) => {
                const l = document.getElementById('gitDiffLeft');
                const r = document.getElementById('gitDiffRight');
                if (!l || !r) return;
                const lines = (diffText || '').split('\n');
                const leftLines = [];
                const rightLines = [];
                let leftNo = 0;
                let rightNo = 0;
                lines.forEach(line => {
                    if (line.startsWith('+++') || line.startsWith('---')) return;
                    if (line.startsWith('@@')) {
                        leftLines.push(`<span class="diff-line diff-hunk">${line}</span>`);
                        rightLines.push(`<span class="diff-line diff-hunk">${line}</span>`);
                        const match = line.match(/-([0-9]+)/);
                        const matchR = line.match(/\+([0-9]+)/);
                        leftNo = match ? parseInt(match[1], 10) - 1 : leftNo;
                        rightNo = matchR ? parseInt(matchR[1], 10) - 1 : rightNo;
                        return;
                    }
                    if (line.startsWith('+')) {
                        rightNo += 1;
                        rightLines.push(
                            `<span class="diff-line diff-add"><span class="lineno">${rightNo}</span>${line.replace(/</g, '&lt;')}</span>`
                        );
                    } else if (line.startsWith('-')) {
                        leftNo += 1;
                        leftLines.push(
                            `<span class="diff-line diff-del"><span class="lineno">${leftNo}</span>${line.replace(/</g, '&lt;')}</span>`
                        );
                    } else {
                        leftNo += 1;
                        rightNo += 1;
                        const safe = line.replace(/</g, '&lt;');
                        leftLines.push(`<span class="diff-line diff-context"><span class="lineno">${leftNo}</span>${safe}</span>`);
                        rightLines.push(`<span class="diff-line diff-context"><span class="lineno">${rightNo}</span>${safe}</span>`);
                    }
                });
                l.innerHTML = leftLines.join('') || 'No left changes';
                r.innerHTML = rightLines.join('') || 'No right changes';
            };

            const gitSelected = { staged: new Set(), unstaged: new Set() };

            const renderGitChanges = (entries = []) => {
                const unstagedHost = document.getElementById('gitChangesUnstaged');
                const stagedHost = document.getElementById('gitChangesStaged');
                if (unstagedHost) unstagedHost.innerHTML = '';
                if (stagedHost) stagedHost.innerHTML = '';
                const render = (host, list, staged) => {
                    if (!host) return;
                    if (!list.length) {
                        host.textContent = staged ? 'No staged files.' : 'No local changes.';
                        return;
                    }
                    list.forEach(ent => {
                        const row = document.createElement('div');
                        row.className = 'git-change-row' + (staged ? ' staged' : '');
                        const checkbox = document.createElement('input');
                        checkbox.type = 'checkbox';
                        checkbox.checked = staged ? gitSelected.staged.has(ent.path) : gitSelected.unstaged.has(ent.path);
                        checkbox.addEventListener('change', () => {
                            const targetSet = staged ? gitSelected.staged : gitSelected.unstaged;
                            checkbox.checked ? targetSet.add(ent.path) : targetSet.delete(ent.path);
                        });
                        const status = document.createElement('span');
                        status.className = 'git-change-status';
                        status.textContent = ent.status;
                        const path = document.createElement('span');
                        path.className = 'git-change-path';
                        path.textContent = ent.path;
                        row.appendChild(checkbox);
                        row.appendChild(status);
                        row.appendChild(path);
                        host.appendChild(row);
                    });
                };
                const stagedList = entries.filter(e => e.staged);
                const unstagedList = entries.filter(e => !e.staged);
                render(unstagedHost, unstagedList, false);
                render(stagedHost, stagedList, true);
            };

            const renderGitHistory = (lines = []) => {
                const host = document.getElementById('gitHistoryList');
                if (!host) return;
                host.innerHTML = '';
                if (!lines.length) {
                    host.textContent = 'No history yet.';
                    return;
                }
                lines.forEach(line => {
                    const div = document.createElement('div');
                    div.textContent = line;
                    host.appendChild(div);
                });
            };

            const runGit = async (cmd, opts = {}) => {
                if (!opts.silent) gitOutput(`$ ${cmd}`);
                const res = await window.ahmadIDE.git(cmd);
                if (res.ok) {
                    if (opts.onSuccess) opts.onSuccess(res.stdout || '');
                    if (!opts.silent) {
                        if (res.stdout) gitOutput(res.stdout);
                        if (res.stderr) gitOutput(res.stderr);
                    }
                } else {
                    gitError(res.error || res.stderr || 'Git command failed');
                }
                return res;
            };

            const refreshGitStatus = async () => {
                gitSelected.staged.clear();
                gitSelected.unstaged.clear();
                const statusRes = await runGit('git status --short --branch', {
                    onSuccess: (out) => {
                        const lines = (out || '').split('\n').filter(Boolean);
                        const entries = [];
                        lines.forEach(line => {
                            if (line.startsWith('##')) return; // branch info
                            const indexStatus = line[0];
                            const worktreeStatus = line[1];
                            const path = line.slice(3).trim();
                            const staged = indexStatus !== ' ' && indexStatus !== '?';
                            const status = staged ? indexStatus : worktreeStatus;
                            entries.push({ status, path, staged });
                        });
                        renderGitChanges(entries);
                    }
                });
                if (!statusRes.ok) return statusRes;
                return runGit('git branch --format="%(refname:short)"', {
                    silent: true,
                    onSuccess: (out) => {
                        const select = document.getElementById('gitBranchSelect');
                        if (!select) return;
                        select.innerHTML = '';
                        (out || '').split('\n').filter(Boolean).forEach(b => {
                            const opt = document.createElement('option');
                            opt.value = b;
                            opt.textContent = b;
                            select.appendChild(opt);
                        });
                    }
                });
                if (statusRes.ok) {
                    fetchCurrentBranch().catch(() => { });
                }
            };

            const loadGitHistory = async () => {
                await runGit('git log -10 --oneline', {
                    onSuccess: (out) => renderGitHistory((out || '').split('\n').filter(Boolean))
                });
            };

            const openGitToolWindow = (opts = {}) => {
                toggleToolWindowPanel('gitToolPanel', 'bottom');
                if (!opts.skipRefresh) {
                    refreshGitStatus().catch(() => { });
                    loadGitHistory().catch(() => { });
                }
            };

            const openCommitToolWindow = () => {
                openGitToolWindow();
                const msg = document.getElementById('gitCommitMessage');
                msg?.focus();
            };

            const openGitPanel = () => openGitToolWindow();

            document.getElementById('gitRefreshBtn')?.addEventListener('click', refreshGitStatus);
            document.getElementById('gitStatusBtn')?.addEventListener('click', refreshGitStatus);
            document.getElementById('gitLogBtn')?.addEventListener('click', loadGitHistory);
            document.getElementById('gitDiffBtn')?.addEventListener('click', () => runGit('git diff --stat'));
            document.getElementById('gitClearBtn')?.addEventListener('click', () => {
                document.getElementById('gitOutput').textContent = 'Git ready.';
            });

            const stageOrUnstage = async (targetSet, staged) => {
                const files = Array.from(targetSet);
                if (!files.length) {
                    gitOutput(staged ? 'No staged selection.' : 'No unstaged selection.');
                    return;
                }
                const cmd = staged
                    ? `git restore --staged ${files.map(f => `"${f.replace(/"/g, '\\"')}"`).join(' ')}`
                    : `git add ${files.map(f => `"${f.replace(/"/g, '\\"')}"`).join(' ')}`;
                await runGit(cmd);
                await refreshGitStatus();
            };

            document.getElementById('gitStageSelectedBtn')?.addEventListener('click', () => stageOrUnstage(gitSelected.unstaged, false));
            document.getElementById('gitUnstageSelectedBtn')?.addEventListener('click', () => stageOrUnstage(gitSelected.staged, true));

            document.getElementById('gitDiffSelectedBtn')?.addEventListener('click', async () => {
                const path = [...gitSelected.unstaged, ...gitSelected.staged][0];
                if (!path) {
                    gitError('Select a file to diff');
                    return;
                }
                await runGit(`git diff -- ${path}`);
            });

            document.getElementById('gitCommitBtn')?.addEventListener('click', async () => {
                const msgEl = document.getElementById('gitCommitMessage');
                const message = msgEl?.value.trim() || '';
                if (!message) {
                    gitOutput('✗ Commit message required');
                    return;
                }
                const res = await runGit(`git commit -m "${message.replace(/"/g, '\\"')}"`);
                if (res?.ok) {
                    msgEl.value = '';
                    await refreshGitStatus();
                    await loadGitHistory();
                }
            });
            document.getElementById('gitPushBtn')?.addEventListener('click', () => runGit('git push'));
            document.getElementById('gitPullBtn')?.addEventListener('click', () => runGit('git pull'));
            document.getElementById('gitFetchBtn')?.addEventListener('click', () => runGit('git fetch'));
            document.getElementById('gitCheckoutBtn')?.addEventListener('click', async () => {
                const select = document.getElementById('gitBranchSelect');
                const input = document.getElementById('gitBranchInput');
                const target = (input?.value.trim()) || (select?.value || '');
                if (!target) {
                    gitOutput('✗ No branch specified');
                    return;
                }
                await runGit(`git checkout ${target}`);
                await refreshGitStatus();
            });
            document.getElementById('gitDiffFileBtn')?.addEventListener('click', async () => {
                const path = document.getElementById('gitDiffPath')?.value.trim();
                if (!path) {
                    gitError('No path provided for diff');
                    return;
                }
                const diffRes = await runGit(`git diff -- ${path}`);
                const headRes = await window.ahmadIDE.git(`git show HEAD:"${path.replace(/"/g, '\\"')}"`);
                const workRes = await window.ahmadIDE.hostExec(`cat "${path.replace(/"/g, '\\"')}"`);
                setDiffPanes(
                    headRes.ok ? headRes.stdout || '(empty)' : '(no HEAD version)',
                    workRes.ok ? workRes.stdout || '(empty)' : workRes.error || '(cannot read)'
                );
                if (diffRes.ok && diffRes.stdout) renderSideBySideDiff(diffRes.stdout);
            });
            document.getElementById('gitHistoryFileBtn')?.addEventListener('click', async () => {
                const path = document.getElementById('gitDiffPath')?.value.trim();
                if (!path) {
                    gitError('No path provided for history');
                    return;
                }
                await runGit(`git log --oneline -- ${path}`);
            });
            document.getElementById('gitCompareBtn')?.addEventListener('click', async () => {
                const a = document.getElementById('gitComparePathA')?.value.trim();
                const b = document.getElementById('gitComparePathB')?.value.trim();
                if (!a || !b) {
                    gitError('Provide both paths to compare');
                    return;
                }
                const aRes = await window.ahmadIDE.hostExec(`cat "${a.replace(/"/g, '\\"')}"`);
                const bRes = await window.ahmadIDE.hostExec(`cat "${b.replace(/"/g, '\\"')}"`);
                setDiffPanes(
                    aRes.ok ? aRes.stdout || '(empty)' : aRes.error || '(cannot read)',
                    bRes.ok ? bRes.stdout || '(empty)' : bRes.error || '(cannot read)'
                );
                const diffRes = await runGit(`git diff -- ${a} ${b}`);
                if (diffRes.ok && diffRes.stdout) renderSideBySideDiff(diffRes.stdout);
            });

            document.getElementById('toolbarGitBtn')?.addEventListener('click', () => {
                openGitToolWindow();
            });

            document.getElementById('toolbarCommitBtn')?.addEventListener('click', () => {
                openCommitToolWindow();
            });

            const vcsWidget = document.getElementById('vcsWidget');
            const vcsToggle = document.getElementById('vcsWidgetBtn');
            const vcsMenu = document.getElementById('vcsWidgetMenu');
            const handleVcsMenuAction = (action) => {
                switch (action) {
                    case 'commit':
                        openCommitToolWindow();
                        break;
                    case 'history':
                        openGitToolWindow();
                        document.getElementById('gitLogBtn')?.click();
                        break;
                    case 'push':
                        document.getElementById('gitPushBtn')
                            ? document.getElementById('gitPushBtn').click()
                            : showToast('info', 'Git', 'NOT IMPLEMENTED YET: Push');
                        break;
                    case 'pull': {
                        const pullBtn = document.getElementById('gitPullBtn') || document.getElementById('gitFetchBtn');
                        if (pullBtn) pullBtn.click();
                        else showToast('info', 'Git', 'NOT IMPLEMENTED YET: Pull / Fetch');
                        break;
                    }
                    case 'open-git':
                        openGitToolWindow();
                        break;
                    default:
                        showToast('info', 'Git', `NOT IMPLEMENTED YET: ${action}`);
                }
            };

            const closeVcsMenu = () => vcsMenu?.classList.add('hidden');

            vcsToggle?.addEventListener('click', (e) => {
                e.stopPropagation();
                if (!vcsMenu) return;
                const shouldOpen = vcsMenu.classList.contains('hidden');
                closeVcsMenu();
                if (shouldOpen) vcsMenu.classList.remove('hidden');
            });

            vcsMenu?.querySelectorAll('.vcs-menu-item').forEach((btn) => {
                btn.addEventListener('click', (e) => {
                    e.stopPropagation();
                    handleVcsMenuAction(btn.getAttribute('data-action'));
                    closeVcsMenu();
                });
            });

            document.addEventListener('click', (e) => {
                if (vcsWidget && !vcsWidget.contains(e.target)) {
                    closeVcsMenu();
                }
            });

            // Initial refresh when tool window wires up
            refreshGitStatus();
            loadGitHistory(); // TODO: add pagination / filters for larger histories

            return {
                openGitToolWindow,
                openCommitToolWindow,
                openGitPanel
            };
        }

        return {
            wireGitToolWindow
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.git = window.AhmadIDEModules.git || {};
        window.AhmadIDEModules.git.createGitToolWindowManager = createGitToolWindowManager;
    }
})();
