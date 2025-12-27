(() => {
    function createStatusBarWiring({ deps } = {}) {
        const getGitRepoManager = deps?.getGitRepoManager;
        const getCurrentProject = deps?.getCurrentProject;

        function wireStatusBar(editor) {
            // --- Status Bar Updates ---
            const updateStatusBar = () => {
                const position = editor.getPosition();
                if (position) {
                    const lineCol = document.getElementById('lineColInfo');
                    if (lineCol) {
                        lineCol.textContent = `Ln ${position.lineNumber}, Col ${position.column}`;
                    }
                }
            };

            // Update status bar on cursor position change
            editor.onDidChangeCursorPosition(updateStatusBar);
            updateStatusBar(); // Initial update

            const setBranchDisplay = (branchName) => {
                const label = (branchName && branchName.trim()) ? branchName.trim() : 'Git';
                const branchEl = document.getElementById('gitBranch');
                if (branchEl) {
                    branchEl.innerHTML = `<span class="icon">⎇</span> ${label}`;
                }
                const vcsToggle = document.getElementById('vcsWidgetBtn');
                if (vcsToggle) {
                    const hint = label === 'Git' ? 'Git' : `Git: ${label}`;
                    vcsToggle.title = hint;
                    vcsToggle.setAttribute('aria-label', hint);
                    vcsToggle.dataset.branch = label;
                }
                const vcsLabel = document.getElementById('vcsWidgetLabel');
                if (vcsLabel) {
                    vcsLabel.textContent = label;
                }
            };

            const fetchCurrentBranch = async () => {
                try {
                    const gitRepoManager = getGitRepoManager ? getGitRepoManager() : null;
                    const currentProject = getCurrentProject ? getCurrentProject() : null;
                    const repoState = gitRepoManager?.getState ? gitRepoManager.getState() : null;
                    if (repoState?.gitDisabled) return null;
                    if (repoState && !repoState.gitAvailable) return null;
                    if (repoState && !repoState.repoDetected) {
                        setBranchDisplay('Git');
                        return null;
                    }

                    if (currentProject && currentProject.projectPath) {
                        const branchRes = await (gitRepoManager?.runGit
                            ? gitRepoManager.runGit('git branch --show-current')
                            : window.ahmadIDE.git('git branch --show-current'));
                        if (branchRes?.ok && branchRes.stdout) {
                            const name = branchRes.stdout.trim();
                            setBranchDisplay(name || 'Git');
                            return name || 'Git';
                        }
                    }
                } catch (_) { }
                return null;
            };

            // Keep branch / tooltip in sync with repo detection changes.
            try {
                const gitRepoManager = getGitRepoManager ? getGitRepoManager() : null;
                gitRepoManager?.subscribe?.((s) => {
                    if (!s || s.gitDisabled || !s.gitAvailable || !s.repoDetected) {
                        setBranchDisplay('Git');
                        return;
                    }
                    fetchCurrentBranch().catch(() => { });
                });
            } catch (_) { }

            // Update Git branch if in a git repo
            fetchCurrentBranch();

            return { fetchCurrentBranch };
        }

        return { wireStatusBar };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.status = window.AhmadIDEModules.renderer.status || {};
        window.AhmadIDEModules.renderer.status.createStatusBarWiring = createStatusBarWiring;
    }
})();
