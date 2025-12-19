(() => {
    const panelTemplates = {
        terminalPanel: `
            <div class="pane-header terminal-header">
                <div class="pane-title">Terminal</div>
                <div class="pane-actions">
                    <div class="pill subtle" id="terminalStatusPill" title="Toggle with Alt+F12">Alt+F12</div>
                    <button class="btn ghost icon-btn" id="terminalNewTabBtn" title="New Terminal Tab (Ctrl+Shift+T)">+</button>
                    <button class="btn ghost icon-btn" id="terminalHideBtn" title="Hide Terminal">✕</button>
                </div>
            </div>
            <div class="terminal-tabbar">
                <div id="terminalTabs" class="terminal-tabs"></div>
                <div class="terminal-tab-actions">
                    <button class="btn ghost icon-btn" id="terminalClearBtn" title="Clear Active Terminal">Clear</button>
                </div>
            </div>
            <div class="terminal-surface">
                <div class="terminal-viewport" id="terminalViewport" aria-label="Terminal"></div>
                <div class="terminal-error hidden" id="terminalError" role="alert"></div>
            </div>
        `,
        debugPanel: `
            <div class="pane-header">
                <div class="pane-title">Debug</div>
                <div class="pill subtle">Breakpoints / Locals / Stack / Output</div>
                <div class="pane-subtitle">Tip: while paused, hover variables in the editor to see their values.</div>
            </div>
            <div class="debug-tabs">
                <div class="debug-tab active" data-tab="tab-breakpoints">Breakpoints</div>
                <div class="debug-tab" data-tab="tab-variables">Variables</div>
                <div class="debug-tab" data-tab="tab-watches">Watches</div>
                <div class="debug-tab" data-tab="tab-stack">Call Stack</div>
                <div class="debug-tab" data-tab="tab-console">Console</div>
                <div class="debug-tab" data-tab="tab-problems">Problems</div>
            </div>
            <div class="debug-tabpanes">
                <div class="debug-tabpane active" id="tab-breakpoints">
                    <div class="bp-action-row">
                        <button class="btn ghost" id="bpClearAllBtn">Clear All</button>
                    </div>
                    <ul class="problems-list" id="breakpointsList"></ul>
                </div>
                <div class="debug-tabpane" id="tab-variables">
                    <ul class="problems-list" id="localsList"></ul>
                </div>
                <div class="debug-tabpane" id="tab-watches">
                    <div class="pane-subtitle">Watches not implemented.</div>
                </div>
                <div class="debug-tabpane" id="tab-stack">
                    <ul class="problems-list" id="stackList"></ul>
                </div>
                <div class="debug-tabpane" id="tab-console">
                    <pre class="debug-output" id="debugOutput"></pre>
                </div>
                <div class="debug-tabpane" id="tab-problems">
                    <ul class="problems-list" id="problemsList"></ul>
                </div>
            </div>
        `,
        problemsPanel: `
            <div class="pane-header">
                <div class="pane-title">Problems</div>
                <div class="pill subtle">Diagnostics</div>
            </div>
            <ul class="problems-list" id="problemsListStandalone"></ul>
        `,
        gitToolPanel: `
            <div class="pane-header">
                <div class="pane-title">Git</div>
                <div class="pill subtle">Status / Log / Diff</div>
            </div>
            <div class="git-grid">
                <div class="git-left">
                    <div class="pane-title">Local Changes</div>
                    <div class="pane-subtitle">Unstaged</div>
                    <div class="git-changes-list" id="gitChangesUnstaged">Run status to load changes.</div>
                    <div class="pane-subtitle" style="margin-top:6px;">Staged</div>
                    <div class="git-changes-list" id="gitChangesStaged">No staged files.</div>
                    <div class="flex git-actions" style="margin-top:8px;">
                        <button class="btn ghost" id="gitRefreshBtn">Refresh</button>
                        <button class="btn ghost" id="gitStageSelectedBtn">Stage</button>
                        <button class="btn ghost" id="gitUnstageSelectedBtn">Unstage</button>
                        <button class="btn ghost" id="gitDiffSelectedBtn">Diff Selected</button>
                    </div>
                    <div class="pane-title" style="margin-top:10px;">Commit</div>
                    <textarea id="gitCommitMessage" class="git-commit-input" placeholder="Commit message"></textarea>
                    <div class="flex git-actions">
                        <button class="btn primary" id="gitCommitBtn">Commit</button>
                        <button class="btn ghost" id="gitPushBtn">Push</button>
                        <button class="btn ghost" id="gitPullBtn">Pull</button>
                        <button class="btn ghost" id="gitFetchBtn">Fetch</button>
                    </div>
                    <div class="pane-title" style="margin-top:6px;">Branch</div>
                    <div class="git-branch-row">
                        <select id="gitBranchSelect" class="git-branch-select"></select>
                        <input id="gitBranchInput" class="git-branch-input" placeholder="New branch name">
                        <button class="btn ghost" id="gitCheckoutBtn">Checkout</button>
                    </div>
                </div>
                <div class="git-right">
                    <div class="pane-title">History</div>
                    <div class="flex" style="margin-bottom:6px; flex-wrap: wrap;">
                        <button class="btn ghost" id="gitStatusBtn">Status</button>
                        <button class="btn ghost" id="gitLogBtn">Log</button>
                        <button class="btn ghost" id="gitDiffBtn">Diff (stat)</button>
                        <button class="btn ghost" id="gitClearBtn">Clear</button>
                    </div>
                    <div class="git-history-list" id="gitHistoryList">History not loaded.</div>
                    <div class="pane-title" style="margin-top:8px;">Console</div>
                    <pre class="debug-output" id="gitOutput">Git ready.</pre>
                    <div class="pane-title" style="margin-top:8px;">File Diff</div>
                    <div class="flex" style="gap:6px; margin-bottom:6px;">
                        <input class="ssh-input" id="gitDiffPath" placeholder="Path (relative)">
                        <button class="btn ghost" id="gitDiffFileBtn">Show Diff</button>
                        <button class="btn ghost" id="gitHistoryFileBtn">File History</button>
                    </div>
                    <div class="pane-title" style="margin-top:6px;">Compare Two Files</div>
                    <div class="flex" style="gap:6px; margin-bottom:6px;">
                        <input class="ssh-input" id="gitComparePathA" placeholder="Path A">
                        <input class="ssh-input" id="gitComparePathB" placeholder="Path B">
                        <button class="btn ghost" id="gitCompareBtn">Compare</button>
                    </div>
                    <div class="git-diff-grid">
                        <pre class="debug-output" id="gitDiffLeft">Left</pre>
                        <pre class="debug-output" id="gitDiffRight">Right</pre>
                    </div>
                </div>
            </div>
        `,
        extensionsPanel: `
            <div class="pane-header">
                <div class="pane-title">Extensions</div>
                <div class="pill subtle">Plugins / Add-ons</div>
            </div>
            <div class="extensions-grid">
                <div class="extensions-list" id="extensionsList">Loading extensions...</div>
                <div class="extensions-detail" id="extensionDetail">
                    <div class="pane-title">Select an extension</div>
                    <div class="pane-subtitle">Details will appear here.</div>
                </div>
            </div>
        `,
        servicesPanel: `
            <div class="pane-header">
                <div class="pane-title">Services</div>
                <div class="pill subtle">Docker / SSH</div>
            </div>
            <div class="pane-title">Docker</div>
            <div class="pane-subtitle">List running containers</div>
            <div class="flex" style="margin-bottom:6px;">
                <button class="btn ghost" id="svcDockerListBtn">List Containers</button>
                <button class="btn ghost" id="svcDockerRefreshBtn">Refresh</button>
            </div>
            <div class="git-changes-list" id="svcDockerOutput">Docker services not queried.</div>
            <div class="pane-title" style="margin-top:10px;">SSH</div>
            <div class="pane-subtitle">Run simple command</div>
            <div class="flex" style="gap:6px; margin-bottom:6px;">
                <input class="ssh-input" id="svcSshCmd" placeholder="Command (requires SSH config)">
                <button class="btn ghost" id="svcSshRunBtn">Run</button>
            </div>
            <div class="git-changes-list" id="svcSshOutput">SSH services not queried.</div>
        `,
        connectionsPanel: `
            <div class="connections-header">
                <div>
                    <div class="pane-title">Connections</div>
                    <div class="pane-subtitle">Switch between Docker and SSH</div>
                </div>
                <button class="btn ghost icon-btn" id="closeConnectionsBtn" title="Close">✕</button>
            </div>

            <!-- Docker Setup Notification -->
            <div id="dockerSetupNotice" class="docker-setup-notice hidden" style="margin: 12px; padding: 12px; background: #2a4d6e; border-left: 4px solid #4a90e2; border-radius: 4px;">
                <div style="display: flex; align-items: start; gap: 12px;">
                    <div style="font-size: 24px;">🐳</div>
                    <div style="flex: 1;">
                        <div style="font-weight: 600; margin-bottom: 6px;">Docker Setup Required</div>
                        <div style="font-size: 13px; color: #ccc; margin-bottom: 8px;">
                            To use Docker features, run this command in your terminal:
                        </div>
                        <code style="display: block; background: #1e1e1e; padding: 8px; border-radius: 4px; font-size: 12px; margin-bottom: 8px;">
                            sudo snap connect mumps-studio:docker-support && sudo usermod -aG docker $USER
                        </code>
                        <div style="font-size: 12px; color: #ffa500;">
                            ⚠️ Log out and log back in after running the command
                        </div>
                    </div>
                    <button class="btn ghost" onclick="document.getElementById('dockerSetupNotice').classList.add('hidden')" style="padding: 4px 8px; min-width: auto;">✕</button>
                </div>
            </div>

            <div class="connections-grid">
                <div class="connection-card">
                    <div class="panel-title">Docker</div>
                    <div class="pane-subtitle">Pick a running container</div>
                    <div id="dockerList" class="list docker-list"></div>
                    <div class="flex">
                        <button class="btn ghost" id="refreshDockerBtn">Refresh Docker</button>
                        <button class="btn ghost" id="useLocalDockerBtn">Use Universal</button>
                    </div>
                    <div class="pane-subtitle" style="margin-top: 12px;">Docker Configuration (optional)</div>
                    <div class="ssh-form">
                        <input class="ssh-input" id="dockerEnvKeyInput" placeholder="Environment key (e.g., h)">
                        <input class="ssh-input" id="dockerYdbPathInput" placeholder="YottaDB path (e.g., /opt/fis-gtm/YDB136)">
                        <input class="ssh-input" id="dockerGldPathInput" placeholder="Global directory path (optional)">
                        <input class="ssh-input" id="dockerRoutinesPathInput" placeholder="Routines path (optional)">
                    </div>
                    <div class="flex">
                        <button class="btn ghost" id="dockerSaveConfigBtn">Save Docker Config</button>
                    </div>
                    <div class="pill subtle" id="dockerConfigStatus">No config saved (universal mode)</div>
                </div>
                <div class="connection-card">
                    <div class="panel-title">SSH</div>
                    <div class="pane-subtitle">Connect to a remote host</div>
                    <div class="ssh-form">
                        <input class="ssh-input" id="sshHostInput" placeholder="Host (e.g., 10.0.0.5)">
                        <input class="ssh-input" id="sshPortInput" placeholder="Port (default 22)" type="number" min="1" max="65535">
                        <input class="ssh-input" id="sshEnvInput" placeholder="Environment key (e.g., cc)">
                        <input class="ssh-input" id="sshUserInput" placeholder="Username">
                        <input class="ssh-input" id="sshPassInput" placeholder="Password" type="password">
                        <button class="btn primary" id="sshConnectBtn">Connect SSH</button>
                    </div>
                    <div class="pill subtle" id="sshFormStatus">Not connected</div>
                    <div class="ssh-saved">
                        <div class="pane-subtitle">Saved environments</div>
                        <div id="sshSavedList" class="saved-env-list"></div>
                        <div class="flex">
                            <button class="btn ghost" id="sshSaveEnvBtn">Save environment</button>
                        </div>
                    </div>
                </div>
            </div>
        `,
        settingsPanel: `
            <div class="connections-header">
                <div>
                    <div class="pane-title">Settings</div>
                    <div class="pane-subtitle">Preferences</div>
                </div>
                <button class="btn ghost icon-btn" id="closeSettingsBtn" title="Close">✕</button>
            </div>
            <div class="connections-grid">
                <div class="connection-card">
                    <div class="pane-title">Appearance</div>
                    <div class="pane-subtitle">IDE & Code themes</div>
                    <div class="ssh-form">
                        <label class="pane-subtitle">IDE Theme</label>
                        <select class="ssh-input" id="settingsIdeTheme">
                            <option value="earth">Earth Dark</option>
                            <option value="desert">Desert Contrast</option>
                        </select>
                        <label class="pane-subtitle">Code Theme</label>
                        <select class="ssh-input" id="settingsCodeTheme">
                            <option value="mumps-earth">Earthy</option>
                            <option value="mumps-dark">Carbon</option>
                        </select>
                    </div>
                </div>
                <div class="connection-card">
                    <div class="pane-title">Git</div>
                    <div class="pane-subtitle">Configure author & remote</div>
                    <div class="ssh-form">
                        <input class="ssh-input" id="gitUserName" placeholder="User name (git config user.name)">
                        <input class="ssh-input" id="gitUserEmail" placeholder="Email (git config user.email)">
                        <input class="ssh-input" id="gitRemoteUrl" placeholder="Remote URL (origin)">
                        <button class="btn primary" id="gitSettingsSave">Save Git Settings</button>
                        <button class="btn ghost" id="gitSettingsTest">Test Git Status</button>
                    </div>
                    <div class="pane-subtitle" id="gitSettingsStatus">Not applied</div>
                </div>
                <div class="connection-card">
                    <div class="pane-title">Developer Tools</div>
                    <div class="pane-subtitle">Debug and inspect IDE</div>
                    <button class="btn primary" id="toggleDevTools">Toggle DevTools</button>
                </div>
                <div class="connection-card">
                    <div class="pane-title">Terminal</div>
                    <div class="pane-subtitle">Shell settings (stub)</div>
                </div>
                <div class="connection-card">
                    <div class="pane-title">SSH / Docker</div>
                    <div class="pane-subtitle">Configure remotes (stub)</div>
                </div>
            </div>
        `,
        newProjectPanel: `
            <div class="connections-header">
                <div>
                    <div class="pane-title">New Project</div>
                    <div class="pane-subtitle">Create web + routines structure</div>
                </div>
                <button class="btn ghost icon-btn" id="closeNewProjectBtn" title="Close">✕</button>
            </div>
            <div class="connections-grid">
                <div class="connection-card">
                    <div class="panel-title">Project Details</div>
                    <div class="pane-subtitle">Name and location</div>
                    <div class="ssh-form">
                        <label class="pane-subtitle">Project Name</label>
                        <input class="ssh-input" id="projectName" placeholder="my-project">

                        <label class="pane-subtitle">Project Path (parent directory)</label>
                        <input class="ssh-input" id="projectPath" placeholder="/home/ahmad/projects" value="/home/ahmad/projects">

                        <div class="pane-subtitle" style="margin-top:12px;">
                            <label style="display:flex; align-items:center; gap:6px; cursor:pointer;">
                                <input type="checkbox" id="fetchRoutines" checked style="cursor:pointer;">
                                Auto-fetch MUMPS routines from Docker (max 100)
                            </label>
                        </div>

                        <button class="btn primary" id="createProjectBtn" style="margin-top:16px;">Create Project</button>
                    </div>
                    <div class="pill subtle" id="projectCreationStatus">Ready</div>
                </div>

                <div class="connection-card">
                    <div class="panel-title">Structure Preview</div>
                    <div class="pane-subtitle">Will be created:</div>
                    <pre class="debug-output" id="structurePreview" style="font-size:12px; line-height:1.6;">my-project/
├── web/
│   └── (empty - add your PHP files here)
└── routines/
    └── (MUMPS .m files from Docker)</pre>
                </div>
            </div>
        `,
        findDialog: `
            <div class="search-dialog-header">
                <div>
                    <div class="pane-title" id="findDialogTitle">Find in Files</div>
                    <div class="pane-subtitle" id="findScopeLabel">Scope: Unknown</div>
                </div>
                <div class="dialog-actions">
                    <span class="pill subtle" id="findModePill">Find</span>
                    <button class="btn ghost icon-btn" id="closeFindDialog" title="Close">✕</button>
                </div>
            </div>
            <div class="search-dialog-body">
                <label class="field-label" for="findQueryInput">Text to find</label>
                <input class="search-text-input" id="findQueryInput" placeholder="Search term">
                <div class="replace-row" id="replaceRow">
                    <label class="field-label" for="replaceQueryInput">Replace with</label>
                    <input class="search-text-input" id="replaceQueryInput" placeholder="Replacement text">
                </div>
                <div class="search-options">
                    <label><input type="checkbox" id="findCaseOption"> Case sensitive</label>
                    <label><input type="checkbox" id="findWholeOption"> Whole words</label>
                    <label><input type="checkbox" id="findRegexOption"> Regex</label>
                    <span class="search-scope-pill" id="findScopePath">Scope: Unknown</span>
                </div>
                <div class="search-results-list" id="findResults">Type to search…</div>
                <div class="search-actions">
                    <button class="btn ghost" id="findReplaceToggleBtn">Switch to Replace</button>
                    <button class="btn primary" id="replaceAllBtn">Replace All</button>
                </div>
            </div>
        `,
        searchEverywhereDialog: `
            <input class="search-everywhere-input" id="searchEverywhereInput" placeholder="Search Everywhere (files)">
            <div class="search-everywhere-results" id="searchEverywhereResults">Press Shift twice to search…</div>
        `
    };

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.app = window.AhmadIDEModules.app || {};
        window.AhmadIDEModules.app.panelTemplates = panelTemplates;
    }
})();
