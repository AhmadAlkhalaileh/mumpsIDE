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
            <div class="git-toolwindow" id="gitToolWindow">
                <div class="git-tw-header">
                    <div class="git-tw-title">Git</div>

                    <div class="git-tw-tabs" role="tablist" aria-label="Git tool window">
                        <button class="git-tw-tab active" type="button" data-git-tab="log" role="tab"
                            aria-selected="true">Log</button>
                        <button class="git-tw-tab" type="button" data-git-tab="changes" role="tab"
                            aria-selected="false">Commit</button>
                    </div>

                </div>

                <div class="git-banner hidden" id="gitBanner" role="status" aria-live="polite">
                    <div class="git-banner-text" id="gitBannerText"></div>
                    <div class="git-banner-actions" id="gitBannerActions"></div>
                </div>

                <div class="git-tw-toolbar" role="toolbar" aria-label="Git toolbar">
                    <div class="git-tw-toolbar-group">
                        <button class="git-tw-iconbtn" type="button" id="gitFetchBtn" title="Fetch">
                            <span data-ui-icon="download" data-ui-icon-size="16" aria-hidden="true"></span>
                        </button>
                        <button class="git-tw-iconbtn" type="button" id="gitPullBtn" title="Pull">
                            <span data-ui-icon="download" data-ui-icon-size="16" aria-hidden="true"></span>
                        </button>
                        <button class="git-tw-iconbtn" type="button" id="gitPushBtn" title="Push">
                            <span data-ui-icon="upload" data-ui-icon-size="16" aria-hidden="true"></span>
                        </button>

                        <div class="git-tw-sep" role="separator"></div>

                        <button class="git-tw-iconbtn" type="button" id="gitRefreshBtn" title="Refresh">
                            <span data-ui-icon="refresh" data-ui-icon-size="16" aria-hidden="true"></span>
                        </button>
                    </div>

                    <div class="git-tw-sep" role="separator"></div>

                    <button class="git-tw-iconbtn" type="button" id="gitFocusSearchBtn" title="Search">
                        <span data-ui-icon="search" data-ui-icon-size="16" aria-hidden="true"></span>
                    </button>
                    <button class="git-tw-iconbtn" type="button" id="gitFilterBtn" title="Filter">
                        <span data-ui-icon="filter" data-ui-icon-size="16" aria-hidden="true"></span>
                    </button>

                    <div class="git-spacer"></div>

                    <button class="git-tw-iconbtn" type="button" id="gitSettingsBtn" title="Settings">
                        <span data-ui-icon="settings" data-ui-icon-size="16" aria-hidden="true"></span>
                    </button>
                    <button class="git-tw-iconbtn" type="button" id="gitOverflowBtn" title="More">⋯</button>
                </div>

                <div class="git-tw-panes">
                    <section class="git-tw-pane active" data-git-pane="log" role="tabpanel">
                        <div class="git-log-layout git-log-layout--3pane">
                            <div class="git-pane git-pane--branches">
                                <div class="git-pane-header">
                                    <div class="git-pane-title">Branches</div>
                                    <div class="git-pane-actions">
                                        <button class="git-tw-iconbtn" type="button" id="gitBranchesRefreshBtn" title="Refresh">
                                            <span data-ui-icon="refresh" data-ui-icon-size="16" aria-hidden="true"></span>
                                        </button>
                                    </div>
                                </div>
                                <div class="git-branches-tree" id="gitBranchesTree">Branches not loaded.</div>
                            </div>

                            <div class="git-pane git-pane--commits">
                                <div class="git-log-filter-row" role="toolbar" aria-label="Commit filters">
                                    <div class="git-log-filter-search" title="Search commits">
                                        <span data-ui-icon="search" data-ui-icon-size="16" aria-hidden="true"></span>
                                        <input id="gitLogSearchInput" class="git-input" placeholder="Text or hash">
                                    </div>
                                    <div class="git-log-filter-field" title="Branch scope">
                                        <span class="git-log-filter-label">Branch:</span>
                                        <select id="gitLogBranchSelect" class="git-select"></select>
                                    </div>

                                    <select id="gitLogUserSelect" class="git-select" disabled title="Not implemented yet">
                                        <option>User</option>
                                    </select>
                                    <select id="gitLogDateSelect" class="git-select" disabled title="Not implemented yet">
                                        <option>Date</option>
                                    </select>
                                    <select id="gitLogPathsSelect" class="git-select" disabled title="Not implemented yet">
                                        <option>Paths</option>
                                    </select>

                                    <button class="btn ghost git-small-btn" type="button" id="gitLogResetBtn">Reset</button>
                                    <button class="git-tw-iconbtn" type="button" id="gitLogBtn" title="Reload Log">
                                        <span data-ui-icon="refresh" data-ui-icon-size="16" aria-hidden="true"></span>
                                    </button>
                                </div>

                                <div class="git-log-list" id="gitHistoryList">History not loaded.</div>
                            </div>

                            <div class="git-pane git-pane--details">
                                <div class="git-pane-header">
                                    <div>
                                        <div class="git-pane-title">Commit details</div>
                                        <div class="git-pane-subtitle" id="gitLogDetailsSubtitle"></div>
                                    </div>
                                </div>

                                <div class="git-log-empty" id="gitLogEmptyState">
                                    Select commit to view changes
                                </div>

                                <div class="git-log-details-body hidden" id="gitLogDetailsBody">
                                    <div class="git-commit-header">
                                        <div class="git-commit-subject" id="gitLogSubject"></div>
                                        <div class="git-commit-meta">
                                            <span class="git-commit-hash" id="gitLogHash"></span>
                                            <span class="git-commit-meta-sep">·</span>
                                            <span class="git-commit-author" id="gitLogAuthor"></span>
                                            <span class="git-commit-meta-sep">·</span>
                                            <span class="git-commit-date" id="gitLogDate"></span>
                                            <span class="git-spacer"></span>
                                            <button class="git-tw-iconbtn git-commit-copy" type="button"
                                                id="gitCopyCommitHashBtn" title="Copy commit hash">
                                                <span data-ui-icon="copy" data-ui-icon-size="16" aria-hidden="true"></span>
                                            </button>
                                        </div>
                                    </div>

                                    <div class="git-log-files">
                                        <div class="git-section-header">
                                            <div class="git-group-header">Files</div>
                                            <div class="git-section-actions">
                                                <span class="git-section-meta" id="gitLogFilesCount"></span>
                                            </div>
                                        </div>
                                        <div class="git-log-files-list" id="gitLogFiles"></div>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </section>

                    <section class="git-tw-pane" data-git-pane="changes" role="tabpanel">
                        <div class="git-changes-layout git-changes-layout--single">
                            <div class="git-pane">
                                <div class="git-commit-view">
                                    <div class="git-commit-changes">
                                        <div class="git-section-row">
                                            <div class="git-section-title">Changes</div>
                                            <div class="git-section-actions">
                                                <button class="git-tw-iconbtn" type="button" id="gitStageSelectedBtn"
                                                    title="Stage Selected">
                                                    <span data-ui-icon="download" data-ui-icon-size="16" aria-hidden="true"></span>
                                                </button>
                                                <button class="git-tw-iconbtn" type="button" id="gitUnstageSelectedBtn"
                                                    title="Unstage Selected">
                                                    <span data-ui-icon="upload" data-ui-icon-size="16" aria-hidden="true"></span>
                                                </button>
                                                <button class="git-tw-iconbtn" type="button" id="gitDiffSelectedBtn"
                                                    title="Open Diff in Editor">
                                                    <span data-ui-icon="format" data-ui-icon-size="16" aria-hidden="true"></span>
                                                </button>
                                            </div>
                                        </div>

                                        <div class="git-changes-groups">
                                            <div class="git-group">
                                                <div class="git-group-header">Unstaged</div>
                                                <div class="git-changes-list" id="gitChangesUnstaged">Run refresh to load
                                                    changes.</div>
                                            </div>
                                            <div class="git-group">
                                                <div class="git-group-header">Staged</div>
                                                <div class="git-changes-list" id="gitChangesStaged">No staged files.</div>
                                            </div>
                                        </div>
                                    </div>

                                    <div class="git-commit-message">
                                        <div class="git-commit-options">
                                            <label class="git-checkbox">
                                                <input type="checkbox" id="gitAmendCheckbox">
                                                Amend
                                            </label>
                                            <div class="git-spacer"></div>
                                            <button class="btn ghost git-small-btn" type="button" id="gitCommitOptionsBtn"
                                                title="Commit Options" disabled>⋯</button>
                                        </div>
                                        <textarea id="gitCommitMessage" class="git-commit-input"
                                            placeholder="Commit message"></textarea>
                                        <div class="git-commit-actions">
                                            <button class="btn primary git-primary git-small-btn" type="button"
                                                id="gitCommitBtn">Commit</button>
                                            <button class="btn ghost git-small-btn" type="button"
                                                id="gitCommitAndPushBtn">Commit and Push…</button>
                                        </div>
                                        <details class="git-branch-details">
                                            <summary class="git-branch-summary">Branch</summary>
                                            <div class="git-branch-row">
                                                <select id="gitBranchSelect" class="git-select"></select>
                                                <input id="gitBranchInput" class="git-input" placeholder="New branch name">
                                                <button class="btn ghost git-small-btn" type="button"
                                                    id="gitCheckoutBtn">Checkout</button>
                                            </div>
                                        </details>
                                    </div>
                                </div>
                            </div>

                            <div class="hidden">
                                <button class="btn ghost git-small-btn" type="button" id="gitStatusBtn">Status</button>
                                <button class="btn ghost git-small-btn" type="button" id="gitClearBtn">Clear</button>
                                <input class="git-input" id="gitDiffPath" placeholder="Path (relative)">
                                <button class="btn ghost git-small-btn" type="button" id="gitDiffFileBtn">Show Diff</button>
                                <button class="btn ghost git-small-btn" type="button" id="gitHistoryFileBtn">File History</button>
                                <input class="git-input" id="gitComparePathA" placeholder="Path A">
                                <input class="git-input" id="gitComparePathB" placeholder="Path B">
                                <button class="btn ghost git-small-btn" type="button" id="gitCompareBtn">Compare</button>
                                <button class="btn ghost git-small-btn" type="button" id="gitDiffBtn">Diff (stat)</button>
                                <pre class="debug-output git-console-output" id="gitOutput">Git ready.</pre>
                            </div>
                        </div>
                    </section>
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
            <div class="services-toolwindow" id="servicesToolWindow">
                <div class="services-split">
                    <div class="services-left">
                        <div class="services-titlebar">Services</div>
                        <div class="services-divider"></div>
                        <div class="services-toolbar" role="toolbar" aria-label="Services toolbar">
                            <button class="services-toolbar-btn" id="servicesAddBtn" type="button" title="Add">
                                <svg viewBox="0 0 16 16" fill="none">
                                    <path d="M8 3.2v9.6M3.2 8h9.6" stroke="currentColor" stroke-width="1.6"
                                        stroke-linecap="round" />
                                    <circle cx="12.3" cy="12.3" r="0.9" fill="currentColor" opacity="0.7" />
                                </svg>
                            </button>
                            <button class="services-toolbar-btn" id="servicesViewBtn" type="button" title="View options">
                                <svg viewBox="0 0 16 16" fill="none">
                                    <path d="M1.8 8s2.2-4 6.2-4 6.2 4 6.2 4-2.2 4-6.2 4-6.2-4-6.2-4z"
                                        stroke="currentColor" stroke-width="1.3" stroke-linejoin="round" />
                                    <circle cx="8" cy="8" r="1.8" stroke="currentColor" stroke-width="1.3" />
                                </svg>
                            </button>
                            <button class="services-toolbar-btn" id="servicesNewBtn" type="button" title="New service">
                                <svg viewBox="0 0 16 16" fill="none">
                                    <rect x="3" y="3" width="10" height="10" rx="1.8" stroke="currentColor"
                                        stroke-width="1.3" />
                                    <path d="M8 5.3v5.4M5.3 8h5.4" stroke="currentColor" stroke-width="1.4"
                                        stroke-linecap="round" />
                                </svg>
                            </button>
                            <div class="services-toolbar-sep" role="separator"></div>
                            <button class="services-toolbar-btn" id="servicesExpandAllBtn" type="button"
                                title="Expand all">
                                <svg viewBox="0 0 16 16" fill="none">
                                    <path d="M5 6l3 3 3-3" stroke="currentColor" stroke-width="1.4"
                                        stroke-linecap="round" stroke-linejoin="round" />
                                    <path d="M5 10l3 3 3-3" stroke="currentColor" stroke-width="1.4"
                                        stroke-linecap="round" stroke-linejoin="round" />
                                </svg>
                            </button>
                            <button class="services-toolbar-btn" id="servicesCollapseAllBtn" type="button"
                                title="Collapse all">
                                <svg viewBox="0 0 16 16" fill="none">
                                    <path d="M5 10l3-3 3 3" stroke="currentColor" stroke-width="1.4"
                                        stroke-linecap="round" stroke-linejoin="round" />
                                    <path d="M5 6l3-3 3 3" stroke="currentColor" stroke-width="1.4"
                                        stroke-linecap="round" stroke-linejoin="round" />
                                </svg>
                            </button>
                        </div>
                        <div class="services-tree" id="servicesTree" role="tree" aria-label="Services tree"></div>
                    </div>
                    <div class="services-right">
                        <div class="services-detail" id="servicesDetail">
                            <div class="services-empty" id="servicesEmpty">Select service to view details</div>
                            <div class="services-detail-content hidden" id="servicesDetailContent"></div>
                        </div>
                    </div>
                </div>
            </div>
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
