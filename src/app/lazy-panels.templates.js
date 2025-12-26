(() => {
    const panelTemplates = {
        terminalPanel: `
            <div class="pane-header terminal-header">
                <div class="terminal-header-left">
                    <span class="pane-title">Run</span>
                    <button class="terminal-icon-btn" id="terminalNewTabBtn" title="New Run Tab (Ctrl+Shift+T)">
                        <svg width="12" height="12" viewBox="0 0 16 16" fill="none"><path d="M8 3v10M3 8h10" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/></svg>
                    </button>
                    <button class="terminal-icon-btn" id="terminalClearBtn" title="Clear Output" type="button">
                        <svg width="12" height="12" viewBox="0 0 16 16" fill="none">
                            <path d="M3.5 4.5h9" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/>
                            <path d="M6.5 4.5v-1h3v1" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round"/>
                            <path d="M5.25 6v6.75h5.5V6" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round"/>
                        </svg>
                    </button>
                    <button class="terminal-icon-btn" id="terminalDropdownBtn" title="Terminal Options">
                        <svg width="10" height="10" viewBox="0 0 16 16" fill="none"><path d="M4 6l4 4 4-4" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/></svg>
                    </button>
                </div>
                <div id="terminalTabs" class="terminal-tabs"></div>
                <div class="terminal-header-right">
                    <button class="terminal-icon-btn" id="terminalMenuBtn" title="More">
                        <svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor"><circle cx="8" cy="3" r="1.2"/><circle cx="8" cy="8" r="1.2"/><circle cx="8" cy="13" r="1.2"/></svg>
                    </button>
                    <button class="terminal-icon-btn" id="terminalHideBtn" title="Hide">
                        <svg width="12" height="12" viewBox="0 0 16 16" fill="none"><path d="M4 8h8" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/></svg>
                    </button>
                </div>
            </div>
            <div class="terminal-surface">
                <div class="terminal-viewport" id="terminalViewport" aria-label="Terminal"></div>
                <div class="terminal-error hidden" id="terminalError" role="alert"></div>
            </div>
        `,
        terminalToolPanel: `
            <div class="pane-header terminal-header">
                <div class="terminal-header-left">
                    <span class="pane-title">Terminal</span>
                    <button class="terminal-icon-btn" id="terminalToolNewTabBtn" title="New Terminal Tab (Ctrl+Shift+T)" type="button">
                        <svg width="12" height="12" viewBox="0 0 16 16" fill="none"><path d="M8 3v10M3 8h10" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/></svg>
                    </button>
                    <button class="terminal-icon-btn" id="terminalToolSplitBtn" title="Split Right" type="button">
                        <svg width="12" height="12" viewBox="0 0 16 16" fill="none">
                            <path d="M2.75 3.25h10.5v9.5H2.75z" stroke="currentColor" stroke-width="1.2"/>
                            <path d="M8 3.25v9.5" stroke="currentColor" stroke-width="1.2"/>
                        </svg>
                    </button>
                    <button class="terminal-icon-btn" id="terminalToolSplitDownBtn" title="Split Down" type="button">
                        <svg width="12" height="12" viewBox="0 0 16 16" fill="none">
                            <path d="M2.75 3.25h10.5v9.5H2.75z" stroke="currentColor" stroke-width="1.2"/>
                            <path d="M2.75 8h10.5" stroke="currentColor" stroke-width="1.2"/>
                        </svg>
                    </button>
                    <button class="terminal-icon-btn" id="terminalToolOptionsBtn" title="Terminal Options" type="button">
                        <svg width="10" height="10" viewBox="0 0 16 16" fill="none"><path d="M4 6l4 4 4-4" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/></svg>
                    </button>
                </div>
                <div id="terminalToolTabs" class="terminal-tabs"></div>
                <div class="terminal-header-right">
                    <button class="terminal-icon-btn" id="terminalToolMenuBtn" title="More" type="button">
                        <svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor"><circle cx="8" cy="3" r="1.2"/><circle cx="8" cy="8" r="1.2"/><circle cx="8" cy="13" r="1.2"/></svg>
                    </button>
                    <button class="terminal-icon-btn" id="terminalToolHideBtn" title="Hide" type="button">
                        <svg width="12" height="12" viewBox="0 0 16 16" fill="none"><path d="M4 8h8" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/></svg>
                    </button>
                </div>
            </div>
            <div class="terminal-surface terminal-tool-surface">
                <div class="terminal-viewport terminal-tool-viewport" id="terminalToolViewport" aria-label="Terminal"></div>
                <div class="terminal-error hidden" id="terminalToolError" role="alert"></div>
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
        commitPanel: `
            <div class="ps-commit-window">
                <div class="ps-commit-header">
                    <span class="ps-commit-title">Commit</span>
                    <button class="ps-commit-pill" id="commitHeaderBtn" disabled>Commit</button>
                    <div class="ps-spacer"></div>
                    <button class="ps-icon-btn ps-commit-header-btn" id="commitHeaderMenuBtn" title="Menu">
                        <span data-ui-icon="dots-vertical" data-ui-icon-size="16" aria-hidden="true"></span>
                    </button>
                    <button class="ps-icon-btn ps-commit-header-btn" id="commitHeaderMinimizeBtn" title="Minimize">
                        <span data-ui-icon="minus" data-ui-icon-size="16" aria-hidden="true"></span>
                    </button>
                </div>
                <div class="ps-commit-toolbar" role="toolbar" aria-label="Commit toolbar">
                    <button class="ps-icon-btn" id="commitRefreshBtn" title="Refresh"><span data-ui-icon="refresh" data-ui-icon-size="16" aria-hidden="true"></span></button>
                    <button class="ps-icon-btn" id="commitRollbackBtn" title="Undo"><span data-ui-icon="undo" data-ui-icon-size="16" aria-hidden="true"></span></button>
                    <button class="ps-icon-btn" id="commitDownloadBtn" title="Download Patch"><span data-ui-icon="download" data-ui-icon-size="16" aria-hidden="true"></span></button>
                    <button class="ps-icon-btn" id="commitDiffBtn" title="Show Diff"><span data-ui-icon="eye" data-ui-icon-size="16" aria-hidden="true"></span></button>
                    <button class="ps-icon-btn" id="commitExpandBtn" title="Expand"><span data-ui-icon="chevron-up" data-ui-icon-size="16" aria-hidden="true"></span></button>
                    <button class="ps-icon-btn" id="commitCollapseBtn" title="Collapse"><span data-ui-icon="chevron-down" data-ui-icon-size="16" aria-hidden="true"></span></button>
                    <button class="ps-icon-btn" id="commitCloseBtn" title="Close"><span data-ui-icon="close" data-ui-icon-size="16" aria-hidden="true"></span></button>
                </div>
                <div class="ps-changes-section">
                    <div class="ps-section-header" id="commitChangesToggle">
                        <span class="ps-chevron" data-ui-icon="chevron-down" data-ui-icon-size="12" aria-hidden="true"></span>
                        <span class="ps-section-title">Changes</span>
                        <span class="ps-count" id="commitChangesCount">0 files</span>
                    </div>
                    <div class="ps-file-list" id="commitFileList"></div>
                </div>
                <div class="ps-commit-message-section">
                    <div class="ps-commit-message-toolbar">
                        <label class="ps-checkbox"><input type="checkbox" id="commitAmend"> Amend</label>
                        <button class="ps-icon-btn-sm" id="commitAmendOptionsBtn" title="Amend Options">
                            <span data-ui-icon="square-arrow-right" data-ui-icon-size="14" aria-hidden="true"></span>
                        </button>
                        <button class="ps-icon-btn-sm" id="commitTimeBtn" title="Commit Time">
                            <span data-ui-icon="clock" data-ui-icon-size="14" aria-hidden="true"></span>
                        </button>
                        <button class="ps-icon-btn-sm ps-icon-warm" id="commitCleanupBtn" title="Cleanup">
                            <span data-ui-icon="flame" data-ui-icon-size="14" aria-hidden="true"></span>
                        </button>
                    </div>
                    <textarea id="commitMessageInput" class="ps-textarea"></textarea>
                    <div class="ps-commit-footer">
                        <div class="ps-commit-footer-actions">
                            <button class="ps-btn-commit ps-btn-primary" id="commitBtnMain" disabled>Commit</button>
                            <button class="ps-btn-commit-push" id="commitAndPushBtn" disabled>Commit and Push...</button>
                        </div>
                        <button class="ps-icon-btn" id="commitSettingsBtn" title="Commit Settings">
                            <span data-ui-icon="settings" data-ui-icon-size="16" aria-hidden="true"></span>
                        </button>
                    </div>
                </div>
            </div>
        `,
        gitToolPanel: `
            <div class="ps-git-window">
                <div class="ps-git-toolbar">
                    <input type="text" class="ps-search" placeholder="Text or hash" id="gitSearchInput">
                    <button class="ps-icon-btn" id="gitOpenCommitPanelBtn" type="button" title="Commit"><span data-ui-icon="commit" data-ui-icon-size="16" aria-hidden="true"></span></button>
                    <span class="ps-separator"></span>
                    <select class="ps-select-sm" id="gitLogBranchSelect"><option value="__ALL__">origin</option></select>
                    <button class="ps-icon-btn" id="gitFilterAuthorBtn" type="button" title="User"><span data-ui-icon="user" data-ui-icon-size="16" aria-hidden="true"></span></button>
                    <button class="ps-icon-btn" id="gitFilterDateBtn" type="button" title="Date"><span data-ui-icon="calendar" data-ui-icon-size="16" aria-hidden="true"></span></button>
                    <button class="ps-icon-btn" id="gitFilterPathBtn" type="button" title="Paths"><span data-ui-icon="folder" data-ui-icon-size="16" aria-hidden="true"></span></button>
                    <span class="ps-separator"></span>
                    <button class="ps-icon-btn" id="gitShowGraphBtn" type="button" title="Show Graph"><span data-ui-icon="graph" data-ui-icon-size="16" aria-hidden="true"></span></button>
                    <button class="ps-icon-btn" id="gitLogRefreshBtn" type="button" title="Refresh"><span data-ui-icon="refresh" data-ui-icon-size="16" aria-hidden="true"></span></button>
                    <button class="ps-icon-btn" id="gitOpenGitSettingsBtn" type="button" title="Settings"><span data-ui-icon="settings" data-ui-icon-size="16" aria-hidden="true"></span></button>
                </div>
                <div class="ps-git-3pane">
                    <div class="ps-git-branches" id="gitBranchesTree">
                        <div class="ps-tree-section">
                            <div class="ps-tree-header"><span data-ui-icon="chevron-right" data-ui-icon-size="12" aria-hidden="true"></span> HEAD (Current Branch)</div>
                            <div class="ps-tree-empty">Loading...</div>
                        </div>
                        <div class="ps-tree-section">
                            <div class="ps-tree-header"><span data-ui-icon="chevron-right" data-ui-icon-size="12" aria-hidden="true"></span> Local</div>
                        </div>
                        <div class="ps-tree-section">
                            <div class="ps-tree-header"><span data-ui-icon="chevron-right" data-ui-icon-size="12" aria-hidden="true"></span> Remote</div>
                        </div>
                    </div>
                    <div class="ps-git-commits" id="gitHistoryList"></div>
                    <div class="ps-git-details">
                        <div class="ps-details-empty" id="gitLogEmptyState">Select commit to view changes</div>
                        <div class="ps-details-body hidden" id="gitLogDetailsBody">
                            <div class="ps-commit-info">
                                <div class="ps-commit-subject" id="gitLogSubject"></div>
                                <div class="ps-commit-meta">
                                    <span id="gitLogHash"></span> · <span id="gitLogAuthor"></span> · <span id="gitLogDate"></span>
                                </div>
                            </div>
                            <div class="ps-changed-files" id="gitLogFiles"></div>
                            <div class="ps-diff-view" id="gitLogDiffGrid"></div>
                        </div>
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
        comparePanel: `
            <div class="ps-compare-window" id="releaseComparePanel">
                <div class="ps-compare-empty">
                    <svg width="64" height="64" viewBox="0 0 24 24" fill="none" stroke="currentColor" opacity="0.3">
                        <path d="M8 3H5a2 2 0 0 0-2 2v3m18 0V5a2 2 0 0 0-2-2h-3m0 18h3a2 2 0 0 0 2-2v-3M3 16v3a2 2 0 0 0 2 2h3" stroke-width="2"/>
                        <path d="M12 8v8m-4-4h8" stroke-width="2"/>
                    </svg>
                    <p>Right-click on a routine file and select "Compare with Release"</p>
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
	                <div class="connection-card hidden" id="releaseConnectionCard">
	                    <div class="panel-title">Release Connection</div>
	                    <div class="pane-subtitle">Compare routines via SSH (Compare with Release)</div>
	                    <div class="ssh-form">
	                        <input class="ssh-input" id="releaseConnHostInput" placeholder="Host (e.g., 10.0.0.5)">
	                        <input class="ssh-input" id="releaseConnPortInput" placeholder="Port (default 22)" type="number" min="1" max="65535">
	                        <input class="ssh-input" id="releaseConnUserInput" placeholder="Username">
	                        <input class="ssh-input" id="releaseConnPassInput" placeholder="Password (stored securely)" type="password" autocomplete="new-password">
	                        <div class="flex">
	                            <button class="btn ghost" id="releaseConnTestBtn" type="button">Test</button>
	                            <button class="btn ghost" id="releaseConnSaveBtn" type="button">Save</button>
	                            <button class="btn ghost" id="releaseConnDeleteBtn" type="button">Delete</button>
	                            <button class="btn primary" id="releaseConnFocusCompareBtn" type="button">Compare…</button>
	                        </div>
	                    </div>
	                    <div class="pill subtle" id="releaseConnStatus">Not configured</div>
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
        `,
        patchTrackingPanel: `
            <div style="display: flex; flex-direction: column; height: 100%; background: var(--panel-2, #282a36); color: var(--text, #f8f8f2);">
                <!-- Compact Header with Inline Stats -->
                <div style="flex-shrink: 0; padding: 12px 16px; border-bottom: 1px solid var(--border, #44475a); background: var(--panel-strong, #21222c); display: flex; align-items: center; justify-content: space-between;">
                    <div style="display: flex; align-items: center; gap: 12px;">
                        <div style="display: flex; align-items: center; gap: 8px;">
                            <svg width="18" height="18" fill="none" stroke="#8be9fd" stroke-width="2">
                                <rect x="3" y="3" width="6" height="6" rx="1"/>
                                <rect x="11" y="3" width="6" height="6" rx="1"/>
                                <rect x="3" y="11" width="6" height="6" rx="1"/>
                                <rect x="11" y="11" width="6" height="6" rx="1"/>
                            </svg>
                            <span style="font-weight: 600; font-size: 13px; color: var(--text-bright, #ffffff);">Vista Patch Tracking</span>
                        </div>
                        <div style="display: flex; gap: 12px; font-size: 11px; color: var(--text-muted, #6272a4);">
                            <span><strong id="statPatches">0</strong> patches</span>
                            <span>•</span>
                            <span><strong id="statPending" style="color: #ffb86c;">0</strong> pending</span>
                            <span>•</span>
                            <span><strong id="statCommitted" style="color: #50fa7b;">0</strong> committed</span>
                        </div>
                    </div>
                    <div style="display: flex; gap: 6px; position: relative;">
                        <button id="patchTrackingRefreshBtn" title="Refresh stats" style="padding: 4px 8px; background: transparent; border: 1px solid transparent; border-radius: 4px; cursor: pointer; color: var(--text-muted, #6272a4); font-size: 11px;">
                            <svg width="14" height="14" fill="none" stroke="currentColor" stroke-width="2">
                                <path d="M3 8a5 5 0 0 1 5-5 5 5 0 0 1 5 5M13 8a5 5 0 0 1-5 5 5 5 0 0 1-5-5" stroke-linecap="round"/>
                                <path d="M8 3V1M8 15v-2M3 8H1M15 8h-2" stroke-linecap="round"/>
                            </svg>
                        </button>
                        <button id="patchTrackingMenuBtn" title="Menu" style="padding: 4px 8px; background: transparent; border: 1px solid transparent; border-radius: 4px; cursor: pointer; color: var(--text-muted, #6272a4); font-size: 11px;">
                            <svg width="14" height="14" fill="currentColor">
                                <circle cx="7" cy="3" r="1.5"/>
                                <circle cx="7" cy="7" r="1.5"/>
                                <circle cx="7" cy="11" r="1.5"/>
                            </svg>
                        </button>
                        <!-- Dropdown Menu -->
                        <div id="patchTrackingDropdown" style="display: none; position: absolute; top: 100%; right: 0; margin-top: 4px; min-width: 280px; background: var(--panel-strong, #21222c); border: 1px solid var(--border, #44475a); border-radius: 6px; box-shadow: 0 8px 24px rgba(0, 0, 0, 0.4); z-index: 1000;">
                            <!-- Dropdown content will be inserted here -->
                        </div>
                    </div>
                </div>

                <!-- Workflow Progress Bar -->
                <div style="flex-shrink: 0; display: flex; background: var(--panel-2, #282a36); border-bottom: 1px solid var(--border, #44475a);">
                    <div class="workflow-step" data-step="1" style="flex: 1; padding: 10px; text-align: center; cursor: pointer; border-right: 1px solid var(--border, #44475a); transition: background 0.15s;">
                        <div style="font-size: 11px; font-weight: 600; color: var(--text-muted, #6272a4);">UPLOAD</div>
                        <div class="workflow-step-indicator" style="width: 100%; height: 3px; background: rgba(139, 233, 253, 0.2); margin-top: 6px; border-radius: 2px;"></div>
                    </div>
                    <div class="workflow-step" data-step="2" style="flex: 1; padding: 10px; text-align: center; cursor: pointer; border-right: 1px solid var(--border, #44475a); transition: background 0.15s;">
                        <div style="font-size: 11px; font-weight: 600; color: var(--text-muted, #6272a4);">SCAN</div>
                        <div class="workflow-step-indicator" style="width: 100%; height: 3px; background: rgba(139, 233, 253, 0.2); margin-top: 6px; border-radius: 2px;"></div>
                    </div>
                    <div class="workflow-step" data-step="3" style="flex: 1; padding: 10px; text-align: center; cursor: pointer; border-right: 1px solid var(--border, #44475a); transition: background 0.15s;">
                        <div style="font-size: 11px; font-weight: 600; color: var(--text-muted, #6272a4);">CORRELATE</div>
                        <div class="workflow-step-indicator" style="width: 100%; height: 3px; background: rgba(139, 233, 253, 0.2); margin-top: 6px; border-radius: 2px;"></div>
                    </div>
                    <div class="workflow-step" data-step="4" style="flex: 1; padding: 10px; text-align: center; cursor: pointer; transition: background 0.15s;">
                        <div style="font-size: 11px; font-weight: 600; color: var(--text-muted, #6272a4);">COMMIT</div>
                        <div class="workflow-step-indicator" style="width: 100%; height: 3px; background: rgba(139, 233, 253, 0.2); margin-top: 6px; border-radius: 2px;"></div>
                    </div>
                </div>

                <!-- Main Content Area (Scrollable) -->
                <div id="patchTrackingContent" style="flex: 1; overflow-y: auto; padding: 16px;">
                    <!-- Initial Welcome State -->
                    <div style="display: flex; flex-direction: column; align-items: center; justify-content: center; min-height: 100%; padding: 40px 20px; text-align: center; color: var(--text-muted, #6272a4);">
                        <svg width="64" height="64" fill="none" stroke="currentColor" stroke-width="1.5" style="opacity: 0.25; margin-bottom: 16px;">
                            <rect x="8" y="8" width="48" height="48" rx="4"/>
                            <path d="M20 28h24M20 36h16" stroke-linecap="round"/>
                            <circle cx="16" cy="20" r="2" fill="currentColor"/>
                            <circle cx="24" cy="20" r="2" fill="currentColor"/>
                        </svg>
                        <div style="font-size: 14px; font-weight: 500; color: var(--text, #f8f8f2); margin-bottom: 6px;">Drop .KIDS file to begin</div>
                        <div style="font-size: 12px;">or click on workflow steps above</div>
                    </div>
                </div>

                <style>
                    .workflow-step:hover {
                        background: rgba(139, 233, 253, 0.05);
                    }
                    .workflow-step.active {
                        background: rgba(139, 233, 253, 0.08);
                    }
                    .workflow-step.active .workflow-step-indicator {
                        background: #8be9fd !important;
                        box-shadow: 0 0 8px rgba(139, 233, 253, 0.5);
                    }
                    .workflow-step.completed .workflow-step-indicator {
                        background: #50fa7b !important;
                    }

                    .patch-form {
                        max-width: 640px;
                        margin: 0 auto;
                    }
                    .patch-form-group {
                        margin-bottom: 16px;
                    }
                    .patch-form-label {
                        display: block;
                        font-size: 12px;
                        font-weight: 600;
                        color: var(--text-bright, #ffffff);
                        margin-bottom: 6px;
                    }
                    .patch-form-input {
                        width: 100%;
                        padding: 8px 12px;
                        background: rgba(0, 0, 0, 0.2);
                        border: 1px solid var(--border, #44475a);
                        border-radius: 4px;
                        color: var(--text, #f8f8f2);
                        font-size: 13px;
                        outline: none;
                    }
                    .patch-form-input:focus {
                        border-color: #8be9fd;
                        box-shadow: 0 0 0 2px rgba(139, 233, 253, 0.15);
                    }
                    .patch-btn {
                        padding: 8px 16px;
                        border-radius: 4px;
                        font-size: 12px;
                        font-weight: 600;
                        cursor: pointer;
                        border: 1px solid;
                        transition: all 0.15s;
                        outline: none;
                    }
                    .patch-btn:disabled {
                        opacity: 0.5;
                        cursor: not-allowed;
                    }
                    .patch-btn-primary {
                        background: rgba(139, 233, 253, 0.15);
                        border-color: #8be9fd;
                        color: #8be9fd;
                    }
                    .patch-btn-primary:hover:not(:disabled) {
                        background: rgba(139, 233, 253, 0.25);
                    }
                    .patch-btn-secondary {
                        background: rgba(255, 255, 255, 0.05);
                        border-color: var(--border, #44475a);
                        color: var(--text, #f8f8f2);
                    }
                    .patch-btn-secondary:hover:not(:disabled) {
                        background: rgba(255, 255, 255, 0.08);
                    }
                    .patch-actions {
                        display: flex;
                        gap: 8px;
                        justify-content: flex-end;
                        margin-top: 20px;
                    }
                    .patch-result-item {
                        margin-top: 16px;
                        padding: 12px;
                        border-radius: 6px;
                        border: 1px solid;
                        font-size: 13px;
                    }
                    .patch-result-item.success {
                        background: rgba(80, 250, 123, 0.08);
                        border-color: rgba(80, 250, 123, 0.3);
                    }
                    .patch-result-item.warning {
                        background: rgba(255, 184, 108, 0.08);
                        border-color: rgba(255, 184, 108, 0.3);
                    }
                    .patch-result-item.error {
                        background: rgba(255, 85, 85, 0.08);
                        border-color: rgba(255, 85, 85, 0.3);
                    }
                    .patch-result-header {
                        display: flex;
                        justify-content: space-between;
                        align-items: center;
                        margin-bottom: 8px;
                    }
                    .patch-result-title {
                        font-weight: 600;
                    }
                    .patch-result-badge {
                        font-size: 10px;
                        font-weight: 700;
                        padding: 3px 8px;
                        border-radius: 3px;
                        background: rgba(0, 0, 0, 0.2);
                    }
                    .patch-result-body {
                        font-size: 12px;
                        line-height: 1.6;
                    }

                    /* Drag and drop styles */
                    #patchTrackingContent.drag-over {
                        background: rgba(139, 233, 253, 0.05);
                        outline: 2px dashed #8be9fd;
                        outline-offset: -8px;
                    }

                    /* Compact repo settings */
                    .patch-repo-settings {
                        background: rgba(0, 0, 0, 0.15);
                        border: 1px solid var(--border, #44475a);
                        border-radius: 6px;
                        padding: 12px;
                        margin-bottom: 16px;
                    }
                    .patch-repo-settings-compact {
                        display: flex;
                        gap: 8px;
                        align-items: center;
                    }
                    .patch-repo-settings-input {
                        flex: 1;
                        min-width: 0;
                    }

                    /* Progress animation */
                    @keyframes progress {
                        0% { transform: translateX(-100%); }
                        50% { transform: translateX(0); }
                        100% { transform: translateX(100%); }
                    }

                    @keyframes pulse {
                        0%, 100% { opacity: 1; }
                        50% { opacity: 0.5; }
                    }

                    /* Info cards */
                    .patch-info-card {
                        background: rgba(0, 0, 0, 0.2);
                        border: 1px solid var(--border, #44475a);
                        border-radius: 6px;
                        padding: 12px;
                        font-size: 12px;
                        margin-bottom: 12px;
                    }

                    .patch-info-card-row {
                        display: flex;
                        justify-content: space-between;
                        padding: 4px 0;
                    }

                    .patch-info-card-label {
                        color: var(--text-muted, #6272a4);
                        font-weight: 600;
                    }

                    .patch-info-card-value {
                        color: var(--text, #f8f8f2);
                    }

                    /* Scrollable routine list */
                    .patch-routine-list {
                        max-height: 200px;
                        overflow-y: auto;
                        background: rgba(0, 0, 0, 0.15);
                        border: 1px solid var(--border, #44475a);
                        border-radius: 4px;
                        padding: 8px;
                        font-size: 11px;
                        font-family: 'Courier New', monospace;
                        color: #50fa7b;
                        margin-top: 8px;
                    }

                    .patch-routine-list::-webkit-scrollbar {
                        width: 8px;
                    }

                    .patch-routine-list::-webkit-scrollbar-track {
                        background: rgba(0, 0, 0, 0.2);
                        border-radius: 4px;
                    }

                    .patch-routine-list::-webkit-scrollbar-thumb {
                        background: rgba(139, 233, 253, 0.3);
                        border-radius: 4px;
                    }

                    .patch-routine-list::-webkit-scrollbar-thumb:hover {
                        background: rgba(139, 233, 253, 0.5);
                    }
                </style>
            </div>
        `
    };

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.app = window.AhmadIDEModules.app || {};
        window.AhmadIDEModules.app.panelTemplates = panelTemplates;
    }
})();
