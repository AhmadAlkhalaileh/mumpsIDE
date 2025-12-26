/**
 * Call Hierarchy Panel
 * Displays call graph for MUMPS routines (static analysis)
 */

(() => {
    // Get callIndexer from global scope (loaded via script tag)
    const callIndexer = window.AhmadIDEModules?.callIndexer;
    if (!callIndexer) {
        console.error('[Call Hierarchy] callIndexer not found! Make sure callIndexer.js is loaded first.');
        return;
    }

    function createCallHierarchyManager({ deps } = {}) {
        const showToast = deps?.showToast || (() => {});

        let currentMode = 'outgoing'; // 'outgoing' or 'incoming'
        let currentRoutine = null;
        let currentLabel = null;
        let indexedDirectory = null;
        let isIndexing = false;

        function wireCallHierarchyPanel() {
            const featureRegistry = window.AhmadIDEModules?.app?.featureRegistry || null;

            const ensurePanelMounted = () => {
                try {
                    featureRegistry?.ensureById?.('callHierarchyPanel');
                } catch (_) {}
            };

            let panelDomWired = false;

            function wirePanelDom() {
                if (panelDomWired) return;

                const container = document.getElementById('callHierarchyPanel');
                if (!container) return;

                panelDomWired = true;

                // Get toolbar buttons
                const modeOutgoingBtn = document.getElementById('callHierarchyModeOutgoing');
                const modeIncomingBtn = document.getElementById('callHierarchyModeIncoming');
                const refreshBtn = document.getElementById('callHierarchyRefreshBtn');
                const searchInput = document.getElementById('callHierarchySearchInput');

                // Mode switching
                if (modeOutgoingBtn) {
                    modeOutgoingBtn.addEventListener('click', () => {
                        currentMode = 'outgoing';
                        updateModeButtons();
                        refreshTree();
                    });
                }

                if (modeIncomingBtn) {
                    modeIncomingBtn.addEventListener('click', () => {
                        currentMode = 'incoming';
                        updateModeButtons();
                        refreshTree();
                    });
                }

                // Refresh index
                if (refreshBtn) {
                    refreshBtn.addEventListener('click', async () => {
                        await reindexWorkspace();
                    });
                }

                // Search
                if (searchInput) {
                    searchInput.addEventListener('input', (e) => {
                        const query = e.target.value.trim();
                        filterTree(query);
                    });
                }

                function updateModeButtons() {
                    if (modeOutgoingBtn) {
                        modeOutgoingBtn.classList.toggle('active', currentMode === 'outgoing');
                    }
                    if (modeIncomingBtn) {
                        modeIncomingBtn.classList.toggle('active', currentMode === 'incoming');
                    }
                }

                updateModeButtons();
            }

            async function reindexWorkspace() {
                if (isIndexing) {
                    showToast('⏳ Indexing already in progress');
                    return;
                }

                const projectPath = getProjectPath();
                if (!projectPath) {
                    showToast('❌ No project open');
                    return;
                }

                isIndexing = true;
                showToast('🔍 Indexing routines...');

                try {
                    const stats = await callIndexer.indexDirectory(projectPath);

                    indexedDirectory = projectPath;
                    showToast(`✓ Indexed ${stats.totalRoutines} routines (${stats.totalCalls} calls) in ${stats.indexDuration}ms`);
                    refreshTree();

                } catch (error) {
                    console.error('[Call Hierarchy] Indexing error:', error);
                    showToast(`❌ Indexing failed: ${error.message}`);
                } finally {
                    isIndexing = false;
                }
            }

            function refreshTree() {
                const treeContainer = document.getElementById('callHierarchyTree');
                if (!treeContainer) return;

                if (!currentRoutine) {
                    treeContainer.innerHTML = `
                        <div class="ps-tree-empty">
                            <div style="text-align: center; padding: 20px; color: var(--text-secondary);">
                                <div style="font-size: 14px; margin-bottom: 8px;">No routine selected</div>
                                <div style="font-size: 11px;">Right-click in editor → "Show Call Hierarchy"</div>
                            </div>
                        </div>
                    `;
                    return;
                }

                if (!indexedDirectory) {
                    treeContainer.innerHTML = `
                        <div class="ps-tree-empty">
                            <div style="text-align: center; padding: 20px; color: var(--text-secondary);">
                                <div style="font-size: 14px; margin-bottom: 8px;">Index not built</div>
                                <button class="ps-btn-sm ps-btn-primary" onclick="document.getElementById('callHierarchyRefreshBtn')?.click()">
                                    Refresh Index
                                </button>
                            </div>
                        </div>
                    `;
                    return;
                }

                // Build call tree
                const calls = currentMode === 'outgoing'
                    ? callIndexer.getOutgoingCalls(currentRoutine, currentLabel)
                    : callIndexer.getIncomingCalls(currentRoutine, currentLabel);

                if (calls.length === 0) {
                    const modeText = currentMode === 'outgoing' ? 'calls nothing' : 'is not called';
                    treeContainer.innerHTML = `
                        <div class="ps-tree-empty">
                            <div style="text-align: center; padding: 20px; color: var(--text-secondary);">
                                <div style="font-size: 12px;">${currentLabel || currentRoutine} ${modeText}</div>
                            </div>
                        </div>
                    `;
                    return;
                }

                // Group calls
                const grouped = {};
                calls.forEach(call => {
                    const key = currentMode === 'outgoing'
                        ? `${call.callee}${call.calleeLabel ? '^' + call.calleeLabel : ''}`
                        : `${call.callerRoutine}${call.caller ? '^' + call.caller : ''}`;

                    if (!grouped[key]) grouped[key] = [];
                    grouped[key].push(call);
                });

                // Render tree
                let html = `<div class="ps-tree-section">`;
                html += `<div class="ps-tree-header">
                    <span data-ui-icon="chevron-down" data-ui-icon-size="12"></span>
                    ${currentLabel || currentRoutine} - ${currentMode === 'outgoing' ? 'Calls' : 'Called by'} (${calls.length})
                </div>`;

                Object.keys(grouped).sort().forEach(key => {
                    const callList = grouped[key];
                    const firstCall = callList[0];
                    const targetRoutine = currentMode === 'outgoing' ? firstCall.callee : firstCall.callerRoutine;
                    const targetLabel = currentMode === 'outgoing' ? firstCall.calleeLabel : firstCall.caller;
                    const isInternal = firstCall.isInternal;

                    const displayName = targetLabel ? `${targetLabel}^${targetRoutine}` : `^${targetRoutine}`;
                    const badge = isInternal ? '<span style="color: var(--accent); font-size: 9px;">INT</span>' : '';

                    html += `
                        <div class="ps-tree-item" data-routine="${targetRoutine}" data-label="${targetLabel || ''}">
                            <span>${displayName}</span>
                            ${badge}
                            <span style="color: var(--text-secondary); font-size: 10px; margin-left: auto;">${callList.length}×</span>
                        </div>
                    `;
                });

                html += `</div>`;
                treeContainer.innerHTML = html;

                // Wire click handlers
                treeContainer.querySelectorAll('.ps-tree-item').forEach(item => {
                    item.addEventListener('click', () => {
                        const routine = item.getAttribute('data-routine');
                        const label = item.getAttribute('data-label');
                        navigateToRoutine(routine, label);
                    });
                });
            }

            function filterTree(query) {
                const treeContainer = document.getElementById('callHierarchyTree');
                if (!treeContainer) return;

                const items = treeContainer.querySelectorAll('.ps-tree-item');
                items.forEach(item => {
                    const text = item.textContent.toLowerCase();
                    const matches = !query || text.includes(query.toLowerCase());
                    item.style.display = matches ? '' : 'none';
                });
            }

            function navigateToRoutine(routineName, labelName) {
                const routineInfo = callIndexer.getRoutineInfo(routineName);
                if (!routineInfo) {
                    showToast(`❌ Routine ${routineName} not found in index`);
                    return;
                }

                const filePath = routineInfo.filePath;
                if (window.ahmadIDE?.openFile) {
                    window.ahmadIDE.openFile(filePath);
                }
            }

            function getProjectPath() {
                if (window.AhmadIDEModules?.project?.getProjectRoot) {
                    return window.AhmadIDEModules.project.getProjectRoot();
                }

                try {
                    const projectPath = localStorage.getItem('ahmadIDE:currentProjectPath');
                    if (projectPath) return projectPath;
                } catch (_) {}

                return null;
            }

            // Wire DOM immediately
            ensurePanelMounted();
            wirePanelDom();

            return {
                showCallHierarchy: async (routineName, labelName = null) => {
                    currentRoutine = routineName;
                    currentLabel = labelName;

                    ensurePanelMounted();
                    wirePanelDom();

                    if (!indexedDirectory) {
                        await reindexWorkspace();
                    }

                    refreshTree();

                    const toggleToolWindowPanel = window.AhmadIDEModules?.ui?.toggleToolWindowPanel;
                    if (toggleToolWindowPanel) {
                        toggleToolWindowPanel('callHierarchyPanel', 'bottom');
                    }
                }
            };
        }

        return {
            wireCallHierarchyPanel
        };
    }

    // Export
    window.AhmadIDEModules = window.AhmadIDEModules || {};
    window.AhmadIDEModules.callHierarchy = { createCallHierarchyManager };
})();
