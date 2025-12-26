/**
 * Patch Tracking Panel Renderer
 * Wires up the patch tracking UI with Ahmad IDE
 */

(() => {
    console.log('[Patch Tracking] Renderer loading...');

        function createPatchTrackingManager() {
            let panelWired = false;
            let currentWorkflow = {
                step: 1,
                patch: null,
                scan: null,
                correlation: null
            };

        const GLOBAL_REPO_ROOT_KEY = 'ahmadIDE:gitRepoRootGlobal';
        const VISTA_REPO_ROOT_KEY = 'ahmadIDE:vistaRoutinesRepoPath';

        const showToast = window.AhmadIDEModules?.ui?.showToast || ((msg) => {
            console.log('[Toast]', msg);
            // Fallback toast
            const toast = document.createElement('div');
            toast.textContent = msg;
            toast.style.cssText = 'position: fixed; bottom: 20px; right: 20px; background: #333; color: #fff; padding: 12px 20px; border-radius: 4px; z-index: 10000;';
            document.body.appendChild(toast);
            setTimeout(() => toast.remove(), 3000);
        });

        function wirePatchTrackingPanel() {
            const featureRegistry = window.AhmadIDEModules?.app?.featureRegistry || null;

            const ensurePanelMounted = () => {
                try {
                    featureRegistry?.ensureById?.('patchTrackingPanel');
                } catch (e) {
                    console.error('[Patch Tracking] Mount failed:', e);
                }
            };

            function wirePanelDom() {
                if (panelWired) {
                    console.log('[Patch Tracking] Panel already wired');
                    return;
                }
                panelWired = true;

                console.log('[Patch Tracking] Wiring panel DOM...');

                const panel = document.getElementById('patchTrackingPanel');
                if (!panel) {
                    console.warn('[Patch Tracking] ❌ Panel element not found!');
                    return;
                }

                console.log('[Patch Tracking] ✓ Panel element found:', panel);
                console.log('[Patch Tracking] Panel innerHTML length:', panel.innerHTML.length);

                // Wire up workflow steps
                const steps = panel.querySelectorAll('.workflow-step');
                steps.forEach((step, index) => {
                    step.addEventListener('click', () => {
                        handleStepClick(index + 1);
                    });
                });

                // Wire up buttons
                const refreshBtn = document.getElementById('patchTrackingRefreshBtn');
                const menuBtn = document.getElementById('patchTrackingMenuBtn');
                const dropdown = document.getElementById('patchTrackingDropdown');

                if (refreshBtn) {
                    refreshBtn.addEventListener('click', () => refreshStatistics());
                }

                if (menuBtn && dropdown) {
                    menuBtn.addEventListener('click', (e) => {
                        e.stopPropagation();
                        const isVisible = dropdown.style.display !== 'none';
                        if (isVisible) {
                            dropdown.style.display = 'none';
                        } else {
                            renderDropdownMenu();
                            dropdown.style.display = 'block';
                        }
                    });

                    // Close dropdown when clicking outside
                    document.addEventListener('click', (e) => {
                        if (!dropdown.contains(e.target) && e.target !== menuBtn) {
                            dropdown.style.display = 'none';
                        }
                    });
                }

                // Initialize panel
                initializePanel();
            }

            async function initializePanel() {
                console.log('[Patch Tracking] Initializing panel');

                try {
                    // Load initial statistics
                    await refreshStatistics();

                    // Set initial workflow step (this will call wireRepoPathSettings when rendering step 1)
                    setActiveStep(1);

                    // Add global drag-and-drop support for entire panel
                    const contentArea = document.getElementById('patchTrackingContent');
                    if (contentArea) {
                        contentArea.addEventListener('dragover', (e) => {
                            e.preventDefault();
                            contentArea.classList.add('drag-over');
                        });

                        contentArea.addEventListener('dragleave', (e) => {
                            if (e.target === contentArea) {
                                contentArea.classList.remove('drag-over');
                            }
                        });

                        contentArea.addEventListener('drop', async (e) => {
                            e.preventDefault();
                            contentArea.classList.remove('drag-over');

                            const file = e.dataTransfer?.files?.[0];
                            if (file && (file.name.toLowerCase().endsWith('.kid') || file.name.toLowerCase().endsWith('.kids'))) {
                                // Switch to upload step and trigger upload
                                setActiveStep(1);
                                // Wait for DOM to update
                                setTimeout(() => {
                                    const fileInput = document.getElementById('patchFileInput');
                                    if (fileInput) {
                                        // Create a new FileList and trigger the upload
                                        const dataTransfer = new DataTransfer();
                                        dataTransfer.items.add(file);
                                        fileInput.files = dataTransfer.files;
                                        fileInput.dispatchEvent(new Event('change', { bubbles: true }));
                                    }
                                }, 100);
                            } else {
                                showToast('Please drop a .KIDS file');
                            }
                        });
                    }

                    console.log('[Patch Tracking] Panel initialized successfully');
                } catch (error) {
                    console.error('[Patch Tracking] Initialization error:', error);
                    showToast(`Failed to initialize patch tracking: ${error.message}`);
                }
            }

            async function renderDropdownMenu() {
                const dropdown = document.getElementById('patchTrackingDropdown');
                if (!dropdown) return;

                // Get patch history for graph
                let patchHistory = [];
                try {
                    const stats = await window.ahmadIDE.patchTracking.getStatistics();
                    if (stats.success && stats.history) {
                        patchHistory = stats.history || [];
                    }
                } catch (e) {
                    console.error('[Patch Tracking] Failed to load patch history:', e);
                }

                dropdown.innerHTML = `
                    <div style="padding: 12px;">
                        <!-- Repo Settings Section -->
                        <div style="padding-bottom: 12px; border-bottom: 1px solid var(--border, #44475a);">
                            <div style="font-size: 11px; font-weight: 700; color: var(--text-muted, #6272a4); margin-bottom: 10px; text-transform: uppercase; letter-spacing: 0.5px;">Git Repository</div>
                            <div style="display: flex; gap: 6px; margin-bottom: 8px;">
                                <input type="text" id="dropdownGitRepoPath" placeholder="~/Desktop/vista-routines"
                                    style="flex: 1; padding: 6px 8px; background: rgba(0, 0, 0, 0.3); border: 1px solid var(--border, #44475a); border-radius: 4px; color: var(--text, #f8f8f2); font-size: 11px;">
                                <button id="dropdownBrowseRepo" class="patch-btn patch-btn-secondary" style="padding: 6px 8px; font-size: 11px;">
                                    <svg width="12" height="12" fill="none" stroke="currentColor" stroke-width="2">
                                        <path d="M2 4v6a1 1 0 0 0 1 1h6a1 1 0 0 0 1-1V6l-3-3H3a1 1 0 0 0-1 1z"/>
                                        <path d="M7 3v4h4"/>
                                    </svg>
                                </button>
                            </div>
                            <button id="dropdownSetRepoPath" class="patch-btn patch-btn-primary" style="width: 100%; font-size: 11px; padding: 6px;">
                                Set Repository Path
                            </button>
                            <div id="dropdownRepoStatus" style="font-size: 10px; color: var(--text-muted, #6272a4); margin-top: 6px;"></div>
                        </div>

                        <!-- Patch History Graph -->
                        <div style="padding: 12px 0;">
                            <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;">
                                <div style="font-size: 11px; font-weight: 700; color: var(--text-muted, #6272a4); text-transform: uppercase; letter-spacing: 0.5px;">Patch Activity</div>
                                <div style="font-size: 10px; color: var(--text-muted, #6272a4);">Last 30 days</div>
                            </div>
                            <div id="patchHistoryGraph" style="height: 120px; position: relative;">
                                ${renderPatchGraph(patchHistory)}
                            </div>
                        </div>
                    </div>
                `;

                // Wire up repo settings in dropdown
                wireDropdownRepoSettings();
            }

            function renderPatchGraph(history) {
                if (!history || history.length === 0) {
                    return `
                        <div style="display: flex; align-items: center; justify-content: center; height: 100%; color: var(--text-muted, #6272a4); font-size: 11px;">
                            <svg width="32" height="32" fill="none" stroke="currentColor" stroke-width="1.5" style="opacity: 0.3; margin-right: 8px;">
                                <path d="M4 20l6-6 4 4 6-6 4 4" stroke-linecap="round" stroke-linejoin="round"/>
                                <circle cx="4" cy="20" r="2" fill="currentColor"/>
                                <circle cx="10" cy="14" r="2" fill="currentColor"/>
                                <circle cx="14" cy="18" r="2" fill="currentColor"/>
                                <circle cx="20" cy="12" r="2" fill="currentColor"/>
                                <circle cx="24" cy="16" r="2" fill="currentColor"/>
                            </svg>
                            No patch activity yet
                        </div>
                    `;
                }

                // Generate last 30 days of data
                const days = 30;
                const today = new Date();
                const dataPoints = [];

                for (let i = days - 1; i >= 0; i--) {
                    const date = new Date(today);
                    date.setDate(date.getDate() - i);
                    const dateStr = date.toISOString().split('T')[0];

                    const count = history.filter(p => {
                        const patchDate = new Date(p.committedAt || p.createdAt);
                        return patchDate.toISOString().split('T')[0] === dateStr;
                    }).length;

                    dataPoints.push({ date: dateStr, count });
                }

                const maxCount = Math.max(...dataPoints.map(d => d.count), 1);
                const barWidth = 100 / days;

                return `
                    <div style="display: flex; align-items: flex-end; height: 100%; gap: 1px;">
                        ${dataPoints.map((point, i) => {
                            const height = (point.count / maxCount) * 100;
                            const color = point.count === 0 ? 'rgba(68, 71, 90, 0.5)' :
                                         point.count === 1 ? 'rgba(139, 233, 253, 0.4)' :
                                         point.count === 2 ? 'rgba(139, 233, 253, 0.6)' :
                                         'rgba(139, 233, 253, 0.8)';

                            return `
                                <div style="flex: 1; display: flex; flex-direction: column; justify-content: flex-end; height: 100%; position: relative;"
                                     title="${point.date}: ${point.count} patch${point.count !== 1 ? 'es' : ''}">
                                    <div style="width: 100%; background: ${color}; height: ${height || 2}%; border-radius: 2px 2px 0 0; transition: all 0.2s;"
                                         onmouseenter="this.style.background='#8be9fd'; this.style.transform='scaleY(1.1)'"
                                         onmouseleave="this.style.background='${color}'; this.style.transform='scaleY(1)'">
                                    </div>
                                </div>
                            `;
                        }).join('')}
                    </div>
                    <div style="display: flex; justify-content: space-between; margin-top: 6px; font-size: 9px; color: var(--text-muted, #6272a4);">
                        <span>${dataPoints[0].date.slice(5)}</span>
                        <span>Today</span>
                    </div>
                    <div style="margin-top: 8px; padding-top: 8px; border-top: 1px solid var(--border, #44475a); display: flex; justify-content: space-between; font-size: 10px;">
                        <span style="color: var(--text-muted, #6272a4);">Total commits:</span>
                        <span style="color: #8be9fd; font-weight: 600;">${dataPoints.reduce((sum, d) => sum + d.count, 0)}</span>
                    </div>
                `;
            }

            function wireDropdownRepoSettings() {
                const input = document.getElementById('dropdownGitRepoPath');
                const browseBtn = document.getElementById('dropdownBrowseRepo');
                const setBtn = document.getElementById('dropdownSetRepoPath');
                const status = document.getElementById('dropdownRepoStatus');

                if (!input || !browseBtn || !setBtn || !status) return;

                // Load saved path
                let savedPath = '';
                try {
                    savedPath = String(localStorage.getItem(GLOBAL_REPO_ROOT_KEY) || '').trim();
                    if (!savedPath) {
                        savedPath = String(localStorage.getItem(VISTA_REPO_ROOT_KEY) || '').trim();
                    }
                    if (!savedPath) {
                        const repoManager = window.AhmadIDEModules?.git?.repoManager;
                        const st = repoManager?.getState?.() || {};
                        savedPath = String(st.repoRootOverride || st.repoRoot || '').trim();
                    }
                } catch (_) {}

                if (savedPath) {
                    input.value = savedPath;
                    status.textContent = `Current: ${savedPath}`;
                    status.style.color = '#50fa7b';
                }

                browseBtn.addEventListener('click', async () => {
                    try {
                        const result = await window.ahmadIDE.openFolderDialog();
                        if (result?.ok && result.path) {
                            input.value = result.path;
                            status.textContent = `Selected: ${result.path}`;
                            status.style.color = '#8be9fd';
                        }
                    } catch (error) {
                        console.error('[Patch Tracking] Browse error:', error);
                        showToast('Failed to open folder dialog');
                    }
                });

                setBtn.addEventListener('click', async () => {
                    const repoPath = input.value.trim();
                    if (!repoPath) {
                        showToast('Please enter a repository path');
                        return;
                    }

                    setBtn.disabled = true;
                    setBtn.textContent = 'Setting...';

                    try {
                        const result = await window.ahmadIDE.patchTracking.setRepoPath(repoPath);

                        if (result.success) {
                            const normalizedPath = String(result.repoPath || repoPath).trim();
                            await window.ahmadIDE.gitBlame.setRepoPath(normalizedPath);

                            try {
                                localStorage.setItem(GLOBAL_REPO_ROOT_KEY, normalizedPath);
                                localStorage.setItem(VISTA_REPO_ROOT_KEY, normalizedPath);
                            } catch (_) {}

                            input.value = normalizedPath;
                            status.textContent = `✓ Set: ${normalizedPath}`;
                            status.style.color = '#50fa7b';
                            showToast('Repository path set successfully!');
                        } else {
                            throw new Error(result.error || 'Failed to set repo path');
                        }
                    } catch (error) {
                        console.error('[Patch Tracking] Set repo path error:', error);
                        status.textContent = `✗ Error: ${error.message}`;
                        status.style.color = '#ff5555';
                        showToast(`Failed to set repo path: ${error.message}`);
                    } finally {
                        setBtn.disabled = false;
                        setBtn.textContent = 'Set Repository Path';
                    }
                });
            }

            function wireRepoPathSettings() {
                const setRepoPathBtn = document.getElementById('setRepoPathBtn');
                const browseRepoBtn = document.getElementById('browseRepoBtn');
                const gitRepoPathInput = document.getElementById('gitRepoPathInput');
                const repoPathStatus = document.getElementById('repoPathStatus');

                // Guard: If elements don't exist, exit early
                if (!setRepoPathBtn || !browseRepoBtn || !gitRepoPathInput || !repoPathStatus) {
                    console.warn('[Patch Tracking] Repo path settings elements not found, skipping wire-up');
                    return;
                }

                // Prefer persisted path, else derive from current project repoRoot, else leave blank.
                let initialRepoPath = '';
                try {
                    initialRepoPath = String(localStorage.getItem(GLOBAL_REPO_ROOT_KEY) || '').trim();
                } catch (_) { }
                if (!initialRepoPath) {
                    try {
                        initialRepoPath = String(localStorage.getItem(VISTA_REPO_ROOT_KEY) || '').trim();
                    } catch (_) { }
                }

                if (!initialRepoPath) {
                    try {
                        const repoManager = window.AhmadIDEModules?.git?.repoManager;
                        const st = repoManager?.getState?.() || {};
                        initialRepoPath = String(st.repoRootOverride || st.repoRoot || '').trim();
                    } catch (_) { }
                }

                if (initialRepoPath) {
                    gitRepoPathInput.value = initialRepoPath;
                    repoPathStatus.textContent = `Saved: ${initialRepoPath} (click "Set Path" to apply)`;
                    repoPathStatus.style.color = '#6272a4';
                } else {
                    repoPathStatus.textContent = 'Choose your routines Git repo (e.g. ~/Desktop/vista-routines) and click "Set Path".';
                    repoPathStatus.style.color = '#6272a4';
                }

                // Set repo path button
                if (setRepoPathBtn) {
                    setRepoPathBtn.addEventListener('click', async () => {
                        const repoPath = gitRepoPathInput.value.trim();

                        if (!repoPath) {
                            showToast('Please enter a repository path');
                            return;
                        }

                        setRepoPathBtn.disabled = true;
                        setRepoPathBtn.textContent = 'Setting...';

                        try {
                            const result = await window.ahmadIDE.patchTracking.setRepoPath(repoPath);

                            if (result.success) {
                                const normalizedPath = String(result.repoPath || repoPath).trim();

                                // Also set Git blame repo path for Docker file mapping
                                await window.ahmadIDE.gitBlame.setRepoPath(normalizedPath);

                                try {
                                    localStorage.setItem(GLOBAL_REPO_ROOT_KEY, normalizedPath);
                                    localStorage.setItem(VISTA_REPO_ROOT_KEY, normalizedPath);
                                } catch (_) { }

                                gitRepoPathInput.value = normalizedPath;
                                repoPathStatus.textContent = `✓ Repository set: ${normalizedPath}`;
                                repoPathStatus.style.color = '#50fa7b';
                                showToast('Git repository path set successfully!');
                            } else {
                                throw new Error(result.error || 'Failed to set repo path');
                            }
                        } catch (error) {
                            console.error('[Patch Tracking] Set repo path error:', error);
                            repoPathStatus.textContent = `✗ Error: ${error.message}`;
                            repoPathStatus.style.color = '#ff5555';
                            showToast(`Failed to set repo path: ${error.message}`);
                        } finally {
                            setRepoPathBtn.disabled = false;
                            setRepoPathBtn.textContent = 'Set Path';
                        }
                    });
                }

                // Browse button
                if (browseRepoBtn) {
                    browseRepoBtn.addEventListener('click', async () => {
                        try {
                            const result = await window.ahmadIDE.openFolderDialog();

                            if (result?.ok && result.path) {
                                const selectedPath = result.path;
                                gitRepoPathInput.value = selectedPath;
                                repoPathStatus.textContent = `Selected: ${selectedPath}`;
                                repoPathStatus.style.color = '#8be9fd';
                            }
                        } catch (error) {
                            console.error('[Patch Tracking] Browse error:', error);
                            showToast('Failed to open folder dialog');
                        }
                    });
                }
            }

            function setActiveStep(stepNumber) {
                currentWorkflow.step = stepNumber;

                const panel = document.getElementById('patchTrackingPanel');
                if (!panel) return;

                const steps = panel.querySelectorAll('.workflow-step');
                steps.forEach((step, index) => {
                    step.classList.remove('active', 'completed');

                    if (index + 1 === stepNumber) {
                        step.classList.add('active');
                    } else if (index + 1 < stepNumber) {
                        step.classList.add('completed');
                    }
                });

                renderStepContent(stepNumber);
            }

            function renderStepContent(stepNumber) {
                const contentArea = document.getElementById('patchTrackingContent');
                if (!contentArea) return;

                switch (stepNumber) {
                    case 1:
                        renderUploadStep(contentArea);
                        break;
                    case 2:
                        renderScanStep(contentArea);
                        break;
                    case 3:
                        renderCorrelateStep(contentArea);
                        break;
                    case 4:
                        renderCommitStep(contentArea);
                        break;
                }
            }

            function renderUploadStep(container) {
                container.innerHTML = `
                    <div class="patch-form">
                        <!-- Drag and Drop Upload Area -->
                        <div class="patch-form-group">
                            <label class="patch-form-label">Upload KIDS Patch File</label>
                            <div id="patchDropZone" style="border: 2px dashed var(--border, #44475a); border-radius: 6px; padding: 40px; text-align: center; background: rgba(0, 0, 0, 0.15); cursor: pointer; transition: all 0.2s;">
                                <input type="file" id="patchFileInput" accept=".KID,.KIDS,.kid,.kids,*" style="display: none;">
                                <svg width="56" height="56" fill="none" stroke="var(--text-muted, #6272a4)" stroke-width="1.5" style="opacity: 0.4; margin: 0 auto 16px;">
                                    <rect x="8" y="8" width="40" height="40" rx="2"/>
                                    <path d="M28 20v16M20 28h16" stroke-linecap="round"/>
                                </svg>
                                <div style="font-size: 14px; font-weight: 600; color: var(--text, #f8f8f2); margin-bottom: 6px;">Drop .KIDS file here</div>
                                <div style="font-size: 12px; color: var(--text-muted, #6272a4);">or <span style="color: #8be9fd; text-decoration: underline;">click to browse</span></div>
                                <div style="font-size: 11px; color: var(--text-muted, #6272a4); margin-top: 12px;">Supports .KID and .KIDS files</div>
                            </div>
                        </div>
                        <div id="patchUploadResult"></div>
                    </div>
                `;

                // Wire up drag and drop upload
                const dropZone = document.getElementById('patchDropZone');
                const fileInput = document.getElementById('patchFileInput');

                const handleFileUpload = async (file) => {
                    if (!file) return;

                    try {
                        // Update drop zone to show loading
                        dropZone.innerHTML = `
                            <div style="padding: 20px;">
                                <div style="font-size: 13px; color: #8be9fd;">Parsing ${file.name}...</div>
                                <div style="width: 200px; height: 4px; background: rgba(139, 233, 253, 0.2); border-radius: 2px; margin: 12px auto; overflow: hidden;">
                                    <div style="width: 100%; height: 100%; background: #8be9fd; animation: progress 1s ease-in-out infinite;"></div>
                                </div>
                            </div>
                        `;

                        showToast('Reading KIDS file...');

                        // Read file content
                        const content = await new Promise((resolve, reject) => {
                            const reader = new FileReader();
                            reader.onload = (e) => resolve(e.target.result);
                            reader.onerror = (e) => reject(new Error('Failed to read file'));
                            reader.readAsText(file);
                        });

                        console.log('[Patch Tracking] File read successfully, size:', content.length);

                        // Parse KIDS content via IPC
                        showToast('Parsing KIDS file...');
                        const parseResult = await window.ahmadIDE.patchTracking.uploadPatch(content);

                        if (!parseResult.success) {
                            throw new Error(parseResult.error || 'Failed to parse KIDS file');
                        }

                        console.log('[Patch Tracking] Patch parsed:', parseResult.patchId);

                        // Store patch info for next steps
                        currentWorkflow.patch = parseResult;

                        const resultDiv = document.getElementById('patchUploadResult');
                        if (resultDiv) {
                            const metadata = parseResult.metadata || {};
                            resultDiv.innerHTML = `
                                <div class="patch-result-item success">
                                    <div class="patch-result-header">
                                        <div class="patch-result-title">✓ Patch Parsed Successfully</div>
                                        <div class="patch-result-badge">READY</div>
                                    </div>
                                    <div class="patch-result-body">
                                        <strong>Patch ID:</strong> ${parseResult.patchId || 'Unknown'}<br>
                                        <strong>Title:</strong> ${metadata.title || 'N/A'}<br>
                                        <strong>Routines:</strong> ${(metadata.routines || []).length} file(s)<br>
                                        ${metadata.routines && metadata.routines.length > 0 ?
                                            '<div style="margin-top:8px;font-size:11px;color:#6272a4;">Modified: ' +
                                            metadata.routines.slice(0, 5).join(', ') +
                                            (metadata.routines.length > 5 ? ' +' + (metadata.routines.length - 5) + ' more' : '') +
                                            '</div>' : ''}
                                    </div>
                                </div>
                            `;
                        }

                        // Update drop zone to show success
                        dropZone.innerHTML = `
                            <div style="padding: 12px;">
                                <svg width="32" height="32" fill="none" stroke="#50fa7b" stroke-width="2" style="margin: 0 auto 8px;">
                                    <circle cx="16" cy="16" r="14"/>
                                    <path d="M10 16l4 4 8-8" stroke-linecap="round" stroke-linejoin="round"/>
                                </svg>
                                <div style="font-size: 13px; color: #50fa7b; font-weight: 600;">${file.name}</div>
                                <div style="font-size: 11px; color: var(--text-muted, #6272a4); margin-top: 4px;">Uploaded successfully</div>
                            </div>
                        `;
                        dropZone.style.borderColor = '#50fa7b';
                        dropZone.style.background = 'rgba(80, 250, 123, 0.08)';

                        showToast(`Patch ${parseResult.patchId} ready!`);

                        // Move to next step after delay
                        setTimeout(() => {
                            setActiveStep(2);
                        }, 1500);

                    } catch (error) {
                        console.error('[Patch Tracking] Upload error:', error);
                        showToast(`Upload failed: ${error.message}`);

                        const resultDiv = document.getElementById('patchUploadResult');
                        if (resultDiv) {
                            resultDiv.innerHTML = `
                                <div class="patch-result-item error">
                                    <div class="patch-result-header">
                                        <div class="patch-result-title">✗ Upload Failed</div>
                                        <div class="patch-result-badge">ERROR</div>
                                    </div>
                                    <div class="patch-result-body">
                                        ${error.message}
                                    </div>
                                </div>
                            `;
                        }

                        // Reset drop zone
                        dropZone.innerHTML = `
                            <svg width="48" height="48" fill="none" stroke="var(--text-muted, #6272a4)" stroke-width="1.5" style="opacity: 0.4; margin: 0 auto 12px;">
                                <rect x="8" y="8" width="32" height="32" rx="2"/>
                                <path d="M24 18v12M18 24h12" stroke-linecap="round"/>
                            </svg>
                            <div style="font-size: 13px; font-weight: 500; color: var(--text, #f8f8f2); margin-bottom: 4px;">Drop .KIDS file here</div>
                            <div style="font-size: 12px; color: var(--text-muted, #6272a4);">or <span style="color: #8be9fd; text-decoration: underline;">click to browse</span></div>
                        `;
                        dropZone.style.borderColor = '';
                        dropZone.style.background = '';
                    }
                };

                // Click to browse
                if (dropZone && fileInput) {
                    dropZone.addEventListener('click', () => {
                        fileInput.click();
                    });

                    // Handle file selection via browse
                    fileInput.addEventListener('change', (e) => {
                        const file = e.target.files?.[0];
                        if (file) handleFileUpload(file);
                    });

                    // Handle drag and drop
                    dropZone.addEventListener('dragover', (e) => {
                        e.preventDefault();
                        dropZone.style.borderColor = '#8be9fd';
                        dropZone.style.background = 'rgba(139, 233, 253, 0.08)';
                    });

                    dropZone.addEventListener('dragleave', (e) => {
                        e.preventDefault();
                        dropZone.style.borderColor = '';
                        dropZone.style.background = '';
                    });

                    dropZone.addEventListener('drop', (e) => {
                        e.preventDefault();
                        dropZone.style.borderColor = '';
                        dropZone.style.background = '';

                        const file = e.dataTransfer?.files?.[0];
                        if (file) handleFileUpload(file);
                    });
                }
            }

            function renderScanStep(container) {
                if (!currentWorkflow.patch) {
                    container.innerHTML = `
                        <div style="text-align:center;padding:40px;color:#6272a4;">
                            <svg width="48" height="48" fill="none" stroke="currentColor" stroke-width="1.5" style="opacity: 0.3; margin-bottom: 12px;">
                                <circle cx="24" cy="24" r="20"/>
                                <path d="M12 24h24" stroke-linecap="round"/>
                            </svg>
                            <div>Please upload a patch first</div>
                        </div>
                    `;
                    return;
                }

                const metadata = currentWorkflow.patch.metadata || {};
                const routines = metadata.routines || [];

                container.innerHTML = `
                    <div class="patch-form">
                        <!-- Compact Patch Info -->
                        <div class="patch-info-card" style="padding: 10px; margin-bottom: 10px;">
                            <div style="font-size: 10px; font-weight: 700; color: var(--text-muted, #6272a4); margin-bottom: 6px; text-transform: uppercase;">Patch: <strong style="color: #8be9fd;">${metadata.patchId || 'Unknown'}</strong></div>
                            <div style="font-size: 11px; margin-bottom: 4px;">${metadata.title || 'N/A'}</div>
                            <div style="font-size: 10px; color: var(--text-muted, #6272a4);">
                                ${routines.length} routines: ${routines.slice(0, 5).join(', ')}${routines.length > 5 ? '...' : ''}
                            </div>
                        </div>

                        <!-- Compact Connection Info -->
                        <div class="patch-info-card" style="padding: 10px; margin-bottom: 10px;">
                            <div style="font-size: 10px; font-weight: 700; color: var(--text-muted, #6272a4); margin-bottom: 6px; text-transform: uppercase;">Connection</div>
                            <div id="connectionInfo" style="font-size: 11px;">
                                <div style="display: flex; align-items: center; gap: 6px;">
                                    <div style="width: 10px; height: 10px; border: 2px solid currentColor; border-top-color: transparent; border-radius: 50%; animation: spin 1s linear infinite;"></div>
                                    Detecting...
                                </div>
                            </div>
                        </div>

                        <div class="patch-actions">
                            <button class="patch-btn patch-btn-primary" id="startScanBtn">
                                <svg width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" style="margin-right: 4px; vertical-align: middle;">
                                    <circle cx="7" cy="7" r="6"/>
                                    <path d="M7 4v3l2 2" stroke-linecap="round"/>
                                </svg>
                                Start Environment Scan
                            </button>
                            <button class="patch-btn patch-btn-secondary" id="forceProceedBtn" style="display: none;">
                                <svg width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" style="margin-right: 4px; vertical-align: middle;">
                                    <path d="M1 7h12M7 1l6 6-6 6" stroke-linecap="round" stroke-linejoin="round"/>
                                </svg>
                                Force Proceed Anyway
                            </button>
                        </div>
                        <div id="scanResult"></div>

                        <style>
                            @keyframes spin {
                                to { transform: rotate(360deg); }
                            }
                        </style>
                    </div>
                `;

                // Detect current connection
                (async () => {
                    try {
                        const conn = await window.ahmadIDE.getConnection();
                        const connInfo = document.getElementById('connectionInfo');
                        if (connInfo && conn.ok) {
                            connInfo.innerHTML = `
                                <div style="font-size: 11px;">
                                    <strong style="color: #50fa7b;">● Connected</strong> · ${conn.type} · ${conn.paths.envKey}
                                    <div style="font-size: 9px; color: var(--text-muted, #6272a4); margin-top: 4px;">
                                        ${conn.paths.localrPath}
                                    </div>
                                </div>
                            `;
                        } else {
                            throw new Error('No connection');
                        }
                    } catch (error) {
                        console.error('[Patch Tracking] Connection detection error:', error);
                        const connInfo = document.getElementById('connectionInfo');
                        if (connInfo) {
                            connInfo.innerHTML = `
                                <div style="font-size: 11px; color: #ff5555;">
                                    <strong>● Not Connected</strong>
                                    <div style="font-size: 9px; margin-top: 3px;">Connect to Docker/SSH first</div>
                                </div>
                            `;
                        }
                    }
                })();

                // Wire up Force Proceed button
                const forceProceedBtn = document.getElementById('forceProceedBtn');
                if (forceProceedBtn) {
                    forceProceedBtn.addEventListener('click', () => {
                        console.log('[Patch Tracking] Force proceeding to correlation despite 0 changes');
                        currentWorkflow.forcedMode = true; // Set forced mode flag
                        showToast('⚠ Entering forced mode - manual commit will be required');
                        setActiveStep(3);
                    });
                }

                const startScanBtn = document.getElementById('startScanBtn');
                if (startScanBtn) {
                    startScanBtn.addEventListener('click', async () => {

                        startScanBtn.disabled = true;
                        startScanBtn.textContent = 'Scanning...';

                        try {
                            // Get current IDE connection
                            const conn = await window.ahmadIDE.getConnection();
                            if (!conn.ok) {
                                throw new Error('No active connection found. Please connect to Docker or SSH first.');
                            }

                            // Handle both Docker and SSH connections
                            const connectionId = conn.connectionId || conn.type || 'docker';
                            const localrPath = conn.paths.localrPath;
                            const routinesPath = conn.paths.routinesPath;

                            if (!localrPath || !routinesPath) {
                                throw new Error('Invalid connection paths. Please check your connection settings.');
                            }

                            console.log('[Patch Tracking] About to scan with patchId:', currentWorkflow.patch.patchId);
                            console.log('[Patch Tracking] Connection ID:', connectionId);
                            console.log('[Patch Tracking] Patch routines:', currentWorkflow.patch.metadata.routines);

                            showToast(`Scanning ${currentWorkflow.patch.metadata.routines.length} routines from patch...`);

                            const scanResult = await window.ahmadIDE.patchTracking.scanEnvironment({
                                connectionId,
                                envName: conn.type,
                                localrPath,
                                routinesPath,
                                patchId: currentWorkflow.patch.patchId // Pass patch ID for targeted scan
                            });

                            if (!scanResult.success) {
                                throw new Error(scanResult.error || 'Scan failed');
                            }

                            console.log('[Patch Tracking] Scan complete:', scanResult);

                            // Store scan info for correlation
                            currentWorkflow.scan = scanResult;

                            const summary = scanResult.changeResult?.summary || {};
                            const scanData = scanResult.scanResult || {};
                            const totalChanges = (summary.modified || 0) + (summary.added || 0);
                            const hasChanges = totalChanges > 0; // Move outside for broader scope

                            const resultDiv = document.getElementById('scanResult');
                            if (resultDiv) {
                                resultDiv.innerHTML = `
                                    <div class="patch-result-item ${hasChanges ? 'success' : 'warning'}" style="margin-top: 12px;">
                                        <div class="patch-result-header">
                                            <div class="patch-result-title" style="font-size: 12px;">${hasChanges ? '✓ Scan Complete' : '⚠ No Changes Detected'}</div>
                                            <div class="patch-result-badge" style="font-size: 9px;">${hasChanges ? 'DONE' : 'WARNING'}</div>
                                        </div>
                                        <div class="patch-result-body" style="font-size: 11px;">
                                            ${hasChanges ? `
                                                <strong>Changes:</strong> ${summary.modified || 0} modified, ${summary.added || 0} added
                                                <div style="margin-top:6px;font-size:10px;color:#50fa7b;">Change ID: ${scanResult.changeId}</div>
                                            ` : `
                                                <strong style="color:#ffb86c;">Options:</strong><br>
                                                <div style="margin-top:6px;font-size:10px;line-height:1.5;">
                                                    • Click <strong>"Force Proceed Anyway"</strong> to continue<br>
                                                    • Or install patch <strong>${currentWorkflow.patch?.patchId || 'N/A'}</strong> in Docker first<br>
                                                    <div style="margin-top:4px;color:var(--text-muted,#6272a4);">
                                                        Paths: <code style="font-size:9px;background:rgba(0,0,0,0.3);padding:1px 3px;">${scanData.paths?.localr || 'N/A'}</code>
                                                    </div>
                                                </div>
                                            `}
                                        </div>
                                    </div>
                                `;
                            }

                            if (hasChanges) {
                                showToast(`✓ Found ${totalChanges} changes!`);
                                setTimeout(() => {
                                    setActiveStep(3);
                                }, 1500);
                            } else {
                                showToast('⚠ No changes detected - Click "Force Proceed" or install patch in Docker first', 'warning');

                                // Show the "Force Proceed" button
                                const forceProceedBtn = document.getElementById('forceProceedBtn');
                                if (forceProceedBtn) {
                                    forceProceedBtn.style.display = 'inline-flex';
                                }

                                // DO NOT auto-advance to step 3 - user must use Force Proceed
                            }

                        } catch (error) {
                            console.error('[Patch Tracking] Scan error:', error);
                            showToast(`Scan failed: ${error.message}`);

                            const resultDiv = document.getElementById('scanResult');
                            if (resultDiv) {
                                resultDiv.innerHTML = `
                                    <div class="patch-result-item error">
                                        <div class="patch-result-header">
                                            <div class="patch-result-title">Scan Failed</div>
                                            <div class="patch-result-badge">ERROR</div>
                                        </div>
                                        <div class="patch-result-body">${error.message}</div>
                                    </div>
                                `;
                            }
                        } finally {
                            startScanBtn.disabled = false;
                            startScanBtn.textContent = 'Start Scan';
                        }
                    });
                }
            }

            function renderCorrelateStep(container) {
                container.innerHTML = `
                    <div class="patch-form">
                        <div class="patch-form-group">
                            <label class="patch-form-label">Correlate Patch with Changes</label>
                            <p style="font-size: 13px; color: var(--fg-muted, #808080); margin: 8px 0;">
                                This step will match the patch routines with the detected changes in the environment.
                            </p>
                        </div>
                        <div class="patch-actions">
                            <button class="patch-btn patch-btn-primary" id="correlateBtn">
                                Run Correlation
                            </button>
                        </div>
                        <div id="correlationResult"></div>
                    </div>
                `;

                const correlateBtn = document.getElementById('correlateBtn');
                if (correlateBtn) {
                    correlateBtn.addEventListener('click', async () => {
                        if (!currentWorkflow.patch) {
                            showToast('Please complete upload step first');
                            return;
                        }

                        // Check if scan has valid changes
                        const scanHasChanges = currentWorkflow.scan?.changeResult?.summary?.modified > 0 ||
                                              currentWorkflow.scan?.changeResult?.summary?.added > 0;

                        // Check if we're in "forced" mode (0 changes detected)
                        const isForced = !currentWorkflow.scan || !currentWorkflow.scan.changeId || !scanHasChanges;

                        if (isForced) {
                            // Create manual correlation from patch metadata
                            showToast('⚠ Creating manual correlation (no changes detected)');
                            const manualCorrelation = {
                                success: true,
                                correlation: {
                                    matched: (currentWorkflow.patch.metadata?.routines || []).map(routine => ({
                                        routine: routine,
                                        patchRoutine: routine,
                                        status: 'forced', // Mark as forced
                                        confidence: 0
                                    })),
                                    missing: [],
                                    unmatched: [],
                                    matchRate: 0
                                },
                                validation: {
                                    canCommit: true,
                                    warnings: ['⚠ No environment changes detected - files will be created/overwritten from patch metadata'],
                                    errors: []
                                }
                            };

                            currentWorkflow.correlation = manualCorrelation;

                            // Show manual correlation result
                            const resultDiv = document.getElementById('correlationResult');
                            if (resultDiv) {
                                resultDiv.innerHTML = `
                                    <div class="patch-result-item warning">
                                        <div class="patch-result-header">
                                            <div class="patch-result-title">⚠ Manual Correlation (Forced Mode)</div>
                                            <div class="patch-result-badge">FORCED</div>
                                        </div>
                                        <div class="patch-result-body">
                                            <strong>Manual Mode:</strong><br>
                                            • Routines from patch: ${manualCorrelation.correlation.matched.length}<br>
                                            • Status: Will be committed as-is<br>
                                            <div style="margin-top:12px;padding:10px;background:rgba(255,184,108,0.1);border:1px solid rgba(255,184,108,0.3);border-radius:4px;font-size:11px;">
                                                <strong style="color:#ffb86c;">⚠ Warning</strong><br>
                                                <div style="margin-top:6px;color:var(--text-muted,#6272a4);">
                                                    No environment changes were detected. The commit will be based on patch metadata only.
                                                    Make sure you've copied the routines from Docker to your Git repo manually.
                                                </div>
                                            </div>
                                        </div>
                                    </div>
                                `;
                            }

                            showToast('Manual correlation ready - proceed to commit');
                            setTimeout(() => setActiveStep(4), 1500);
                            return;
                        }

                        if (!currentWorkflow.scan) {
                            showToast('Please complete scan step first');
                            return;
                        }

                        correlateBtn.disabled = true;
                        correlateBtn.textContent = 'Correlating...';

                        try {
                            showToast('Running correlation...');

                            const correlateResult = await window.ahmadIDE.patchTracking.correlate(
                                currentWorkflow.patch.patchId,
                                currentWorkflow.scan.changeId
                            );

                            if (!correlateResult.success) {
                                throw new Error(correlateResult.error || 'Correlation failed');
                            }

                            console.log('[Patch Tracking] Correlation complete:', correlateResult);

                            // Store correlation for commit step
                            currentWorkflow.correlation = correlateResult;

                            const correlation = correlateResult.correlation || {};
                            const validation = correlateResult.validation || {};
                            const matchRate = correlation.matchRate || 0;
                            const isValid = validation.canCommit;

                            const resultDiv = document.getElementById('correlationResult');
                            if (resultDiv) {

                                resultDiv.innerHTML = `
                                    <div class="patch-result-item ${isValid ? 'success' : 'warning'}">
                                        <div class="patch-result-header">
                                            <div class="patch-result-title">Correlation ${isValid ? 'Complete' : 'Complete (Warnings)'}</div>
                                            <div class="patch-result-badge">${matchRate.toFixed(0)}% MATCH</div>
                                        </div>
                                        <div class="patch-result-body">
                                            <strong>Results:</strong><br>
                                            • Matched: ${correlation.matched?.length || 0} routine(s)<br>
                                            • Missing: ${correlation.missing?.length || 0} routine(s)<br>
                                            • Unmatched changes: ${correlation.unmatched?.length || 0}<br>
                                            ${validation.warnings?.length > 0 ?
                                                '<div style="margin-top:8px;color:#ffb86c;">⚠ ' + validation.warnings[0] + '</div>' : ''}
                                            ${validation.errors?.length > 0 ?
                                                '<div style="margin-top:8px;color:#ff5555;">✗ ' + validation.errors[0] + '</div>' : ''}
                                            ${isValid ?
                                                '<div style="margin-top:8px;color:#50fa7b;">✓ Ready to commit</div>' : ''}
                                        </div>
                                    </div>
                                `;
                            }

                            showToast(`Correlation: ${matchRate.toFixed(0)}% match`);

                            setTimeout(() => {
                                setActiveStep(4);
                            }, 1500);

                        } catch (error) {
                            console.error('[Patch Tracking] Correlation error:', error);
                            showToast(`Correlation failed: ${error.message}`);

                            const resultDiv = document.getElementById('correlationResult');
                            if (resultDiv) {
                                resultDiv.innerHTML = `
                                    <div class="patch-result-item error">
                                        <div class="patch-result-header">
                                            <div class="patch-result-title">Correlation Failed</div>
                                            <div class="patch-result-badge">ERROR</div>
                                        </div>
                                        <div class="patch-result-body">${error.message}</div>
                                    </div>
                                `;
                            }
                        } finally {
                            correlateBtn.disabled = false;
                            correlateBtn.textContent = 'Run Correlation';
                        }
                    });
                }
            }

            function renderCommitStep(container) {
                // Prepare commit first
                if (!currentWorkflow.correlation) {
                    container.innerHTML = `
                        <div style="text-align:center;padding:40px;color:#6272a4;">
                            Please complete correlation step first.
                        </div>
                    `;
                    return;
                }

                // Check if we're in forced mode (manual correlation)
                const isForced = currentWorkflow.correlation.correlation?.matched?.some(m => m.status === 'forced');

                if (isForced) {
                    // Show manual commit instructions for forced mode
                    container.innerHTML = `
                        <div class="patch-form">
                            <div class="patch-result-item warning">
                                <div class="patch-result-header">
                                    <div class="patch-result-title">⚠ Manual Commit Required (Forced Mode)</div>
                                    <div class="patch-result-badge">MANUAL</div>
                                </div>
                                <div class="patch-result-body" style="font-size: 12px; line-height: 1.6;">
                                    <p style="margin: 0 0 12px 0;"><strong>You're in forced mode because no environment changes were detected.</strong></p>

                                    <div style="background: rgba(0,0,0,0.2); padding: 12px; border-radius: 4px; margin-bottom: 12px;">
                                        <div style="font-weight: 600; margin-bottom: 8px;">📋 Manual Steps Required:</div>
                                        <ol style="margin: 0; padding-left: 20px; font-size: 11px;">
                                            <li style="margin-bottom: 6px;">Copy routines from Docker to your Git repo:
                                                <code style="display: block; margin-top: 4px; padding: 4px 6px; background: rgba(0,0,0,0.3); border-radius: 3px; font-size: 10px;">
                                                docker cp &lt;container&gt;:/var/worldvista/.../localr/*.m ~/Desktop/vista-routines/localr/
                                                </code>
                                            </li>
                                            <li style="margin-bottom: 6px;">Or manually copy the specific routines:
                                                <div style="margin-top: 4px; padding: 4px 6px; background: rgba(0,0,0,0.3); border-radius: 3px; font-size: 10px;">
                                                    ${(currentWorkflow.patch?.metadata?.routines || []).slice(0, 5).join(', ')}${(currentWorkflow.patch?.metadata?.routines?.length || 0) > 5 ? '...' : ''}
                                                </div>
                                            </li>
                                            <li style="margin-bottom: 6px;">Add patch markers to the files (recommended for tracking)</li>
                                            <li>Commit manually using Git:
                                                <code style="display: block; margin-top: 4px; padding: 4px 6px; background: rgba(0,0,0,0.3); border-radius: 3px; font-size: 10px;">
                                                cd ~/Desktop/vista-routines<br>
                                                git add localr/*.m<br>
                                                git commit -m "Applied patch ${currentWorkflow.patch?.patchId || 'N/A'}"<br>
                                                git push
                                                </code>
                                            </li>
                                        </ol>
                                    </div>

                                    <div style="background: rgba(139, 233, 253, 0.1); padding: 10px; border-radius: 4px; border: 1px solid rgba(139, 233, 253, 0.3);">
                                        <strong style="color: #8be9fd;">💡 Better Approach:</strong>
                                        <div style="font-size: 11px; margin-top: 6px;">
                                            1. Install the patch in Docker VistA first<br>
                                            2. Re-run the scan (it will detect the changes)<br>
                                            3. Use the normal workflow (auto-commit)
                                        </div>
                                    </div>
                                </div>
                            </div>

                            <div class="patch-actions" style="margin-top: 16px;">
                                <button class="patch-btn patch-btn-secondary" id="backToUploadBtn">
                                    ← Start Over
                                </button>
                            </div>
                        </div>
                    `;

                    // Wire up back button
                    setTimeout(() => {
                        const backBtn = document.getElementById('backToUploadBtn');
                        if (backBtn) {
                            backBtn.addEventListener('click', () => {
                                // Reset workflow
                                currentWorkflow = { patch: null, scan: null, correlation: null };
                                setActiveStep(1);
                                showToast('Workflow reset - start with a new patch');
                            });
                        }
                    }, 100);

                    return;
                }

                container.innerHTML = `
                    <div class="patch-form">
                        <div class="patch-form-group">
                            <label class="patch-form-label">Preparing Commit...</label>
                            <div id="commitPreview" style="padding:20px;text-align:center;color:#6272a4;">
                                Loading commit preview...
                            </div>
                        </div>
                        <div class="patch-actions" id="commitActions" style="display:none;">
                            <button class="patch-btn patch-btn-secondary" id="cancelCommitBtn">
                                Cancel
                            </button>
                            <button class="patch-btn patch-btn-primary" id="approveCommitBtn">
                                Approve & Execute Commit
                            </button>
                        </div>
                        <div id="commitResult"></div>
                    </div>
                `;

                // Prepare commit automatically
                (async () => {
                    try {
                        showToast('Preparing commit...');

                        const prepareResult = await window.ahmadIDE.patchTracking.prepareCommit(
                            currentWorkflow.patch.patchId,
                            currentWorkflow.correlation
                        );

                        if (!prepareResult.success) {
                            throw new Error(prepareResult.error || 'Failed to prepare commit');
                        }

                        console.log('[Patch Tracking] Commit prepared:', prepareResult);

                        // Show commit preview
                        const previewDiv = document.getElementById('commitPreview');
                        if (previewDiv && prepareResult.preview) {
                            previewDiv.innerHTML = `<pre style="text-align:left;white-space:pre-wrap;font-size:12px;line-height:1.6;">${prepareResult.preview}</pre>`;
                        }

                        // Show action buttons
                        document.getElementById('commitActions').style.display = 'flex';

                    } catch (error) {
                        console.error('[Patch Tracking] Prepare commit error:', error);
                        const previewDiv = document.getElementById('commitPreview');
                        if (previewDiv) {
                            previewDiv.innerHTML = `<div style="color:#ff5555;">Failed to prepare commit: ${error.message}</div>`;
                        }
                    }
                })();

                // Wire up buttons after DOM is ready
                setTimeout(() => {
                    const approveBtn = document.getElementById('approveCommitBtn');
                    const cancelBtn = document.getElementById('cancelCommitBtn');

                    if (approveBtn) {
                        approveBtn.addEventListener('click', async () => {
                            const confirmed = confirm('⚠️ This will commit and push changes to Git.\n\nAre you sure you want to proceed?');
                            if (!confirmed) return;

                            approveBtn.disabled = true;
                            approveBtn.textContent = 'Committing...';

                            try {
                                showToast('Executing Git commit...');

                                const executeResult = await window.ahmadIDE.patchTracking.executeCommit();

                                if (!executeResult.success) {
                                    throw new Error(executeResult.error || 'Commit execution failed');
                                }

                                console.log('[Patch Tracking] Commit executed:', executeResult);

                                const resultDiv = document.getElementById('commitResult');
                                if (resultDiv) {
                                    resultDiv.innerHTML = `
                                        <div class="patch-result-item success">
                                            <div class="patch-result-header">
                                                <div class="patch-result-title">✓ Commit Successful</div>
                                                <div class="patch-result-badge">COMMITTED</div>
                                            </div>
                                            <div class="patch-result-body">
                                                <strong>Patch committed and pushed to GitLab!</strong><br>
                                                <div style="margin-top:8px;font-size:11px;color:#50fa7b;">
                                                    ${executeResult.result?.commitOutput || 'Commit completed successfully'}
                                                </div>
                                            </div>
                                        </div>
                                    `;
                                }

                                showToast('✓ Patch committed successfully!');

                                // Reset workflow after delay
                                setTimeout(() => {
                                    currentWorkflow = { step: 1, patch: null, scan: null, correlation: null };
                                    setActiveStep(1);
                                    refreshStatistics();
                                }, 3000);

                            } catch (error) {
                                console.error('[Patch Tracking] Commit execution error:', error);
                                showToast(`Commit failed: ${error.message}`);

                                const resultDiv = document.getElementById('commitResult');
                                if (resultDiv) {
                                    resultDiv.innerHTML = `
                                        <div class="patch-result-item error">
                                            <div class="patch-result-header">
                                                <div class="patch-result-title">Commit Failed</div>
                                                <div class="patch-result-badge">ERROR</div>
                                            </div>
                                            <div class="patch-result-body">
                                                ${error.message}
                                            </div>
                                        </div>
                                    `;
                                }

                                approveBtn.disabled = false;
                                approveBtn.textContent = 'Approve & Execute Commit';
                            }
                        });
                    }

                    if (cancelBtn) {
                        cancelBtn.addEventListener('click', async () => {
                            if (confirm('Cancel this commit workflow?')) {
                                try {
                                    await window.ahmadIDE.patchTracking.cancelCommit();
                                    currentWorkflow = { step: 1, patch: null, scan: null, correlation: null };
                                    setActiveStep(1);
                                    showToast('Workflow cancelled');
                                } catch (error) {
                                    console.error('[Patch Tracking] Cancel error:', error);
                                }
                            }
                        });
                    }
                }, 100);
            }

            function handleStepClick(stepNumber) {
                // Validate step transitions
                if (stepNumber === 3 || stepNumber === 4) {
                    // Steps 3 & 4 require either successful scan OR forced mode
                    const scanHasChanges = currentWorkflow.scan?.changeResult?.summary?.modified > 0 ||
                                          currentWorkflow.scan?.changeResult?.summary?.added > 0;

                    if (!scanHasChanges && !currentWorkflow.forcedMode) {
                        showToast('⚠ No changes detected - Use "Force Proceed" button in scan step first');
                        setActiveStep(2); // Go back to scan step
                        return;
                    }
                }

                // Only allow clicking on completed steps or current step
                if (stepNumber <= currentWorkflow.step) {
                    setActiveStep(stepNumber);
                }
            }

            async function refreshStatistics() {
                try {
                    const result = await window.ahmadIDE.patchTracking.getStatistics();

                    if (result.success && result.stats) {
                        const stats = result.stats;

                        // Safe updates with null checks
                        const statPatches = document.getElementById('statPatches');
                        const statPending = document.getElementById('statPending');
                        const statCommitted = document.getElementById('statCommitted');

                        if (statPatches) statPatches.textContent = stats.totalPatches || 0;
                        if (statPending) statPending.textContent = stats.pendingPatches || 0;
                        if (statCommitted) statCommitted.textContent = stats.committedPatches || 0;
                    }
                } catch (error) {
                    console.error('[Patch Tracking] Stats refresh error:', error);
                }
            }

            // Lazy-mount support
            if (featureRegistry && typeof featureRegistry.onMounted === 'function') {
                featureRegistry.onMounted('patchTrackingPanel', () => {
                    wirePanelDom();
                    const panel = document.getElementById('patchTrackingPanel');
                    if (panel && !panel.classList.contains('hidden')) {
                        initializePanel().catch(() => {});
                    }
                });
            } else {
                // Fallback: wire immediately
                wirePanelDom();
            }

            return { ensurePanelMounted, setActiveStep, refreshStatistics };
        }

        return { wirePatchTrackingPanel };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.patchTracking = window.AhmadIDEModules.patchTracking || {};
        window.AhmadIDEModules.patchTracking.createPatchTrackingManager = createPatchTrackingManager;
    }
})();

// Auto-initialize
if (typeof window !== 'undefined') {
    console.log('[Patch Tracking] Setting up DOMContentLoaded listener...');

    document.addEventListener('DOMContentLoaded', () => {
        console.log('[Patch Tracking] DOMContentLoaded fired');
        const manager = window.AhmadIDEModules?.patchTracking?.createPatchTrackingManager?.();
        if (manager) {
            console.log('[Patch Tracking] ✓ Manager created, wiring panel...');
            manager.wirePatchTrackingPanel();
        } else {
            console.error('[Patch Tracking] ❌ Failed to create manager!');
        }
    });

    console.log('[Patch Tracking] ✓ Renderer script loaded successfully');
}
