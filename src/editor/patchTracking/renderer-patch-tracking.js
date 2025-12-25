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
                const minimizeBtn = document.getElementById('patchTrackingMinimizeBtn');

                if (refreshBtn) {
                    refreshBtn.addEventListener('click', () => refreshStatistics());
                }

                if (minimizeBtn) {
                    minimizeBtn.addEventListener('click', () => {
                        const toolWindowPanel = panel.closest('.tool-window-panel');
                        if (toolWindowPanel) {
                            toolWindowPanel.classList.add('hidden');
                        }
                    });
                }

                // Initialize panel
                initializePanel();
            }

            async function initializePanel() {
                console.log('[Patch Tracking] Initializing panel');

                try {
                    // Wire up repo path settings
                    wireRepoPathSettings();

                    // Load initial statistics
                    await refreshStatistics();

                    // Set initial workflow step
                    setActiveStep(1);

                    console.log('[Patch Tracking] Panel initialized successfully');
                } catch (error) {
                    console.error('[Patch Tracking] Initialization error:', error);
                    showToast(`Failed to initialize patch tracking: ${error.message}`);
                }
            }

            function wireRepoPathSettings() {
                const setRepoPathBtn = document.getElementById('setRepoPathBtn');
                const browseRepoBtn = document.getElementById('browseRepoBtn');
                const gitRepoPathInput = document.getElementById('gitRepoPathInput');
                const repoPathStatus = document.getElementById('repoPathStatus');

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
                        <div class="patch-form-group">
                            <label class="patch-form-label">Upload KIDS Patch File</label>
                            <div style="display: flex; gap: 10px; align-items: center;">
                                <input type="text" class="patch-form-input" id="patchFilePath"
                                    placeholder="Select a KIDS file..." readonly
                                    style="flex: 1;">
                                <input type="file" id="patchFileInput" accept=".KID,.KIDS,.kid,.kids,*" style="display: none;">
                                <button class="patch-btn patch-btn-primary" id="selectPatchBtn">
                                    Browse...
                                </button>
                            </div>
                        </div>
                        <div class="patch-actions">
                            <button class="patch-btn patch-btn-primary" id="uploadPatchBtn" disabled>
                                Upload and Parse
                            </button>
                        </div>
                        <div id="patchUploadResult"></div>
                    </div>
                `;

                // Wire up browse button
                const selectBtn = document.getElementById('selectPatchBtn');
                const fileInput = document.getElementById('patchFileInput');
                const filePathInput = document.getElementById('patchFilePath');
                const uploadBtn = document.getElementById('uploadPatchBtn');

                if (selectBtn && fileInput && filePathInput && uploadBtn) {
                    // Trigger file input when browse button is clicked
                    selectBtn.addEventListener('click', () => {
                        fileInput.click();
                    });

                    // Handle file selection
                    fileInput.addEventListener('change', (e) => {
                        const file = e.target.files?.[0];
                        if (file) {
                            filePathInput.value = file.name;
                            fileInput.dataset.file = file.name;
                            uploadBtn.disabled = false;
                        }
                    });

                    uploadBtn.addEventListener('click', async () => {
                        const file = fileInput.files?.[0];
                        if (!file) return;

                        try {
                            uploadBtn.disabled = true;
                            uploadBtn.textContent = 'Parsing...';

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
                                            <div class="patch-result-title">Patch Parsed Successfully</div>
                                            <div class="patch-result-badge">✓ READY</div>
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
                                            <div class="patch-result-title">Upload Failed</div>
                                            <div class="patch-result-badge">ERROR</div>
                                        </div>
                                        <div class="patch-result-body">
                                            ${error.message}
                                        </div>
                                    </div>
                                `;
                            }
                        } finally {
                            uploadBtn.disabled = false;
                            uploadBtn.textContent = 'Upload and Parse';
                        }
                    });
                }
            }

            function renderScanStep(container) {
                if (!currentWorkflow.patch) {
                    container.innerHTML = `
                        <div style="text-align:center;padding:40px;color:#6272a4;">
                            Please upload a patch first.
                        </div>
                    `;
                    return;
                }

                container.innerHTML = `
                    <div class="patch-form">
                        <div class="patch-form-group">
                            <label class="patch-form-label">Patch Info</label>
                            <div style="padding:12px;background:#282a36;border-radius:4px;font-size:12px;">
                                <div><strong>Patch:</strong> ${currentWorkflow.patch.metadata.patchId}</div>
                                <div style="margin-top:4px;"><strong>Routines:</strong> ${currentWorkflow.patch.metadata.routines.length} routine(s)</div>
                                <div style="margin-top:4px;color:#50fa7b;font-size:11px;">${currentWorkflow.patch.metadata.routines.join(', ')}</div>
                            </div>
                        </div>
                        <div class="patch-form-group">
                            <label class="patch-form-label">Connection</label>
                            <div id="connectionInfo" style="padding:12px;background:#282a36;border-radius:4px;font-size:12px;color:#6272a4;">
                                Detecting current connection...
                            </div>
                        </div>
                        <div class="patch-actions">
                            <button class="patch-btn patch-btn-primary" id="startScanBtn">
                                Start Scan
                            </button>
                        </div>
                        <div id="scanResult"></div>
                    </div>
                `;

                // Detect current connection
                (async () => {
                    try {
                        const conn = await window.ahmadIDE.getConnection();
                        const connInfo = document.getElementById('connectionInfo');
                        if (connInfo && conn.ok) {
                            connInfo.innerHTML = `
                                <div><strong>Type:</strong> ${conn.type}</div>
                                <div style="margin-top:4px;"><strong>Container/Host:</strong> ${conn.connectionId || 'SSH'}</div>
                                <div style="margin-top:4px;"><strong>Env:</strong> ${conn.paths.envKey}</div>
                                <div style="margin-top:4px;color:#50fa7b;font-size:11px;">Will scan: ${conn.paths.localrPath} & ${conn.paths.routinesPath}</div>
                            `;
                            connInfo.style.color = '#f8f8f2';
                        }
                    } catch (error) {
                        console.error('[Patch Tracking] Connection detection error:', error);
                        const connInfo = document.getElementById('connectionInfo');
                        if (connInfo) {
                            connInfo.innerHTML = `<span style="color:#ff5555;">Failed to detect connection</span>`;
                        }
                    }
                })();

                const startScanBtn = document.getElementById('startScanBtn');
                if (startScanBtn) {
                    startScanBtn.addEventListener('click', async () => {

                        startScanBtn.disabled = true;
                        startScanBtn.textContent = 'Scanning...';

                        try {
                            // Get current IDE connection
                            const conn = await window.ahmadIDE.getConnection();
                            if (!conn.ok) {
                                throw new Error('No active connection found');
                            }

                            const connectionId = conn.connectionId;
                            const localrPath = conn.paths.localrPath;
                            const routinesPath = conn.paths.routinesPath;

                            console.log('[Patch Tracking] About to scan with patchId:', currentWorkflow.patch.patchId);
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
                            const resultDiv = document.getElementById('scanResult');
                            if (resultDiv) {
                                resultDiv.innerHTML = `
                                    <div class="patch-result-item success">
                                        <div class="patch-result-header">
                                            <div class="patch-result-title">Scan Complete</div>
                                            <div class="patch-result-badge">✓ DONE</div>
                                        </div>
                                        <div class="patch-result-body">
                                            <strong>Changes detected:</strong><br>
                                            • Modified: ${summary.modified || 0} routine(s)<br>
                                            • Added: ${summary.added || 0} routine(s)<br>
                                            • Removed: ${summary.removed || 0} routine(s)<br>
                                            <div style="margin-top:8px;font-size:11px;color:#50fa7b;">
                                                Change ID: ${scanResult.changeId}
                                            </div>
                                        </div>
                                    </div>
                                `;
                            }

                            showToast(`Scan complete: ${summary.modified || 0} modified, ${summary.added || 0} added`);

                            setTimeout(() => {
                                setActiveStep(3);
                            }, 1500);

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
                        if (!currentWorkflow.patch || !currentWorkflow.scan) {
                            showToast('Please complete upload and scan steps first');
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
                        document.getElementById('statPatches').textContent = stats.totalPatches || 0;
                        document.getElementById('statChanges').textContent = stats.totalChanges || 0;
                        document.getElementById('statPending').textContent = stats.pendingPatches || 0;
                        document.getElementById('statCommitted').textContent = stats.committedPatches || 0;
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
