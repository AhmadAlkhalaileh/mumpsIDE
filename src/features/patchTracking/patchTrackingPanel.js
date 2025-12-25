/**
 * Patch Tracking Panel
 * UI for Vista patch tracking system
 */

const patchTrackerService = require('../../services/patchTracking/patchTrackerService');
const { dialog } = require('electron').remote || require('@electron/remote');
const path = require('path');

class PatchTrackingPanel {
    constructor() {
        this.panel = null;
        this.currentPatch = null;
        this.currentScan = null;
        this.currentCorrelation = null;
    }

    /**
     * Initialize and render the panel
     * @param {HTMLElement} container - Container element
     */
    async init(container) {
        console.log('[Patch Tracking Panel] Initializing');

        this.panel = container;

        // Initialize service
        await patchTrackerService.init();

        // Render initial UI
        await this.render();

        // Load statistics
        await this.refreshStatistics();

        console.log('[Patch Tracking Panel] Initialized');
    }

    /**
     * Render the panel UI
     */
    async render() {
        if (!this.panel) return;

        this.panel.innerHTML = `
            <div id="patchTrackingContainer" style="padding: 20px; background: var(--ps-bg-main, #282a36); color: var(--ps-text-main, #f8f8f2); height: 100%; overflow-y: auto;">

                <!-- Header -->
                <div style="display: flex; align-items: center; justify-content: space-between; margin-bottom: 24px; padding-bottom: 16px; border-bottom: 2px solid rgba(139, 233, 253, 0.3);">
                    <div>
                        <h2 style="margin: 0; font-size: 24px; color: #8be9fd;">
                            <svg width="28" height="28" fill="currentColor" style="vertical-align: middle; margin-right: 10px;">
                                <path d="M4 4h7v7H4zM13 4h7v7h-7zM4 13h7v7H4zM13 13h7v7h-7z" opacity="0.5"/>
                                <path d="M9 9l3-3M9 15l3 3M15 9l3 3" stroke="currentColor" fill="none" stroke-width="2"/>
                            </svg>
                            Vista Patch Tracking
                        </h2>
                        <p style="margin: 8px 0 0 38px; color: var(--ps-text-muted, #6272a4); font-size: 13px;">
                            Intelligent patch detection and Git integration
                        </p>
                    </div>
                    <div id="patchStats" style="text-align: right; font-size: 12px; color: var(--ps-text-muted, #6272a4);"></div>
                </div>

                <!-- Workflow Steps -->
                <div id="workflowSteps" style="display: grid; grid-template-columns: repeat(4, 1fr); gap: 12px; margin-bottom: 24px;">
                    <!-- Step 1: Upload Patch -->
                    <div class="workflow-step" data-step="1" style="background: rgba(139, 233, 253, 0.1); border: 1px solid rgba(139, 233, 253, 0.3); border-radius: 8px; padding: 16px; cursor: pointer; transition: all 0.2s;">
                        <div style="font-size: 24px; margin-bottom: 8px;">📦</div>
                        <div style="font-weight: 600; margin-bottom: 4px;">1. Upload Patch</div>
                        <div style="font-size: 11px; color: var(--ps-text-muted, #6272a4);">Upload KIDS file</div>
                    </div>

                    <!-- Step 2: Scan Environment -->
                    <div class="workflow-step" data-step="2" style="background: rgba(139, 233, 253, 0.1); border: 1px solid rgba(139, 233, 253, 0.3); border-radius: 8px; padding: 16px; cursor: pointer; transition: all 0.2s;">
                        <div style="font-size: 24px; margin-bottom: 8px;">🔍</div>
                        <div style="font-weight: 600; margin-bottom: 4px;">2. Scan Docker</div>
                        <div style="font-size: 11px; color: var(--ps-text-muted, #6272a4);">Detect changes</div>
                    </div>

                    <!-- Step 3: Correlate -->
                    <div class="workflow-step" data-step="3" style="background: rgba(139, 233, 253, 0.1); border: 1px solid rgba(139, 233, 253, 0.3); border-radius: 8px; padding: 16px; cursor: pointer; transition: all 0.2s;">
                        <div style="font-size: 24px; margin-bottom: 8px;">🔗</div>
                        <div style="font-weight: 600; margin-bottom: 4px;">3. Correlate</div>
                        <div style="font-size: 11px; color: var(--ps-text-muted, #6272a4);">Match patch to changes</div>
                    </div>

                    <!-- Step 4: Commit -->
                    <div class="workflow-step" data-step="4" style="background: rgba(139, 233, 253, 0.1); border: 1px solid rgba(139, 233, 253, 0.3); border-radius: 8px; padding: 16px; cursor: pointer; transition: all 0.2s;">
                        <div style="font-size: 24px; margin-bottom: 8px;">✅</div>
                        <div style="font-weight: 600; margin-bottom: 4px;">4. Approve & Commit</div>
                        <div style="font-size: 11px; color: var(--ps-text-muted, #6272a4);">Review and push to Git</div>
                    </div>
                </div>

                <!-- Main Content Area -->
                <div id="mainContent" style="background: rgba(40, 42, 54, 0.5); border-radius: 8px; padding: 24px; min-height: 400px;">
                    <div id="contentArea">
                        <div style="text-align: center; padding: 60px 20px; color: var(--ps-text-muted, #6272a4);">
                            <svg width="80" height="80" fill="none" stroke="currentColor" stroke-width="1.5" style="opacity: 0.3; margin-bottom: 20px;">
                                <rect x="20" y="20" width="40" height="40" rx="4"/>
                                <path d="M30 35h20M30 40h20M30 45h12"/>
                            </svg>
                            <p style="font-size: 16px; margin: 0;">Select a workflow step to get started</p>
                            <p style="font-size: 13px; margin: 8px 0 0;">Upload a KIDS patch file or scan an environment</p>
                        </div>
                    </div>
                </div>

                <!-- Action Buttons -->
                <div id="actionButtons" style="margin-top: 20px; display: flex; gap: 12px; justify-content: flex-end;">
                    <!-- Buttons will be added dynamically -->
                </div>

            </div>
        `;

        // Wire up event listeners
        this.wireEvents();
    }

    /**
     * Wire up event listeners
     */
    wireEvents() {
        // Workflow step clicks
        const steps = this.panel.querySelectorAll('.workflow-step');
        steps.forEach(step => {
            step.addEventListener('click', () => {
                const stepNumber = parseInt(step.dataset.step);
                this.showStep(stepNumber);
            });

            step.addEventListener('mouseenter', () => {
                step.style.transform = 'translateY(-2px)';
                step.style.boxShadow = '0 4px 12px rgba(139, 233, 253, 0.3)';
            });

            step.addEventListener('mouseleave', () => {
                step.style.transform = 'translateY(0)';
                step.style.boxShadow = 'none';
            });
        });
    }

    /**
     * Show specific workflow step
     * @param {number} stepNumber - Step number (1-4)
     */
    async showStep(stepNumber) {
        const contentArea = this.panel.querySelector('#contentArea');
        const actionButtons = this.panel.querySelector('#actionButtons');

        // Highlight active step
        this.panel.querySelectorAll('.workflow-step').forEach(step => {
            if (parseInt(step.dataset.step) === stepNumber) {
                step.style.background = 'rgba(139, 233, 253, 0.25)';
                step.style.borderColor = 'rgba(139, 233, 253, 0.6)';
            } else {
                step.style.background = 'rgba(139, 233, 253, 0.1)';
                step.style.borderColor = 'rgba(139, 233, 253, 0.3)';
            }
        });

        switch (stepNumber) {
            case 1:
                await this.renderUploadStep(contentArea, actionButtons);
                break;
            case 2:
                await this.renderScanStep(contentArea, actionButtons);
                break;
            case 3:
                await this.renderCorrelateStep(contentArea, actionButtons);
                break;
            case 4:
                await this.renderCommitStep(contentArea, actionButtons);
                break;
        }
    }

    /**
     * Render Step 1: Upload Patch
     */
    async renderUploadStep(contentArea, actionButtons) {
        contentArea.innerHTML = `
            <div>
                <h3 style="color: #8be9fd; margin-top: 0;">Upload KIDS Patch File</h3>
                <p style="color: var(--ps-text-muted, #6272a4); margin-bottom: 20px;">
                    Select a Vista KIDS distribution file (.KID) to parse and track
                </p>

                <div id="uploadArea" style="border: 2px dashed rgba(139, 233, 253, 0.3); border-radius: 8px; padding: 40px; text-align: center; background: rgba(40, 42, 54, 0.3);">
                    <svg width="64" height="64" fill="none" stroke="rgba(139, 233, 253, 0.5)" stroke-width="2" style="margin-bottom: 16px;">
                        <path d="M32 16v32M16 32h32"/>
                        <rect x="12" y="12" width="40" height="40" rx="4"/>
                    </svg>
                    <p style="font-size: 16px; margin: 0 0 8px;">Click to select KIDS file</p>
                    <p style="font-size: 13px; color: var(--ps-text-muted, #6272a4); margin: 0;">Or drag and drop here</p>
                </div>

                <div id="uploadedPatch" style="margin-top: 20px; display: none;"></div>
            </div>
        `;

        actionButtons.innerHTML = `
            <button id="selectPatchBtn" class="ps-btn ps-btn-primary">
                📂 Select KIDS File
            </button>
        `;

        // Event listeners
        this.panel.querySelector('#selectPatchBtn').addEventListener('click', () => this.selectKIDSFile());
        this.panel.querySelector('#uploadArea').addEventListener('click', () => this.selectKIDSFile());
    }

    /**
     * Render Step 2: Scan Environment
     */
    async renderScanStep(contentArea, actionButtons) {
        contentArea.innerHTML = `
            <div>
                <h3 style="color: #8be9fd; margin-top: 0;">Scan Docker Environment</h3>
                <p style="color: var(--ps-text-muted, #6272a4); margin-bottom: 20px;">
                    Connect to Docker container and scan for changes between localr and routines folders
                </p>

                <div id="scanConfig" style="background: rgba(40, 42, 54, 0.3); border-radius: 8px; padding: 16px; margin-bottom: 16px;">
                    <div style="margin-bottom: 12px;">
                        <label style="display: block; margin-bottom: 4px; font-size: 13px;">Environment Name:</label>
                        <input type="text" id="envName" value="docker" style="width: 100%; padding: 8px; background: var(--ps-bg-secondary, #2f3141); border: 1px solid var(--ps-border, #44475a); border-radius: 4px; color: var(--ps-text-main, #f8f8f2);">
                    </div>
                    <div style="margin-bottom: 12px;">
                        <label style="display: block; margin-bottom: 4px; font-size: 13px;">Localr Path:</label>
                        <input type="text" id="localrPath" value="/var/worldvista/prod/hakeem/localr" style="width: 100%; padding: 8px; background: var(--ps-bg-secondary, #2f3141); border: 1px solid var(--ps-border, #44475a); border-radius: 4px; color: var(--ps-text-main, #f8f8f2);">
                    </div>
                    <div style="margin-bottom: 0;">
                        <label style="display: block; margin-bottom: 4px; font-size: 13px;">Routines Path:</label>
                        <input type="text" id="routinesPath" value="/var/worldvista/prod/hakeem/routines" style="width: 100%; padding: 8px; background: var(--ps-bg-secondary, #2f3141); border: 1px solid var(--ps-border, #44475a); border-radius: 4px; color: var(--ps-text-main, #f8f8f2);">
                    </div>
                </div>

                <div id="scanResults" style="display: none;"></div>
            </div>
        `;

        actionButtons.innerHTML = `
            <button id="startScanBtn" class="ps-btn ps-btn-primary">
                🔍 Start Scan
            </button>
        `;

        this.panel.querySelector('#startScanBtn').addEventListener('click', () => this.startScan());
    }

    /**
     * Render Step 3: Correlate
     */
    async renderCorrelateStep(contentArea, actionButtons) {
        if (!this.currentPatch || !this.currentScan) {
            contentArea.innerHTML = `
                <div style="text-align: center; padding: 40px; color: var(--ps-text-muted, #6272a4);">
                    <p>Please complete Steps 1 and 2 first</p>
                </div>
            `;
            actionButtons.innerHTML = '';
            return;
        }

        contentArea.innerHTML = `
            <div>
                <h3 style="color: #8be9fd; margin-top: 0;">Correlate Patch with Changes</h3>
                <p style="color: var(--ps-text-muted, #6272a4); margin-bottom: 20px;">
                    Matching patch "${this.currentPatch.patchId}" with detected changes
                </p>
                <div id="correlationResults"></div>
            </div>
        `;

        actionButtons.innerHTML = `
            <button id="correlateBtn" class="ps-btn ps-btn-primary">
                🔗 Correlate Now
            </button>
        `;

        this.panel.querySelector('#correlateBtn').addEventListener('click', () => this.correlate());
    }

    /**
     * Render Step 4: Approve & Commit
     */
    async renderCommitStep(contentArea, actionButtons) {
        if (!this.currentCorrelation) {
            contentArea.innerHTML = `
                <div style="text-align: center; padding: 40px; color: var(--ps-text-muted, #6272a4);">
                    <p>Please complete correlation first</p>
                </div>
            `;
            actionButtons.innerHTML = '';
            return;
        }

        const prepared = await patchTrackerService.prepareCommit(
            this.currentPatch.patchId,
            this.currentCorrelation
        );

        contentArea.innerHTML = `
            <div>
                <h3 style="color: #8be9fd; margin-top: 0;">Review & Approve Commit</h3>
                <div style="background: rgba(40, 42, 54, 0.5); border-radius: 8px; padding: 16px; font-family: monospace; font-size: 12px; white-space: pre-wrap; max-height: 500px; overflow-y: auto;">
${prepared.preview}
                </div>
            </div>
        `;

        actionButtons.innerHTML = `
            <button id="cancelBtn" class="ps-btn ps-btn-ghost">
                ❌ Cancel
            </button>
            <button id="approveBtn" class="ps-btn ps-btn-primary">
                ✅ Approve & Commit
            </button>
        `;

        this.panel.querySelector('#cancelBtn').addEventListener('click', () => this.cancelCommit());
        this.panel.querySelector('#approveBtn').addEventListener('click', () => this.approveCommit());
    }

    /**
     * Select KIDS file
     */
    async selectKIDSFile() {
        const result = await dialog.showOpenDialog({
            title: 'Select KIDS Patch File',
            filters: [
                { name: 'KIDS Files', extensions: ['KID', 'kid'] },
                { name: 'All Files', extensions: ['*'] }
            ],
            properties: ['openFile']
        });

        if (result.canceled || !result.filePaths || result.filePaths.length === 0) {
            return;
        }

        const filePath = result.filePaths[0];

        try {
            const uploadArea = this.panel.querySelector('#uploadArea');
            const uploadedPatch = this.panel.querySelector('#uploadedPatch');

            uploadArea.innerHTML = '<p>Parsing KIDS file...</p>';

            const result = await patchTrackerService.uploadPatch(filePath);

            this.currentPatch = result.metadata;

            uploadedPatch.style.display = 'block';
            uploadedPatch.innerHTML = `
                <div style="background: rgba(80, 250, 123, 0.1); border: 1px solid rgba(80, 250, 123, 0.3); border-radius: 8px; padding: 16px;">
                    <div style="font-weight: 600; color: #50fa7b; margin-bottom: 8px;">✓ Patch Uploaded Successfully</div>
                    <div style="font-size: 13px;">
                        <div><strong>Patch ID:</strong> ${result.metadata.patchId}</div>
                        <div><strong>Title:</strong> ${result.metadata.title || 'N/A'}</div>
                        <div><strong>Author:</strong> ${result.metadata.author || 'N/A'}</div>
                        <div><strong>Routines:</strong> ${result.metadata.routines.join(', ')}</div>
                    </div>
                </div>
            `;

            uploadArea.innerHTML = `
                <p style="color: #50fa7b;">✓ ${path.basename(filePath)}</p>
            `;

            await this.refreshStatistics();

        } catch (error) {
            alert(`Failed to upload patch: ${error.message}`);
        }
    }

    /**
     * Start environment scan
     */
    async startScan() {
        // TODO: Get active SSH connection ID
        // For now, show error
        alert('Please connect to Docker environment first via SSH');
        // In real implementation, get connectionId from active SSH connection
    }

    /**
     * Correlate patch with changes
     */
    async correlate() {
        try {
            const result = await patchTrackerService.correlatePatchWithChanges(
                this.currentPatch.patchId,
                this.currentScan.changeId
            );

            this.currentCorrelation = result;

            // Show correlation results
            const resultsDiv = this.panel.querySelector('#correlationResults');
            resultsDiv.innerHTML = `
                <div style="background: rgba(80, 250, 123, 0.1); border: 1px solid rgba(80, 250, 123, 0.3); border-radius: 8px; padding: 16px;">
                    <div style="font-weight: 600; color: #50fa7b; margin-bottom: 12px;">Correlation Complete</div>
                    <div style="font-size: 13px;">
                        <div>Match Rate: ${result.correlation.matchRate.toFixed(1)}%</div>
                        <div>Matched: ${result.correlation.summary.matchedChanges} routines</div>
                        <div>Can Commit: ${result.validation.canCommit ? '✓ Yes' : '✗ No'}</div>
                    </div>
                </div>
            `;

        } catch (error) {
            alert(`Correlation failed: ${error.message}`);
        }
    }

    /**
     * Approve and execute commit
     */
    async approveCommit() {
        if (!confirm('Are you sure you want to commit these changes to GitLab?')) {
            return;
        }

        try {
            const result = await patchTrackerService.executeCommit();
            alert('Commit executed successfully!');
            await this.refreshStatistics();
            this.showStep(1); // Return to step 1

        } catch (error) {
            alert(`Commit failed: ${error.message}`);
        }
    }

    /**
     * Cancel commit
     */
    async cancelCommit() {
        await patchTrackerService.cancelCommit();
        this.showStep(3); // Return to correlation step
    }

    /**
     * Refresh statistics display
     */
    async refreshStatistics() {
        const stats = await patchTrackerService.getStatistics();
        const statsDiv = this.panel.querySelector('#patchStats');
        if (statsDiv) {
            statsDiv.innerHTML = `
                Patches: ${stats.totalPatches} total, ${stats.pendingPatches} pending, ${stats.committedPatches} committed<br>
                Changes: ${stats.totalChanges} total, ${stats.pendingChanges} pending
            `;
        }
    }
}

// Export singleton instance
module.exports = new PatchTrackingPanel();
