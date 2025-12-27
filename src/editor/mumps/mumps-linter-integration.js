/**
 * MUMPS Enhanced Linter Integration
 *
 * Wires the enhanced linter with:
 * - Quick Fix provider
 * - GOTO Map visualization
 * - Toolbar/menu commands
 */

(() => {
    'use strict';

    // Store GOTO analysis data per model
    const gotoAnalysisCache = new WeakMap();

    /**
     * Initialize enhanced linter features
     */
    function initMumpsLinterEnhancements({ monacoRef, getActiveEditor, showToast }) {
        if (!monacoRef) {
            console.error('[Linter Integration] Monaco not available');
            return;
        }

        console.log('[Linter Integration] Initializing enhanced linter features...');

        // Initialize Quick Fix provider (auto-registers)
        const QuickFixProvider = window.AhmadIDEModules?.mumps?.MumpsQuickFixProvider;
        if (QuickFixProvider) {
            const quickFixProvider = new QuickFixProvider(monacoRef);
            quickFixProvider.register();
            console.log('[Linter Integration] ✓ Quick Fix provider registered');
        } else {
            console.warn('[Linter Integration] Quick Fix provider not found');
        }

        // Initialize GOTO Map panel
        const GotoMapPanel = window.AhmadIDEModules?.mumps?.MumpsGotoMapPanel;
        let gotoMapPanel = null;

        if (GotoMapPanel) {
            gotoMapPanel = new GotoMapPanel({
                monacoRef,
                getActiveEditor,
                showToast: showToast || (() => {})
            });
            console.log('[Linter Integration] ✓ GOTO Map panel created');
        } else {
            console.warn('[Linter Integration] GOTO Map panel not found');
        }

        // Store GOTO analysis when linting completes
        if (typeof window._originalValidateMumps !== 'function') {
            // Hook into diagnostics to capture GOTO analysis
            const captureGotoAnalysis = () => {
                const originalLinter = window._mumpsLinter || window.MUMPSLinter;
                if (!originalLinter || !originalLinter.prototype || !originalLinter.prototype.lint) {
                    return;
                }

                // The enhanced linter already returns gotoAnalysis in the result
                // We just need to capture it when diagnostics run
                console.log('[Linter Integration] Enhanced linter detected');
            };

            setTimeout(captureGotoAnalysis, 100);
        }

        // Register command to show GOTO Map
        const registerGotoMapCommand = () => {
            if (!gotoMapPanel) return;

            // Create global function to open GOTO map
            window.showMumpsGotoMap = () => {
                const editor = getActiveEditor();
                if (!editor) {
                    showToast('info', 'GOTO Map', 'No active editor');
                    return;
                }

                const model = editor.getModel();
                if (!model) {
                    showToast('info', 'GOTO Map', 'No model available');
                    return;
                }

                // Get cached analysis or run linter
                let gotoAnalysis = gotoAnalysisCache.get(model);

	                if (!gotoAnalysis) {
	                    // Run linter to get fresh analysis
	                    const linter = window._mumpsLinter || window.MUMPSLinter;
	                    if (!linter) {
	                        showToast('error', 'GOTO Map', 'Linter not available');
	                        return;
	                    }

	                    try {
	                        const text = model.getValue();
	                        let result = null;
	                        if (typeof linter === 'function') {
	                            const linterInstance = new linter();
	                            result = linterInstance.lint(text, { mode: 'edit' });
	                        } else if (typeof linter.lint === 'function') {
	                            result = linter.lint(text, { mode: 'edit' });
	                        }
	                        gotoAnalysis = result?.gotoAnalysis;

	                        if (gotoAnalysis) {
	                            gotoAnalysisCache.set(model, gotoAnalysis);
	                        }
	                    } catch (err) {
                        console.error('[GOTO Map] Error running linter:', err);
                        showToast('error', 'GOTO Map', 'Failed to analyze code');
                        return;
                    }
                }

                if (!gotoAnalysis) {
                    showToast('info', 'GOTO Map', 'No GOTO analysis available');
                    return;
                }

                const lineCount = model.getLineCount();
                gotoMapPanel.show(gotoAnalysis, lineCount);
            };

            console.log('[Linter Integration] ✓ GOTO Map command registered (window.showMumpsGotoMap)');
        };

	        registerGotoMapCommand();

	        // Register Monaco action for GOTO Map
	        if (gotoMapPanel) {
	            try {
	                const editor = (typeof getActiveEditor === 'function') ? getActiveEditor() : null;
	                const keybinding = monacoRef.KeyMod.CtrlCmd | monacoRef.KeyMod.Shift | monacoRef.KeyCode.KeyG;
	                if (editor && typeof editor.addAction === 'function') {
	                    editor.addAction({
	                        id: 'mumps.showGotoMap',
	                        label: 'MUMPS: Show GOTO Flow Map',
	                        keybindings: [keybinding],
	                        run: () => {
	                            if (typeof window.showMumpsGotoMap === 'function') {
	                                window.showMumpsGotoMap();
	                            }
	                        }
	                    });
	                    console.log('[Linter Integration] ✓ GOTO Map action registered (Ctrl+Shift+G)');
	                } else if (editor && typeof editor.addCommand === 'function') {
	                    editor.addCommand(keybinding, () => {
	                        if (typeof window.showMumpsGotoMap === 'function') {
	                            window.showMumpsGotoMap();
	                        }
	                    });
	                    console.log('[Linter Integration] ✓ GOTO Map keybinding registered (Ctrl+Shift+G)');
	                }
	            } catch (err) {
	                console.warn('[Linter Integration] Failed to register GOTO Map keybinding:', err);
	            }
	        }

        // Store GOTO analysis after each lint run
        const storeLintResults = (model, lintResult) => {
            if (lintResult && lintResult.gotoAnalysis) {
                gotoAnalysisCache.set(model, lintResult.gotoAnalysis);
            }
        };

        // Export function for diagnostics to call
        window.storeMumpsLintResults = storeLintResults;

        console.log('[Linter Integration] ✓ All enhancements initialized');

        return {
            gotoMapPanel,
            showGotoMap: window.showMumpsGotoMap,
            storeLintResults
        };
    }

    // Export
    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
        window.AhmadIDEModules.mumps.initMumpsLinterEnhancements = initMumpsLinterEnhancements;
    }
})();
