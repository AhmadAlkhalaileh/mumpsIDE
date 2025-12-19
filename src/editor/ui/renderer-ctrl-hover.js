(() => {
    function createCtrlHoverManager({ deps } = {}) {
        const getMonaco = deps?.getMonaco || (() => (typeof monaco !== 'undefined' ? monaco : null));
        const parseRoutineReferenceAtPosition = deps?.parseRoutineReferenceAtPosition;
        const resolveLocalTagLine = deps?.resolveLocalTagLine || null;
        const goToDeclaration = deps?.goToDeclaration || (async () => false);
        const toggleBreakpoint = deps?.toggleBreakpoint || (() => { });

        if (!parseRoutineReferenceAtPosition || typeof parseRoutineReferenceAtPosition !== 'function') {
            throw new Error('createCtrlHoverManager requires deps.parseRoutineReferenceAtPosition');
        }

        function bindCtrlHoverAndGutter(editor, dbgState) {
            if (!editor) return;
            const monaco = getMonaco();
            if (!monaco) {
                throw new Error('bindCtrlHoverAndGutter requires Monaco');
            }

            // Ctrl+Hover: throttle decoration churn + update only when the hovered range changes.
            let isCtrlPressed = false;
            let lastHoverKey = '';
            let lastHoverActive = false;
            let lastCursor = '';
            let tooltipEl = null;
            let lastTooltipKey = '';

            const supportsCollection = typeof editor.createDecorationsCollection === 'function';
            const hoverCollection = supportsCollection ? editor.createDecorationsCollection() : null;
            let hoverDeltaIds = [];

            const ensureTooltip = () => {
                if (tooltipEl) return tooltipEl;
                tooltipEl = document.createElement('div');
                tooltipEl.className = 'ui-hover-tooltip hidden';
                document.body.appendChild(tooltipEl);
                return tooltipEl;
            };

            const showTooltip = (key, text, browserEvent) => {
                if (!text) return hideTooltip();
                if (key === lastTooltipKey && tooltipEl && !tooltipEl.classList.contains('hidden')) return;
                lastTooltipKey = key;
                const el = ensureTooltip();
                el.textContent = text;
                const x = browserEvent?.clientX ?? 0;
                const y = browserEvent?.clientY ?? 0;
                el.style.left = `${Math.min(window.innerWidth - 20, x + 12)}px`;
                el.style.top = `${Math.min(window.innerHeight - 20, y + 12)}px`;
                el.classList.remove('hidden');
            };

            const hideTooltip = () => {
                lastTooltipKey = '';
                if (!tooltipEl) return;
                tooltipEl.classList.add('hidden');
            };

            const setCursor = (cursor) => {
                const next = cursor || '';
                if (next === lastCursor) return;
                lastCursor = next;
                const editorDom = editor.getDomNode();
                if (editorDom) editorDom.style.cursor = next;
            };

            const clearHover = () => {
                if (!lastHoverKey && !lastHoverActive && !hoverDeltaIds.length) {
                    setCursor('');
                    hideTooltip();
                    return;
                }
                lastHoverKey = '';
                lastHoverActive = false;
                setCursor('');
                hideTooltip();
                if (hoverCollection) {
                    hoverCollection.clear();
                } else if (hoverDeltaIds.length) {
                    hoverDeltaIds = editor.deltaDecorations(hoverDeltaIds, []);
                }
            };

            const clearHoverDecorationOnly = () => {
                if (hoverCollection) {
                    hoverCollection.clear();
                } else if (hoverDeltaIds.length) {
                    hoverDeltaIds = editor.deltaDecorations(hoverDeltaIds, []);
                }
            };

            document.addEventListener('keydown', (e) => {
                if (e.ctrlKey || e.metaKey) isCtrlPressed = true;
            });

            document.addEventListener('keyup', (e) => {
                if (!e.ctrlKey && !e.metaKey) {
                    isCtrlPressed = false;
                    clearHover();
                }
            });

            let pendingMove = null;
            let hoverRaf = 0;
            const scheduleHoverUpdate = () => {
                if (hoverRaf) return;
                hoverRaf = (typeof requestAnimationFrame === 'function')
                    ? requestAnimationFrame(runHoverUpdate)
                    : setTimeout(runHoverUpdate, 16);
            };

            const runHoverUpdate = () => {
                hoverRaf = 0;
                const evt = pendingMove;
                pendingMove = null;
                if (!evt || !evt.target?.position) {
                    if (!isCtrlPressed) clearHover();
                    return;
                }
                if (!isCtrlPressed) {
                    clearHover();
                    return;
                }

                const model = editor.getModel();
                if (!model) return;

                const pos = evt.target.position;
                // Compute a stable underline range (word-ish around the cursor)
                const lineContent = model.getLineContent(pos.lineNumber);
                const column = pos.column;
                let startCol = column;
                let endCol = column;
                while (startCol > 1 && /[A-Z0-9%^$]/i.test(lineContent[startCol - 2])) startCol--;
                while (endCol <= lineContent.length && /[A-Z0-9%^$]/i.test(lineContent[endCol - 1])) endCol++;

                // Nothing meaningful under the cursor
                if (startCol === endCol) {
                    clearHover();
                    return;
                }

                const key = `${pos.lineNumber}:${startCol}:${endCol}`;
                if (key === lastHoverKey) {
                    setCursor(lastHoverActive ? 'pointer' : '');
                    return;
                }
                lastHoverKey = key;

                const ref = parseRoutineReferenceAtPosition(model, pos);
                if (!ref) {
                    lastHoverActive = false;
                    setCursor('');
                    clearHoverDecorationOnly();
                    hideTooltip();
                    return;
                }

                let navigable = true;
                let tooltip = 'Go to declaration';
                if (ref.type === 'local') {
                    const line = resolveLocalTagLine ? resolveLocalTagLine(model, ref.tag) : null;
                    navigable = !!line;
                    tooltip = navigable ? `Go to ${ref.tag} (local)` : '';
                } else if (ref.type === 'external') {
                    const target = ref.tag ? `${ref.tag}^${ref.routine}` : `^${ref.routine}`;
                    tooltip = `Go to ${target}`;
                }

                if (!navigable) {
                    lastHoverActive = false;
                    setCursor('');
                    clearHoverDecorationOnly();
                    hideTooltip();
                    return;
                }

                lastHoverActive = true;
                setCursor('pointer');
                showTooltip(key, tooltip, evt.event?.browserEvent);

                const decoration = {
                    range: new monaco.Range(pos.lineNumber, startCol, pos.lineNumber, endCol),
                    options: { inlineClassName: 'ctrl-hover-underline' }
                };
                if (hoverCollection) {
                    hoverCollection.set([decoration]);
                } else {
                    hoverDeltaIds = editor.deltaDecorations(hoverDeltaIds, [decoration]);
                }
            };

            // Track mouse movement to detect hovering over clickable targets (rAF throttled)
            editor.onMouseMove((e) => {
                pendingMove = e;
                scheduleHoverUpdate();
            });

            // --- Breakpoint gutter toggle + Ctrl+Click navigation ---
            editor.onMouseDown(async (e) => {
                const t = e.target.type;

                // Handle breakpoint toggle in gutter
                if (
                    t === monaco.editor.MouseTargetType.GUTTER_GLYPH_MARGIN ||
                    t === monaco.editor.MouseTargetType.GUTTER_LINE_NUMBERS
                ) {
                    const line = e.target.position && e.target.position.lineNumber;
                    if (!line) return;
                    toggleBreakpoint(line, dbgState, editor);
                    e.event.preventDefault();
                    return;
                }

                // Handle Ctrl+Click navigation
                const isCtrlPressed = e.event.ctrlKey || e.event.metaKey;
                if (isCtrlPressed && e.target.position) {
                    const handled = await goToDeclaration(editor, e.target.position, { silentIfMissing: true });
                    if (handled) {
                        e.event.preventDefault();
                        e.event.stopPropagation();
                    }
                }
            });
        }

        return {
            bindCtrlHoverAndGutter
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.createCtrlHoverManager = createCtrlHoverManager;
    }
})();
