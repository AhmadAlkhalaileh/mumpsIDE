(() => {
    function createLayoutResizer() {
        const root = document.documentElement;

        // --- Helper: Create Resizer Element ---
        function createHandle(id, type, targetSelector, varName, min, max, invert = false) {
            let handle = document.getElementById(id);
            if (!handle) {
                handle = document.createElement('div');
                handle.id = id;
                handle.className = `layout-resizer ${type}`;
                document.body.appendChild(handle);
            }

            // State
            let isResizing = false;
            let startVal = 0;
            let startX = 0;
            let startY = 0;
            let currentVal = 0;
            let rAF = null;

            const onMouseDown = (e) => {
                isResizing = true;
                startX = e.clientX;
                startY = e.clientY;

                const style = getComputedStyle(root);
                const rawVal = parseInt(style.getPropertyValue(varName) || '0', 10);
                startVal = isNaN(rawVal) ? (type === 'vertical' ? 320 : 250) : rawVal;
                currentVal = startVal;

                handle.classList.add('resizing');
                document.body.style.cursor = type === 'vertical' ? 'ns-resize' : 'ew-resize';
                document.body.classList.add('is-resizing'); // Prevent selection

                e.preventDefault();
            };

            const updateLayout = () => {
                if (!isResizing) return;
                root.style.setProperty(varName, `${currentVal}px`);

                // Only trigger window resize occasionally if needed, 
                // but Monaco's automaticLayout should handle container resizing if it's polling.
                // If not, we can dispatch resize.
                // Optimized: Dispatch resize event only occasionally or let Monaco handle it?
                // Dispatching it every frame is what causes LAG.
                // Let's rely on CSS first. Monaco measures on its own timer (100ms usually).

                // Construct a custom event that specific components can listen to if needed, 
                // instead of full window resize.
                // window.dispatchEvent(new Event('resize')); 

                rAF = null;
            };

            const onMouseMove = (e) => {
                if (!isResizing) return;

                let delta = 0;
                if (type === 'horizontal') {
                    delta = e.clientX - startX;
                } else {
                    delta = e.clientY - startY; // Moving down increases Y
                    // For bottom panel: Dragging down REDUCES height.
                    delta = -delta;
                }

                if (invert) delta = -delta; // For right sidebar

                let newVal = startVal + delta;
                if (newVal < min) newVal = min;
                if (newVal > max) newVal = max;

                if (newVal !== currentVal) {
                    currentVal = newVal;
                    if (!rAF) {
                        rAF = requestAnimationFrame(updateLayout);
                    }
                }
            };

            const onMouseUp = () => {
                if (!isResizing) return;
                isResizing = false;
                if (rAF) cancelAnimationFrame(rAF);
                handle.classList.remove('resizing');
                document.body.style.cursor = '';
                document.body.classList.remove('is-resizing');

                // Final hard resize trigger to ensure everything settles
                root.style.setProperty(varName, `${currentVal}px`);
                setTimeout(() => window.dispatchEvent(new Event('resize')), 50);
            };

            handle.addEventListener('mousedown', onMouseDown);
            window.addEventListener('mousemove', onMouseMove);
            window.addEventListener('mouseup', onMouseUp);

            return handle;
        }

        // --- Initialize Resizers ---
        const leftResizer = createHandle('resizerLeft', 'horizontal', '.tool-window-left', '--ui-sidebar-left-w', 150, 600);
        const rightResizer = createHandle('resizerRight', 'horizontal', '.tool-window-right', '--ui-sidebar-right-w', 150, 600, true);
        const bottomResizer = createHandle('resizerBottom', 'vertical', '.bottom-bar-wrapper', '--bottom-toolwindow-height', 100, 800);

        // --- Positioning Logic using RequestAnimationFrame ---
        let posRAF = null;
        function updatePositions() {
            const leftPanel = document.querySelector('.tool-window-left');
            const rightPanel = document.querySelector('.tool-window-right');
            const bottomWrapper = document.getElementById('bottomBarWrapper');

            // Left
            if (leftPanel && !leftPanel.classList.contains('hidden') && leftPanel.offsetParent) {
                const rect = leftPanel.getBoundingClientRect();
                leftResizer.style.display = 'block';
                leftResizer.style.left = `${rect.right - 3}px`; // Center over border
                leftResizer.style.top = `${rect.top}px`;
                leftResizer.style.height = `${rect.height}px`;
            } else {
                leftResizer.style.display = 'none';
            }

            // Right
            if (rightPanel && !rightPanel.classList.contains('hidden') && rightPanel.offsetParent) {
                const rect = rightPanel.getBoundingClientRect();
                rightResizer.style.display = 'block';
                rightResizer.style.left = `${rect.left - 3}px`;
                rightResizer.style.top = `${rect.top}px`;
                rightResizer.style.height = `${rect.height}px`;
            } else {
                rightResizer.style.display = 'none';
            }

            // Bottom
            if (bottomWrapper && !bottomWrapper.classList.contains('hidden')) {
                const rect = bottomWrapper.getBoundingClientRect();
                // We want the resizer at the TOP of the bottom wrapper
                bottomResizer.style.display = 'block';
                bottomResizer.style.left = `${rect.left}px`;
                bottomResizer.style.top = `${rect.top - 3}px`;
                bottomResizer.style.width = `${rect.width}px`;
            } else {
                bottomResizer.style.display = 'none';
            }

            posRAF = null;
        }

        let updateTimer = null;
        const scheduleUpdate = () => {
            // Debounce updates
            if (updateTimer) return;
            updateTimer = setTimeout(() => {
                updateTimer = null;
                if (!posRAF) posRAF = requestAnimationFrame(updatePositions);
            }, 50);
        };

        // Only observe the main layout elements, not entire body
        const elementsToWatch = [
            document.querySelector('.tool-window-left'),
            document.querySelector('.tool-window-right'),
            document.getElementById('bottomBarWrapper')
        ].filter(Boolean);

        elementsToWatch.forEach(el => {
            const observer = new MutationObserver(scheduleUpdate);
            observer.observe(el, { attributes: true, attributeFilter: ['class', 'style'] });
        });

        window.addEventListener('resize', scheduleUpdate);
        setTimeout(updatePositions, 100);
        setInterval(updatePositions, 3000); // Fail-safe less frequent
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', createLayoutResizer);
    } else {
        createLayoutResizer();
    }
})();
