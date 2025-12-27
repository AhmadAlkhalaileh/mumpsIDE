/**
 * MUMPS GOTO Map Visualization Panel
 *
 * A visually stunning flow visualization showing:
 * - Vertical timeline of routine structure (tags/labels)
 * - Curved arcs connecting GOTO statements to targets
 * - Color-coded forward (green) vs backward (orange) jumps
 * - Heat map intensity for dense GOTO regions
 * - Interactive tooltips and click-to-jump navigation
 */

(() => {
    'use strict';

    class MumpsGotoMapPanel {
        constructor({ monacoRef, getActiveEditor, showToast } = {}) {
            this.monaco = monacoRef;
            this.getActiveEditor = getActiveEditor || (() => null);
            this.showToast = showToast || (() => { });

            this.panelElement = null;
            this.svgElement = null;
            this.currentGotoData = null;
            this.isVisible = false;
        }

        /**
         * Create and show the GOTO Map panel
         */
        show(gotoAnalysis, lineCount) {
            if (!gotoAnalysis) {
                this.showToast('info', 'GOTO Map', 'No GOTO analysis data available');
                return;
            }

            this.currentGotoData = gotoAnalysis;

            if (!this.panelElement) {
                this.createPanel();
            }

            this.renderMap(gotoAnalysis, lineCount);
            this.panelElement.classList.add('visible');
            this.isVisible = true;
        }

        /**
         * Hide the panel
         */
        hide() {
            if (this.panelElement) {
                this.panelElement.classList.remove('visible');
                this.isVisible = false;
            }
        }

        /**
         * Toggle panel visibility
         */
        toggle(gotoAnalysis, lineCount) {
            if (this.isVisible) {
                this.hide();
            } else {
                this.show(gotoAnalysis, lineCount);
            }
        }

        /**
         * Create the panel DOM structure
         */
        createPanel() {
            const panel = document.createElement('div');
            panel.className = 'mumps-goto-map-panel';
            panel.innerHTML = `
                <div class="goto-map-header">
                    <div class="goto-map-title">
                        <span class="goto-map-icon">🗺️</span>
                        <span>GOTO Flow Map</span>
                    </div>
                    <div class="goto-map-stats"></div>
                    <button class="goto-map-close" title="Close (Escape)">×</button>
                </div>
                <div class="goto-map-body">
                    <svg class="goto-map-svg" width="100%" height="100%">
                        <defs>
                            <marker id="arrowhead-forward" markerWidth="10" markerHeight="10"
                                    refX="9" refY="3" orient="auto">
                                <polygon points="0 0, 10 3, 0 6" fill="#50fa7b" />
                            </marker>
                            <marker id="arrowhead-backward" markerWidth="10" markerHeight="10"
                                    refX="9" refY="3" orient="auto">
                                <polygon points="0 0, 10 3, 0 6" fill="#ffb86c" />
                            </marker>
                            <filter id="glow">
                                <feGaussianBlur stdDeviation="2" result="coloredBlur"/>
                                <feMerge>
                                    <feMergeNode in="coloredBlur"/>
                                    <feMergeNode in="SourceGraphic"/>
                                </feMerge>
                            </filter>
                        </defs>
                        <g class="goto-map-content"></g>
                    </svg>
                </div>
                <div class="goto-map-legend">
                    <div class="legend-item">
                        <span class="legend-line forward"></span>
                        <span>Forward Jump</span>
                    </div>
                    <div class="legend-item">
                        <span class="legend-line backward"></span>
                        <span>Backward Jump</span>
                    </div>
                    <div class="legend-item">
                        <span class="legend-dot label"></span>
                        <span>Label/Tag</span>
                    </div>
                    <div class="legend-item">
                        <span class="legend-dot goto"></span>
                        <span>GOTO Statement</span>
                    </div>
                </div>
            `;

            // Event listeners
            const closeBtn = panel.querySelector('.goto-map-close');
            closeBtn.addEventListener('click', () => this.hide());

            // Escape key to close
            document.addEventListener('keydown', (e) => {
                if (e.key === 'Escape' && this.isVisible) {
                    this.hide();
                }
            });

            document.body.appendChild(panel);
            this.panelElement = panel;
            this.svgElement = panel.querySelector('.goto-map-svg');
        }

        /**
         * Render the GOTO flow map
         */
        renderMap(gotoData, lineCount) {
            const svg = this.svgElement.querySelector('.goto-map-content');
            svg.innerHTML = ''; // Clear previous

            // Update stats
            const statsEl = this.panelElement.querySelector('.goto-map-stats');
            statsEl.innerHTML = `
                <span class="stat">Total: ${gotoData.totalGotos}</span>
                <span class="stat forward">Forward: ${gotoData.forwardGotos}</span>
                <span class="stat backward">Backward: ${gotoData.backwardGotos}</span>
                <span class="stat spaghetti">Complexity: ${gotoData.spaghettiScore}</span>
            `;

            // Dimensions
            const width = this.svgElement.clientWidth || 600;
            const height = this.svgElement.clientHeight || 800;
            const padding = { top: 20, right: 100, bottom: 20, left: 60 };
            const timelineX = padding.left;
            const mapHeight = height - padding.top - padding.bottom;

            // Scale: line number -> Y position
            const yScale = (lineNum) => {
                return padding.top + (lineNum / lineCount) * mapHeight;
            };

            // Draw timeline axis
            const timeline = document.createElementNS('http://www.w3.org/2000/svg', 'line');
            timeline.setAttribute('x1', timelineX);
            timeline.setAttribute('y1', padding.top);
            timeline.setAttribute('x2', timelineX);
            timeline.setAttribute('y2', height - padding.bottom);
            timeline.setAttribute('class', 'timeline-axis');
            svg.appendChild(timeline);

            // Draw labels (tag definitions)
            const labels = Object.entries(gotoData.labels || {});
            labels.forEach(([tagName, lineNum]) => {
                const y = yScale(lineNum);

                // Label dot
                const dot = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
                dot.setAttribute('cx', timelineX);
                dot.setAttribute('cy', y);
                dot.setAttribute('r', 5);
                dot.setAttribute('class', 'label-dot');
                dot.setAttribute('data-line', lineNum);
                dot.setAttribute('data-tag', tagName);

                // Click to jump
                dot.addEventListener('click', () => this.jumpToLine(lineNum));

                // Tooltip
                const title = document.createElementNS('http://www.w3.org/2000/svg', 'title');
                title.textContent = `${tagName} (line ${lineNum})`;
                dot.appendChild(title);

                svg.appendChild(dot);

                // Label text
                const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
                text.setAttribute('x', timelineX - 10);
                text.setAttribute('y', y + 4);
                text.setAttribute('class', 'label-text');
                text.setAttribute('text-anchor', 'end');
                text.textContent = tagName;
                text.addEventListener('click', () => this.jumpToLine(lineNum));
                svg.appendChild(text);
            });

            // Draw GOTO arcs
            const gotos = gotoData.gotos || [];
            gotos.forEach((gotoItem, index) => {
                const fromY = yScale(gotoItem.from);
                const toY = gotoItem.toLine ? yScale(gotoItem.toLine) : null;

                if (!toY) return; // Unknown target

                const isBackward = gotoItem.toLine < gotoItem.from;
                const direction = isBackward ? 'backward' : 'forward';

                // Calculate curve
                const curve = this.createCurvedPath(timelineX, fromY, toY, isBackward);

                const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
                path.setAttribute('d', curve);
                path.setAttribute('class', `goto-arc ${direction}`);
                path.setAttribute('data-from', gotoItem.from);
                path.setAttribute('data-to', gotoItem.toLine);
                path.setAttribute('marker-end', `url(#arrowhead-${direction})`);

                // Tooltip
                const title = document.createElementNS('http://www.w3.org/2000/svg', 'title');
                title.textContent = `GOTO ${gotoItem.to}\nFrom line ${gotoItem.from} → ${gotoItem.toLine}`;
                path.appendChild(title);

                // Click to jump to source
                path.addEventListener('click', () => this.jumpToLine(gotoItem.from));

                // Highlight on hover
                path.addEventListener('mouseenter', () => {
                    path.classList.add('highlighted');
                });
                path.addEventListener('mouseleave', () => {
                    path.classList.remove('highlighted');
                });

                svg.appendChild(path);

                // GOTO source dot
                const gotoDot = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
                gotoDot.setAttribute('cx', timelineX);
                gotoDot.setAttribute('cy', fromY);
                gotoDot.setAttribute('r', 3);
                gotoDot.setAttribute('class', `goto-dot ${direction}`);
                gotoDot.setAttribute('data-line', gotoItem.from);
                gotoDot.addEventListener('click', () => this.jumpToLine(gotoItem.from));
                svg.appendChild(gotoDot);
            });

            // Draw heat map overlay (density visualization)
            this.drawHeatMap(svg, gotos, lineCount, yScale, timelineX);
        }

        /**
         * Create curved path between two Y positions
         */
        createCurvedPath(x, fromY, toY, isBackward) {
            const distance = Math.abs(toY - fromY);
            const curvature = Math.min(150, distance * 0.4); // Curve intensity
            const offsetX = curvature; // How far right the curve extends

            // Start and end points
            const startX = x + 10;
            const endX = x + 10;

            // Control points for cubic Bezier curve
            const cp1x = startX + offsetX;
            const cp1y = fromY;
            const cp2x = endX + offsetX;
            const cp2y = toY;

            return `M ${startX} ${fromY} C ${cp1x} ${cp1y}, ${cp2x} ${cp2y}, ${endX} ${toY}`;
        }

        /**
         * Draw heat map showing GOTO density
         */
        drawHeatMap(svg, gotos, lineCount, yScale, timelineX) {
            // Divide code into buckets
            const bucketSize = Math.max(5, Math.ceil(lineCount / 50));
            const buckets = new Map();

            gotos.forEach(g => {
                const bucket = Math.floor(g.from / bucketSize);
                buckets.set(bucket, (buckets.get(bucket) || 0) + 1);
            });

            const maxDensity = Math.max(...buckets.values(), 1);

            buckets.forEach((count, bucket) => {
                const intensity = count / maxDensity;
                const startLine = bucket * bucketSize;
                const endLine = (bucket + 1) * bucketSize;
                const y1 = yScale(startLine);
                const y2 = yScale(endLine);

                const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
                rect.setAttribute('x', timelineX - 15);
                rect.setAttribute('y', y1);
                rect.setAttribute('width', 8);
                rect.setAttribute('height', y2 - y1);
                rect.setAttribute('class', 'heat-bar');
                rect.setAttribute('opacity', intensity * 0.6);
                rect.setAttribute('fill', this.getHeatColor(intensity));

                const title = document.createElementNS('http://www.w3.org/2000/svg', 'title');
                title.textContent = `${count} GOTO(s) in lines ${startLine}-${endLine}`;
                rect.appendChild(title);

                svg.insertBefore(rect, svg.firstChild); // Behind other elements
            });
        }

        /**
         * Get heat color based on intensity
         */
        getHeatColor(intensity) {
            if (intensity < 0.3) return '#50fa7b'; // Green (low)
            if (intensity < 0.6) return '#f1fa8c'; // Yellow (medium)
            if (intensity < 0.8) return '#ffb86c'; // Orange (high)
            return '#ff5555'; // Red (very high)
        }

        /**
         * Jump to line in editor
         */
        jumpToLine(lineNumber) {
            const editor = this.getActiveEditor();
            if (!editor) return;

            editor.revealLineInCenter(lineNumber);
            editor.setPosition({ lineNumber, column: 1 });
            editor.focus();

            this.showToast('info', 'GOTO Map', `Jumped to line ${lineNumber}`);
        }

        /**
         * Cleanup
         */
        dispose() {
            if (this.panelElement) {
                this.panelElement.remove();
                this.panelElement = null;
            }
        }
    }

    // Export
    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
        window.AhmadIDEModules.mumps.MumpsGotoMapPanel = MumpsGotoMapPanel;
        // Global fallback
        window.MumpsGotoMapPanel = MumpsGotoMapPanel;
    }
})();
