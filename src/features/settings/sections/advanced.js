(() => {
    function renderAdvancedSection(ctx) {
        const { draft, primitives, onDraftChange } = ctx;
        const { createButton, createToggle, createInput } = primitives;

        const root = document.createElement('div');

        const group = document.createElement('div');
        group.className = 'ui-settings-group';
        group.innerHTML = `
            <div class="ui-settings-group__title">Advanced</div>
            <div class="ui-settings-group__hint">Developer utilities</div>
        `;

        const row = document.createElement('div');
        row.className = 'ui-settings-row';
        row.dataset.filterText = 'devtools developer tools';
        row.innerHTML = `<div class="ui-settings-row__label">Developer Tools</div>`;
        const control = document.createElement('div');
        control.className = 'ui-settings-row__control';

        const toggle = createButton({
            label: 'Toggle DevTools',
            variant: 'primary',
            onClick: async () => {
                try {
                    await window.ahmadIDE?.toggleDevTools?.();
                } catch (_) { }
            }
        });
        control.appendChild(toggle);
        row.appendChild(control);
        group.appendChild(row);

        root.appendChild(group);

        const tlGroup = document.createElement('div');
        tlGroup.className = 'ui-settings-group';
        tlGroup.innerHTML = `
            <div class="ui-settings-group__title">Debugger Timeline</div>
            <div class="ui-settings-group__hint">Time‑travel snapshots on every stop (Premium)</div>
        `;

        const ensureTimelineShape = () => {
            draft.debugger = draft.debugger || {};
            draft.debugger.timeline = draft.debugger.timeline || {};
        };

        const tl = () => {
            ensureTimelineShape();
            return draft.debugger.timeline;
        };

        const numRow = ({ labelText, filterText, value, min, max, step, onValue }) => {
            const rowEl = document.createElement('div');
            rowEl.className = 'ui-settings-row';
            rowEl.dataset.filterText = filterText;
            rowEl.innerHTML = `<div class="ui-settings-row__label">${labelText}</div>`;
            const control = document.createElement('div');
            control.className = 'ui-settings-row__control';
            const input = createInput({ type: 'number', value: String(value ?? ''), ariaLabel: labelText });
            if (Number.isFinite(min)) input.min = String(min);
            if (Number.isFinite(max)) input.max = String(max);
            if (Number.isFinite(step)) input.step = String(step);
            input.addEventListener('change', () => {
                const n = Number(input.value);
                if (!Number.isFinite(n)) return;
                const clamped = Math.max(min, Math.min(max, n));
                input.value = String(clamped);
                onValue(clamped);
            });
            control.appendChild(input);
            rowEl.appendChild(control);
            return rowEl;
        };

        // Enable timeline
        {
            const rowEl = document.createElement('div');
            rowEl.className = 'ui-settings-row';
            rowEl.dataset.filterText = 'debugger timeline time travel snapshots';
            rowEl.innerHTML = `<div class="ui-settings-row__label">Enable Timeline</div>`;
            const control = document.createElement('div');
            control.className = 'ui-settings-row__control';
            const toggle = createToggle({
                label: 'Capture snapshots',
                checked: !!tl().enable,
                onChange: (_e, checked) => onDraftChange((d) => {
                    d.debugger = d.debugger || {};
                    d.debugger.timeline = d.debugger.timeline || {};
                    d.debugger.timeline.enable = checked;
                    return d;
                })
            });
            control.appendChild(toggle.root);
            rowEl.appendChild(control);
            tlGroup.appendChild(rowEl);
        }

        // Capture on stop
        {
            const rowEl = document.createElement('div');
            rowEl.className = 'ui-settings-row';
            rowEl.dataset.filterText = 'debugger timeline capture on stop breakpoints step pause';
            rowEl.innerHTML = `<div class="ui-settings-row__label">Capture on stop</div>`;
            const control = document.createElement('div');
            control.className = 'ui-settings-row__control';
            const toggle = createToggle({
                label: 'Auto-capture on breakpoint/step',
                checked: tl().captureOnStop !== false,
                onChange: (_e, checked) => onDraftChange((d) => {
                    d.debugger = d.debugger || {};
                    d.debugger.timeline = d.debugger.timeline || {};
                    d.debugger.timeline.captureOnStop = checked;
                    return d;
                })
            });
            control.appendChild(toggle.root);
            rowEl.appendChild(control);
            tlGroup.appendChild(rowEl);
        }

        // Max snapshots
        tlGroup.appendChild(numRow({
            labelText: 'Max snapshots',
            filterText: 'debugger timeline max snapshots limit',
            value: tl().maxSnapshots ?? 50,
            min: 1,
            max: 500,
            step: 1,
            onValue: (n) => onDraftChange((d) => {
                d.debugger = d.debugger || {};
                d.debugger.timeline = d.debugger.timeline || {};
                d.debugger.timeline.maxSnapshots = n;
                return d;
            })
        }));

        // Max value length
        tlGroup.appendChild(numRow({
            labelText: 'Max value length',
            filterText: 'debugger timeline max value length truncate',
            value: tl().maxValueLength ?? 200,
            min: 20,
            max: 20000,
            step: 10,
            onValue: (n) => onDraftChange((d) => {
                d.debugger = d.debugger || {};
                d.debugger.timeline = d.debugger.timeline || {};
                d.debugger.timeline.maxValueLength = n;
                return d;
            })
        }));

        // Capture watched globals
        {
            const rowEl = document.createElement('div');
            rowEl.className = 'ui-settings-row';
            rowEl.dataset.filterText = 'debugger timeline watched globals captureGlobals';
            rowEl.innerHTML = `<div class="ui-settings-row__label">Watched globals</div>`;
            const control = document.createElement('div');
            control.className = 'ui-settings-row__control';
            const toggle = createToggle({
                label: 'Capture watched global nodes',
                checked: !!tl().captureGlobals,
                onChange: (_e, checked) => onDraftChange((d) => {
                    d.debugger = d.debugger || {};
                    d.debugger.timeline = d.debugger.timeline || {};
                    d.debugger.timeline.captureGlobals = checked;
                    return d;
                })
            });
            control.appendChild(toggle.root);
            rowEl.appendChild(control);
            tlGroup.appendChild(rowEl);
        }

        root.appendChild(tlGroup);
        return root;
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.settings = window.AhmadIDEModules.features.settings || {};
        window.AhmadIDEModules.features.settings.sections = window.AhmadIDEModules.features.settings.sections || {};
        window.AhmadIDEModules.features.settings.sections.renderAdvancedSection = renderAdvancedSection;
    }
})();
