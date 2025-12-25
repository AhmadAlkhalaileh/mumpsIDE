(() => {
    /**
     * Find/Replace Dialog
     * Compact floating dialog for in-file search
     *
     * Checklist: FND-001 to FND-008
     */
    function createFindReplaceDialog({ deps } = {}) {
        const createDialog = deps?.createDialog || window.AhmadIDEModules?.ui?.createDialog;
        const primitives = deps?.primitives || window.AhmadIDEModules?.ui?.primitives;

        if (!createDialog || !primitives) {
            throw new Error('FindReplaceDialog requires ui primitives');
        }

        let dialogApi = null;
        let mode = 'find'; // 'find' or 'replace'
        let options = { regex: false, caseSensitive: false };

        const buildContent = () => {
            const container = document.createElement('div');
            container.style.cssText = 'padding:var(--ui-space-4);min-width:480px;';

            const { createInput, createCheckbox } = primitives;

            // Find input
            const findRow = document.createElement('div');
            findRow.style.cssText = 'display:flex;gap:var(--ui-space-2);margin-bottom:var(--ui-space-3);';
            const findInput = createInput({ id: 'findInput', placeholder: 'Find', value: '' });
            findInput.style.flex = '1';
            findRow.appendChild(findInput);

            // Replace input (conditional)
            const replaceRow = document.createElement('div');
            replaceRow.id = 'replaceRow';
            replaceRow.style.cssText = 'display:flex;gap:var(--ui-space-2);margin-bottom:var(--ui-space-3);';
            replaceRow.style.display = mode === 'replace' ? 'flex' : 'none';
            const replaceInput = createInput({ id: 'replaceInput', placeholder: 'Replace', value: '' });
            replaceInput.style.flex = '1';
            replaceRow.appendChild(replaceInput);

            // Options row
            const optionsRow = document.createElement('div');
            optionsRow.style.cssText = 'display:flex;gap:var(--ui-space-4);margin-bottom:var(--ui-space-3);';
            const regexCheck = createCheckbox({
                id: 'findRegex',
                label: 'Regex',
                checked: options.regex,
                onChange: (checked) => { options.regex = checked; }
            });
            const caseCheck = createCheckbox({
                id: 'findCase',
                label: 'Match Case',
                checked: options.caseSensitive,
                onChange: (checked) => { options.caseSensitive = checked; }
            });
            optionsRow.appendChild(regexCheck);
            optionsRow.appendChild(caseCheck);

            // Match count
            const matchCount = document.createElement('div');
            matchCount.id = 'matchCount';
            matchCount.style.cssText = 'font-size:12px;color:rgba(255,255,255,0.6);margin-bottom:var(--ui-space-3);';
            matchCount.textContent = 'No matches';

            // Action buttons
            const actionsRow = document.createElement('div');
            actionsRow.style.cssText = 'display:flex;gap:var(--ui-space-2);justify-content:flex-end;';

            const prevBtn = document.createElement('button');
            prevBtn.className = 'ui-btn ui-btn--ghost ui-btn--sm';
            prevBtn.textContent = '↑ Previous';
            prevBtn.addEventListener('click', () => performFind('prev'));

            const nextBtn = document.createElement('button');
            nextBtn.className = 'ui-btn ui-btn--ghost ui-btn--sm';
            nextBtn.textContent = 'Next ↓';
            nextBtn.addEventListener('click', () => performFind('next'));

            actionsRow.appendChild(prevBtn);
            actionsRow.appendChild(nextBtn);

            if (mode === 'replace') {
                const replaceOneBtn = document.createElement('button');
                replaceOneBtn.className = 'ui-btn ui-btn--primary ui-btn--sm';
                replaceOneBtn.textContent = 'Replace';
                replaceOneBtn.addEventListener('click', () => performReplace(false));

                const replaceAllBtn = document.createElement('button');
                replaceAllBtn.className = 'ui-btn ui-btn--primary ui-btn--sm';
                replaceAllBtn.textContent = 'Replace All';
                replaceAllBtn.addEventListener('click', () => performReplace(true));

                actionsRow.appendChild(replaceOneBtn);
                actionsRow.appendChild(replaceAllBtn);
            }

            container.appendChild(findRow);
            container.appendChild(replaceRow);
            container.appendChild(optionsRow);
            container.appendChild(matchCount);
            container.appendChild(actionsRow);

            return container;
        };

        const performFind = (direction) => {
            const query = document.getElementById('findInput')?.value;
            if (!query) return;

            // TODO: Integrate with Monaco editor find API
            console.log('Find:', query, direction, options);
        };

        const performReplace = (all) => {
            const query = document.getElementById('findInput')?.value;
            const replacement = document.getElementById('replaceInput')?.value || '';
            if (!query) return;

            // TODO: Integrate with Monaco editor replace API
            console.log('Replace:', query, replacement, all, options);
        };

        const ensureDialog = () => {
            if (dialogApi) return;

            dialogApi = createDialog({
                ariaLabel: mode === 'find' ? 'Find' : 'Find and Replace',
                closeOnEscape: true,
                closeOnBackdrop: false,
                onClose: () => { }
            });

            const wrapper = document.createElement('div');
            wrapper.className = 'ui-dialog-layout';

            const header = document.createElement('div');
            header.className = 'ui-dialog-header';
            const headerLeft = document.createElement('div');
            headerLeft.className = 'ui-dialog-header__left';
            const title = document.createElement('div');
            title.className = 'ui-dialog-title';
            title.textContent = mode === 'find' ? 'Find' : 'Find and Replace';
            headerLeft.appendChild(title);

            const headerRight = document.createElement('div');
            headerRight.className = 'ui-dialog-header__right';
            const closeBtn = document.createElement('button');
            closeBtn.className = 'ui-dialog-close';
            closeBtn.type = 'button';
            closeBtn.title = 'Close';
            closeBtn.textContent = '✕';
            closeBtn.addEventListener('click', () => dialogApi.close('x'));
            headerRight.appendChild(closeBtn);

            header.appendChild(headerLeft);
            header.appendChild(headerRight);

            const body = document.createElement('div');
            body.style.cssText = 'flex:1;overflow:auto;';
            body.appendChild(buildContent());

            wrapper.appendChild(header);
            wrapper.appendChild(body);

            dialogApi.setContent(wrapper);
        };

        const open = (openMode = 'find') => {
            mode = openMode;
            dialogApi = null; // Force rebuild with new mode
            ensureDialog();
            dialogApi.open();
            requestAnimationFrame(() => {
                document.getElementById('findInput')?.focus();
            });
        };

        return { open };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.search = window.AhmadIDEModules.features.search || {};
        window.AhmadIDEModules.features.search.createFindReplaceDialog = createFindReplaceDialog;
    }
})();
