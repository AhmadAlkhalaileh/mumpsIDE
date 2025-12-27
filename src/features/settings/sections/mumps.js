(() => {
    function renderMumpsSection({ draft, primitives, onDraftChange }) {
        const { createSection, createDescription } = primitives;

        // Fallback or use primitives if available
        const createTextArea = primitives.createTextArea || (({ value, placeholder, rows, onChange }) => {
            const el = document.createElement('textarea');
            el.className = 'ui-input ui-textarea';
            el.value = value || '';
            el.placeholder = placeholder || '';
            el.rows = rows || 3;
            el.style.width = '100%';
            el.style.fontFamily = 'monospace';
            el.addEventListener('input', (e) => onChange(e.target.value));
            return el;
        });

        const createInput = primitives.createInput || (({ value, placeholder, onChange }) => {
            const el = document.createElement('input');
            el.className = 'ui-input';
            el.type = 'text';
            el.value = value || '';
            el.placeholder = placeholder || '';
            el.style.width = '100%';
            el.addEventListener('input', (e) => onChange(e.target.value));
            return el;
        });

        const root = document.createElement('div');
        root.className = 'settings-section';

        // Helper to get/set deeply
        const getVal = (path, def) => {
            return path.split('.').reduce((obj, key) => (obj && obj[key] !== undefined) ? obj[key] : undefined, draft) ?? def;
        };

        const setVal = (path, val) => {
            const keys = path.split('.');
            let curr = draft;
            for (let i = 0; i < keys.length - 1; i++) {
                if (!curr[keys[i]]) curr[keys[i]] = {};
                curr = curr[keys[i]];
            }
            curr[keys[keys.length - 1]] = val;
            onDraftChange();
        };

        // --- Routine Header (Snippet 'entry-point') ---
        const routineHeaderSection = createSection({ title: 'Routine Header Template' });

        const currentSnippet = getVal('mumps.snippets.entry-point.body');
        const routineVal = Array.isArray(currentSnippet) ? currentSnippet.join('\n') : (currentSnippet || '');

        const routineArea = createTextArea({
            value: routineVal,
            placeholder: 'Leave empty to use default routine header...',
            rows: 8,
            onChange: (val) => {
                const lines = val.split('\n');
                let existing = getVal('mumps.snippets.entry-point', {});
                existing.body = lines;
                setVal('mumps.snippets.entry-point', existing);
            }
        });

        routineHeaderSection.content.appendChild(createDescription('Define the header inserted when creating new routines or using "mentry". Variables: ${TAG}, ${DATE}, ${USER}, ${ROUTINE}.'));
        routineHeaderSection.content.appendChild(routineArea);
        root.appendChild(routineHeaderSection.root);

        // --- Tags Index Header ---
        const tagsHeaderSection = createSection({ title: 'Tags Index Header' });

        const currentHeader = getVal('mumps.tagsGenerator.templates.header');
        const headerVal = Array.isArray(currentHeader) ? currentHeader.join('\n') : (currentHeader || '');

        const headerArea = createTextArea({
            value: headerVal,
            placeholder: 'Leave empty to use default tags header...',
            rows: 6,
            onChange: (val) => {
                const lines = val.split('\n');
                let existing = getVal('mumps.tagsGenerator.templates', {});
                existing.header = lines;
                setVal('mumps.tagsGenerator.templates', existing);
            }
        });

        tagsHeaderSection.content.appendChild(createDescription('Define the header for the auto-generated Tags Index. Variables: ${DATE}, ${TIME}, ${COUNT}.'));
        tagsHeaderSection.content.appendChild(headerArea);
        root.appendChild(tagsHeaderSection.root);

        // --- Tags Item Template ---
        const tagsItemSection = createSection({ title: 'Tags Item Template' });

        const currentItem = getVal('mumps.tagsGenerator.templates.item');
        const itemVal = Array.isArray(currentItem) ? currentItem.join('\n') : (currentItem || '');

        const itemArea = createTextArea({
            value: itemVal,
            placeholder: 'Leave empty to use default item format...',
            rows: 3,
            onChange: (val) => {
                const lines = val.split('\n');
                let existing = getVal('mumps.tagsGenerator.templates', {});
                existing.item = lines;
                setVal('mumps.tagsGenerator.templates', existing);
            }
        });

        tagsItemSection.content.appendChild(createDescription('Define the format for each tag item. Variables: ${TAG}, ${LINE}, ${DESC}, ${PARAMS}, ${SIGNATURE}.'));
        tagsItemSection.content.appendChild(itemArea);
        root.appendChild(tagsItemSection.root);

        // --- Auto Tag Header Generation ---
        const autoTagHeaderSection = createSection({ title: 'Auto Tag Header Generation' });

        const authorName = getVal('mumps.tagHeader.author', '');
        const authorInput = createInput({
            value: authorName,
            placeholder: 'Your name (e.g., Ahmad Alkhalaileh)',
            onChange: (val) => {
                setVal('mumps.tagHeader.author', val);
            }
        });

        autoTagHeaderSection.content.appendChild(createDescription('Author name for generated tag headers. Right-click on a tag name and select "Generate Tag Header".'));
        autoTagHeaderSection.content.appendChild(authorInput);

        // Template configuration
        const templateLabel = document.createElement('div');
        templateLabel.style.cssText = 'margin-top:16px;margin-bottom:4px;font-weight:600;font-size:13px;';
        templateLabel.textContent = 'Tag Header Template';
        autoTagHeaderSection.content.appendChild(templateLabel);

        const currentTemplate = getVal('mumps.tagHeader.template', '');
        const templateArea = createTextArea({
            value: currentTemplate,
            placeholder: 'Leave empty for default template...',
            rows: 12,
            onChange: (val) => {
                setVal('mumps.tagHeader.template', val);
            }
        });

        const templateDesc = createDescription('Variables: ${TAG}, ${DESCRIPTION}, ${PARAMS_IN}, ${OUT}, ${RETURN}, ${NOTES}, ${AUTHOR}');
        autoTagHeaderSection.content.appendChild(templateDesc);
        autoTagHeaderSection.content.appendChild(templateArea);

        root.appendChild(autoTagHeaderSection.root);

        return root;
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.settings = window.AhmadIDEModules.features.settings || {};
        window.AhmadIDEModules.features.settings.sections = window.AhmadIDEModules.features.settings.sections || {};
        window.AhmadIDEModules.features.settings.sections.renderMumpsSection = renderMumpsSection;
    }
})();
