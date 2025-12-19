(() => {
    /**
     * Font Preview Helper
     * Creates live preview elements that update as settings change
     */
    function createFontPreview({ type, settings } = {}) {
        const container = document.createElement('div');
        container.className = 'ui-settings-preview';
        container.style.cssText = 'margin-top:var(--ui-space-4);padding:var(--ui-space-3);background:rgba(0,0,0,0.2);border-radius:var(--ui-radius-2);border:1px solid var(--ui-border-subtle);';

        const label = document.createElement('div');
        label.style.cssText = 'font-size:11px;color:rgba(255,255,255,0.5);margin-bottom:var(--ui-space-2);font-weight:500;text-transform:uppercase;letter-spacing:0.5px;';
        label.textContent = 'Preview';

        const previewText = document.createElement('div');
        previewText.className = `font-preview-${type}`;
        previewText.style.cssText = 'line-height:1.6;';

        // Different sample text based on type
        if (type === 'ui') {
            previewText.textContent = 'The quick brown fox jumps over the lazy dog 0123456789';
        } else if (type === 'editor') {
            previewText.innerHTML = `<div>function calculateTotal(items) {</div><div>  return items.reduce((sum, item) => sum + item.price, 0);</div><div>}</div>`;
            previewText.style.whiteSpace = 'pre';
        } else if (type === 'terminal') {
            previewText.innerHTML = `<div>$ npm run build</div><div>Building project...</div><div>✓ Build complete (2.3s)</div>`;
            previewText.style.whiteSpace = 'pre';
        }

        container.appendChild(label);
        container.appendChild(previewText);

        const update = (newSettings) => {
            const family = newSettings?.fontFamily || newSettings?.family || '';
            const size = newSettings?.fontSize || newSettings?.sizePx || 13;
            const lineHeight = newSettings?.lineHeight || 1.6;
            const weight = newSettings?.weight || '400';
            const ligatures = newSettings?.ligatures !== undefined ? newSettings.ligatures : false;

            if (family) {
                previewText.style.fontFamily = family === 'Inter' || family === 'Fira Code' ? family : `${family}, monospace`;
            }
            if (size) {
                previewText.style.fontSize = `${size}px`;
            }
            if (lineHeight) {
                previewText.style.lineHeight = String(lineHeight);
            }
            if (weight) {
                previewText.style.fontWeight = String(weight);
            }
            if (type === 'editor' || type === 'terminal') {
                previewText.style.fontVariantLigatures = ligatures ? 'normal' : 'none';
            }
        };

        // Initial update
        update(settings);

        return { element: container, update };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.settings = window.AhmadIDEModules.features.settings || {};
        window.AhmadIDEModules.features.settings.createFontPreview = createFontPreview;
    }
})();
