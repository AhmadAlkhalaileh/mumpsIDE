(() => {
    function createToggle(opts = {}) {
        const {
            label = '',
            checked = false,
            disabled = false,
            onChange = null
        } = opts;

        const row = document.createElement('label');
        row.className = 'ui-toggle';

        const input = document.createElement('input');
        input.type = 'checkbox';
        input.checked = !!checked;
        input.disabled = !!disabled;
        input.className = 'ui-toggle__input';

        const track = document.createElement('span');
        track.className = 'ui-toggle__track';
        const thumb = document.createElement('span');
        thumb.className = 'ui-toggle__thumb';
        track.appendChild(thumb);

        const text = document.createElement('span');
        text.className = 'ui-toggle__label';
        text.textContent = label;

        row.appendChild(input);
        row.appendChild(track);
        row.appendChild(text);

        if (typeof onChange === 'function') {
            input.addEventListener('change', (e) => onChange(e, input.checked));
        }

        return { root: row, input };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.primitives = window.AhmadIDEModules.ui.primitives || {};
        window.AhmadIDEModules.ui.primitives.createToggle = createToggle;
    }
})();

