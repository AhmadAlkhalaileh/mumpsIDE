(() => {
    function createCheckbox(opts = {}) {
        const {
            label = '',
            checked = false,
            disabled = false,
            onChange = null
        } = opts;

        const row = document.createElement('label');
        row.className = 'ui-check';
        const input = document.createElement('input');
        input.type = 'checkbox';
        input.checked = !!checked;
        input.disabled = !!disabled;
        const text = document.createElement('span');
        text.className = 'ui-check__label';
        text.textContent = label;
        row.appendChild(input);
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
        window.AhmadIDEModules.ui.primitives.createCheckbox = createCheckbox;
    }
})();

