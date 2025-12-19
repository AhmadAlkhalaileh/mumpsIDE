(() => {
    function createSelect(opts = {}) {
        const {
            value = '',
            options = [],
            disabled = false,
            ariaLabel = '',
            onChange = null
        } = opts;

        const select = document.createElement('select');
        select.className = 'ui-select';
        select.disabled = !!disabled;
        if (ariaLabel) select.setAttribute('aria-label', ariaLabel);

        (options || []).forEach((opt) => {
            const o = document.createElement('option');
            if (typeof opt === 'string') {
                o.value = opt;
                o.textContent = opt;
            } else {
                o.value = String(opt.value);
                o.textContent = String(opt.label ?? opt.value);
                if (opt.disabled) o.disabled = true;
            }
            select.appendChild(o);
        });

        select.value = value ?? '';
        if (typeof onChange === 'function') select.addEventListener('change', (e) => onChange(e));
        return select;
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.primitives = window.AhmadIDEModules.ui.primitives || {};
        window.AhmadIDEModules.ui.primitives.createSelect = createSelect;
    }
})();

