(() => {
    function createInput(opts = {}) {
        const {
            value = '',
            placeholder = '',
            type = 'text',
            disabled = false,
            ariaLabel = '',
            onInput = null,
            onChange = null
        } = opts;

        const input = document.createElement('input');
        input.className = 'ui-input';
        input.type = type;
        input.value = value ?? '';
        input.placeholder = placeholder ?? '';
        input.disabled = !!disabled;
        if (ariaLabel) input.setAttribute('aria-label', ariaLabel);

        if (typeof onInput === 'function') input.addEventListener('input', (e) => onInput(e));
        if (typeof onChange === 'function') input.addEventListener('change', (e) => onChange(e));

        return input;
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.primitives = window.AhmadIDEModules.ui.primitives || {};
        window.AhmadIDEModules.ui.primitives.createInput = createInput;
    }
})();

