(() => {
    function createButton(opts = {}) {
        const {
            label = '',
            variant = 'default', // default | primary | ghost | danger
            size = 'md', // sm | md
            icon = null,
            title = '',
            disabled = false,
            onClick = null,
            type = 'button'
        } = opts;

        const btn = document.createElement('button');
        btn.type = type;
        btn.className = `ui-btn ui-btn--${variant} ui-btn--${size}`;
        if (title) btn.title = title;
        btn.disabled = !!disabled;

        if (icon) {
            const ic = document.createElement('span');
            ic.className = 'ui-btn__icon';
            if (typeof icon === 'string') ic.innerHTML = icon;
            else ic.appendChild(icon);
            btn.appendChild(ic);
        }

        const text = document.createElement('span');
        text.className = 'ui-btn__label';
        text.textContent = label;
        btn.appendChild(text);

        if (typeof onClick === 'function') {
            btn.addEventListener('click', (e) => onClick(e));
        }

        return btn;
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.primitives = window.AhmadIDEModules.ui.primitives || {};
        window.AhmadIDEModules.ui.primitives.createButton = createButton;
    }
})();

