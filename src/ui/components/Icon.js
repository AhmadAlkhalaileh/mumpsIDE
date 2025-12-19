(() => {
    const getIcons = () => window.AhmadIDEModules?.ui?.icons;

    const createIcon = (name, opts = {}) => {
        const icons = getIcons();
        if (!icons?.createSvgElement) return document.createElement('span');
        return icons.createSvgElement(name, opts);
    };

    let statusObserver = null;

    const replaceStatusIcons = () => {
        const replace = (host, iconName, size) => {
            if (!host) return;
            const existingIcon = host.querySelector('.icon');
            if (existingIcon && existingIcon.querySelector('svg')) return;

            let label = host.textContent || '';
            if (existingIcon?.textContent) {
                label = label.replace(existingIcon.textContent, '');
            }
            label = label.replace(/\s+/g, ' ').trim();

            host.textContent = '';
            const iconWrap = document.createElement('span');
            iconWrap.className = 'icon';
            iconWrap.appendChild(createIcon(iconName, { size }));
            host.appendChild(iconWrap);
            if (label) host.appendChild(document.createTextNode(` ${label}`));
        };

        const gitHost = document.getElementById('gitBranch');
        const problemsHost = document.getElementById('problemsSummary');
        replace(gitHost, 'git', 12);
        replace(problemsHost, 'warning', 12);

        if (statusObserver) {
            statusObserver.disconnect();
            [gitHost, problemsHost].filter(Boolean).forEach((node) => {
                statusObserver.observe(node, { childList: true, subtree: true });
            });
        }
    };

    const ensureStatusIcons = () => {
        if (!statusObserver && typeof MutationObserver !== 'undefined') {
            statusObserver = new MutationObserver(() => replaceStatusIcons());
        }

        if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', replaceStatusIcons, { once: true });
        } else {
            replaceStatusIcons();
        }
    };

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.components = window.AhmadIDEModules.ui.components || {};
        window.AhmadIDEModules.ui.components.Icon = { createIcon };
        window.AhmadIDEModules.ui.icons = window.AhmadIDEModules.ui.icons || {};
        window.AhmadIDEModules.ui.icons.createIcon = createIcon;
        ensureStatusIcons();
    }
})();
