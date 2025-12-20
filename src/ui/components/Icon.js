(() => {
    const getIcons = () => window.AhmadIDEModules?.ui?.icons;

    const createIcon = (name, opts = {}) => {
        const icons = getIcons();
        if (!icons?.createSvgElement) return document.createElement('span');
        return icons.createSvgElement(name, opts);
    };

    let statusObserver = null;
    let dataIconObserver = null;
    let dataIconDebounce = null;

    const hydrateDataIcons = () => {
        const icons = getIcons();
        if (!icons?.createSvgElement) return;

        document.querySelectorAll('[data-ui-icon]').forEach((host) => {
            if (!host || host.dataset?.uiIconHydrated === '1') return;
            if (host.querySelector?.('svg')) {
                host.dataset.uiIconHydrated = '1';
                return;
            }

            const name = String(host.getAttribute('data-ui-icon') || '').trim();
            if (!name) return;
            const sizeAttr = host.getAttribute('data-ui-icon-size');
            const size = Number(sizeAttr || 16) || 16;
            host.textContent = '';
            host.appendChild(createIcon(name, { size }));
            host.dataset.uiIconHydrated = '1';
        });
    };

    const scheduleHydrateDataIcons = () => {
        if (dataIconDebounce) return;
        dataIconDebounce = setTimeout(() => {
            dataIconDebounce = null;
            hydrateDataIcons();
        }, 50);
    };

    const ensureDataIconHydration = () => {
        if (dataIconObserver || typeof MutationObserver === 'undefined') return;

        const start = () => {
            if (dataIconObserver) return;
            dataIconObserver = new MutationObserver((mutations) => {
                for (const m of mutations) {
                    for (const node of m.addedNodes || []) {
                        if (!node || node.nodeType !== 1) continue;
                        const el = /** @type {Element} */ (node);
                        if (el.matches?.('[data-ui-icon]') || el.querySelector?.('[data-ui-icon]')) {
                            scheduleHydrateDataIcons();
                            return;
                        }
                    }
                }
            });

            const root = document.body || document.documentElement;
            if (!root) return;
            dataIconObserver.observe(root, { childList: true, subtree: true });
            scheduleHydrateDataIcons();
        };

        if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', start, { once: true });
        } else {
            start();
        }
    };

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
        hydrateDataIcons();

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
        window.AhmadIDEModules.ui.components.Icon = { createIcon, hydrateDataIcons };
        window.AhmadIDEModules.ui.icons = window.AhmadIDEModules.ui.icons || {};
        window.AhmadIDEModules.ui.icons.createIcon = createIcon;
        ensureStatusIcons();
        ensureDataIconHydration();
    }
})();
