(() => {
    function createMenuBar({ host, menus, controller, onAction, getContext } = {}) {
        if (!host) throw new Error('MenuBar requires host element');
        if (!controller) throw new Error('MenuBar requires menu controller');
        if (!Array.isArray(menus)) throw new Error('MenuBar requires menus array');

        let hamburgerBtn = null;
        let menuContainer = null;
        let isOpen = false;
        let closeTimer = null;

        const closeMenu = () => {
            if (!isOpen) return;
            isOpen = false;
            hamburgerBtn?.classList.remove('active');
            menuContainer?.classList.add('hidden');
        };

        const openMenu = () => {
            if (isOpen) return;
            isOpen = true;
            hamburgerBtn?.classList.add('active');
            menuContainer?.classList.remove('hidden');

            // Close when clicking outside
            const outsideClick = (e) => {
                if (!host.contains(e.target) && !document.querySelector('.ui-menu')?.contains(e.target)) {
                    closeMenu();
                    document.removeEventListener('pointerdown', outsideClick, true);
                }
            };
            setTimeout(() => document.addEventListener('pointerdown', outsideClick, true), 10);
        };

        const toggleMenu = () => {
            isOpen ? closeMenu() : openMenu();
        };

        const mount = () => {
            host.innerHTML = '';
            host.classList.add('ui-menubar');
            host.setAttribute('role', 'menubar');

            // 1. Create Hamburger Button
            const btn = document.createElement('button');
            btn.type = 'button';
            btn.className = 'ui-menubar-hamburger';
            btn.setAttribute('aria-label', 'Main Menu');
            btn.innerHTML = `
                <svg width="18" height="18" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path d="M3 12H21" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                    <path d="M3 6H21" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                    <path d="M3 18H21" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                </svg>
            `;

            // ONLY click to open - no hover on hamburger
            btn.addEventListener('click', (e) => {
                e.preventDefault();
                e.stopPropagation();
                toggleMenu();
            });

            hamburgerBtn = btn;
            host.appendChild(btn);

            // 2. Create Horizontal Menu Container
            const container = document.createElement('div');
            container.className = 'ui-menubar-horizontal-dropdown hidden';

            menus.forEach(m => {
                const menuBtn = document.createElement('button');
                menuBtn.className = 'ui-menubar-item';
                menuBtn.textContent = m.label || m.id;

                // Click to open submenu
                menuBtn.addEventListener('click', (e) => {
                    e.stopPropagation();
                    const ctx = typeof getContext === 'function' ? getContext() : {};
                    controller.openAtElement({
                        anchorEl: menuBtn,
                        items: m.items || [],
                        ctx,
                        onAction: (action, ctx) => {
                            closeMenu();
                            if (onAction) onAction(action, ctx);
                        }
                    });
                });

                // Hover opens submenu (once menu bar is visible)
                menuBtn.addEventListener('pointerenter', () => {
                    if (!isOpen) return;
                    // Check if controller has an open menu, switch to this one
                    if (controller.isOpen?.()) {
                        const ctx = typeof getContext === 'function' ? getContext() : {};
                        controller.openAtElement({
                            anchorEl: menuBtn,
                            items: m.items || [],
                            ctx,
                            onAction: (action, ctx) => {
                                closeMenu();
                                if (onAction) onAction(action, ctx);
                            }
                        });
                    }
                });

                container.appendChild(menuBtn);
            });

            menuContainer = container;
            host.appendChild(container);
        };

        return { mount, focusBar: () => hamburgerBtn?.focus(), blurBar: closeMenu };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.menu = window.AhmadIDEModules.ui.menu || {};
        window.AhmadIDEModules.ui.menu.createMenuBar = createMenuBar;
    }
})();
