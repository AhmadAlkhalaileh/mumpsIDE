(() => {
    /**
     * Terminal header controller for -style UI
     * Manages the terminal menu dropdown
     */
    function initTerminalHeader() {
        const menuBtn = document.getElementById('terminalMenuBtn');
        const menuDropdown = document.getElementById('terminalMenuDropdown');

        if (!menuBtn || !menuDropdown) return;

        // Toggle menu dropdown
        menuBtn.addEventListener('click', (e) => {
            e.stopPropagation();
            menuDropdown.classList.toggle('hidden');
        });

        // Close menu when clicking outside
        document.addEventListener('click', (e) => {
            if (!menuDropdown.classList.contains('hidden') &&
                !menuBtn.contains(e.target) &&
                !menuDropdown.contains(e.target)) {
                menuDropdown.classList.add('hidden');
            }
        });

        // Handle menu item clicks
        menuDropdown.addEventListener('click', (e) => {
            const item = e.target.closest('.menu-item');
            if (!item) return;

            const action = item.dataset.action;
            menuDropdown.classList.add('hidden');

            // Dispatch actions
            switch (action) {
                case 'new-tab':
                    document.getElementById('terminalNewTabBtn')?.click();
                    break;
                case 'split':
                    // TODO: Implement split terminal
                    console.log('Split terminal not yet implemented');
                    break;
                case 'clear':
                    // Clear button already has its own handler
                    break;
                case 'close':
                    // Close current terminal tab
                    const state = getGlobalTerminalState();
                    if (state && state.active) {
                        const closeTabFn = window.AhmadIDEModules?.terminal?.closeTerminalTab;
                        if (closeTabFn) {
                            closeTabFn(state, state.active);
                        }
                    }
                    break;
            }
        });

        return {};
    }

    function getGlobalTerminalState() {
        return window.AhmadIDEModules?.terminal?.globalTerminalState;
    }

    // Export to window for access from other modules
    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.terminal = window.AhmadIDEModules.terminal || {};
        window.AhmadIDEModules.terminal.initTerminalHeader = initTerminalHeader;
    }
})();
