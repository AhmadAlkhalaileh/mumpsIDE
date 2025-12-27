(() => {
    function createMenuBarWiring({ deps } = {}) {
        const logger = deps?.logger;
        const runMenuAction = deps?.runMenuAction;

        function wireMenuBar(editor, routineState, terminalState) {
            const buildMenuBar = () => {
                const host = document.getElementById('mainMenu');
                if (!host) {
                    logger.error('MENU_HOST_MISSING', { selector: '#mainMenu' });
                    return;
                }

                const menuRegistry = window.AhmadIDEModules?.app?.menuRegistry;
                const createMenuController = window.AhmadIDEModules?.ui?.menu?.createMenuController;
                const createMenuBar = window.AhmadIDEModules?.ui?.menu?.createMenuBar;

                if (!menuRegistry || !createMenuController || !createMenuBar) {
                    logger.error('MENU_SYSTEM_MISSING', {
                        menuRegistry: !!menuRegistry,
                        createMenuController: !!createMenuController,
                        createMenuBar: !!createMenuBar
                    });
                    return;
                }

                const menus = menuRegistry.get('menubar');
                if (!menus || menus.length === 0) {
                    logger.error('MENU_REGISTRY_EMPTY', { id: 'menubar' });
                    return;
                }

                const controller = createMenuController({});
                const panelVisible = (panelId) => {
                    const el = document.getElementById(panelId);
                    if (!el) return false;
                    if (el.classList.contains('hidden')) return false;
                    const hostEl = el.closest?.('#leftToolWindow, #rightToolWindow, #bottomToolWindow');
                    if (!hostEl) return true;
                    return !hostEl.classList.contains('hidden');
                };

                const menuBar = createMenuBar({
                    host,
                    menus,
                    controller,
                    onAction: async (action, menuCtx) => {
                        await runMenuAction(action, {
                            ...(menuCtx || {}),
                            editor,
                            routineState,
                            terminalState
                        });
                    },
                    getContext: () => ({
                        toolWindows: {
                            panels: {
                                projectPanel: panelVisible('projectPanel'),
                                structurePanel: panelVisible('structurePanel'),
                                commitPanel: panelVisible('commitPanel'),
                                terminalToolPanel: panelVisible('terminalToolPanel'),
                                terminalPanel: panelVisible('terminalPanel'),
                                debugPanel: panelVisible('debugPanel'),
                                problemsPanel: panelVisible('problemsPanel'),
                                gitToolPanel: panelVisible('gitToolPanel'),
                                extensionsPanel: panelVisible('extensionsPanel'),
                                servicesPanel: panelVisible('servicesPanel'),
                                patchTrackingPanel: panelVisible('patchTrackingPanel')
                            }
                        }
                    })
                });

                menuBar.mount();
            };

            buildMenuBar();
        }

        return { wireMenuBar };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.menu = window.AhmadIDEModules.renderer.menu || {};
        window.AhmadIDEModules.renderer.menu.createMenuBarWiring = createMenuBarWiring;
    }
})();
