(() => {
    /**
     * Wire All Dialogs to Registry
     * Registers all dialog implementations with the dialog registry
     * and binds menu actions to dialog openers
     */
    function wireAllDialogs() {
        const dialogRegistry = window.AhmadIDEModules?.app?.dialogRegistry;
        if (!dialogRegistry) {
            console.error('Dialog registry not available');
            return;
        }

        // Settings Dialog (already wired in renderer-settings-panel.js)
        // Connections Dialog - DISABLED: Using panel-based UI instead (see renderer-connections.js)
        // The connectionsDialog.js is incomplete and shows an empty profile-based UI.
        // Clicking 'toggleConnections' opens the full SSH/Docker panel which works correctly.
        /*
        try {
            const createConnectionsDialog = window.AhmadIDEModules?.features?.connections?.createConnectionsDialog;
            if (createConnectionsDialog) {
                const connectionsDialog = createConnectionsDialog({});
                dialogRegistry.register({
                    id: 'connections',
                    title: 'Connections',
                    open: () => connectionsDialog.open()
                });
            }
        } catch (e) {
            console.error('Failed to register connections dialog:', e);
        }
        */

        // About Dialog
        try {
            const createAboutDialog = window.AhmadIDEModules?.features?.about?.createAboutDialog;
            if (createAboutDialog) {
                const aboutDialog = createAboutDialog({});
                dialogRegistry.register({
                    id: 'about',
                    title: 'About',
                    open: () => aboutDialog.open()
                });
            }
        } catch (e) {
            console.error('Failed to register about dialog:', e);
        }

        // Shortcuts Dialog
        try {
            const createShortcutsDialog = window.AhmadIDEModules?.features?.shortcuts?.createShortcutsDialog;
            if (createShortcutsDialog) {
                const shortcutsDialog = createShortcutsDialog({});
                dialogRegistry.register({
                    id: 'shortcuts',
                    title: 'Keyboard Shortcuts',
                    open: () => shortcutsDialog.open()
                });
            }
        } catch (e) {
            console.error('Failed to register shortcuts dialog:', e);
        }

        // New Project Dialog
        try {
            const createNewProjectDialog = window.AhmadIDEModules?.features?.projects?.createNewProjectDialog;
            if (createNewProjectDialog) {
                const newProjectDialog = createNewProjectDialog({});
                dialogRegistry.register({
                    id: 'new-project',
                    title: 'New Project',
                    open: () => newProjectDialog.open()
                });
            }
        } catch (e) {
            console.error('Failed to register new-project dialog:', e);
        }

        // Open Project Dialog
        try {
            const createOpenProjectDialog = window.AhmadIDEModules?.features?.projects?.createOpenProjectDialog;
            if (createOpenProjectDialog) {
                const openProjectDialog = createOpenProjectDialog({});
                dialogRegistry.register({
                    id: 'open-project',
                    title: 'Open Project',
                    open: () => openProjectDialog.open()
                });
            }
        } catch (e) {
            console.error('Failed to register open-project dialog:', e);
        }

        // Find/Replace Dialog
        try {
            const createFindReplaceDialog = window.AhmadIDEModules?.features?.search?.createFindReplaceDialog;
            if (createFindReplaceDialog) {
                const findReplaceDialog = createFindReplaceDialog({});
                dialogRegistry.register({
                    id: 'find',
                    title: 'Find',
                    open: () => findReplaceDialog.open('find')
                });
                dialogRegistry.register({
                    id: 'replace',
                    title: 'Replace',
                    open: () => findReplaceDialog.open('replace')
                });
            }
        } catch (e) {
            console.error('Failed to register find/replace dialog:', e);
        }

        // Search Everywhere Dialog
        try {
            const createSearchEverywhereDialog = window.AhmadIDEModules?.features?.search?.createSearchEverywhereDialog;
            if (createSearchEverywhereDialog) {
                const searchEverywhereDialog = createSearchEverywhereDialog({});
                dialogRegistry.register({
                    id: 'search-everywhere',
                    title: 'Search Everywhere',
                    open: () => searchEverywhereDialog.open()
                });
                dialogRegistry.register({
                    id: 'goto-file',
                    title: 'Go to File',
                    open: () => searchEverywhereDialog.open()
                });
            }
        } catch (e) {
            console.error('Failed to register search-everywhere dialog:', e);
        }

        console.log('[wireDialogs] Registered dialogs:', dialogRegistry.list());
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.app = window.AhmadIDEModules.app || {};
        window.AhmadIDEModules.app.wireAllDialogs = wireAllDialogs;

        // Auto-wire on load
        if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', wireAllDialogs);
        } else {
            setTimeout(wireAllDialogs, 100);
        }
    }
})();
