(() => {
    const openPopover = ({ controller, anchorEl, items, ctx, onAction, rootMeta } = {}) => {
        const menu = window.AhmadIDEModules?.ui?.menu;
        const ctrl = controller || menu?.controller || menu?.createMenuController?.({});
        if (!ctrl || typeof ctrl.openAtElement !== 'function') return null;
        return ctrl.openAtElement({ anchorEl, items, ctx, onAction, rootMeta });
    };

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.menu = window.AhmadIDEModules.ui.menu || {};
        window.AhmadIDEModules.ui.menu.openPopover = openPopover;
    }
})();
