(() => {
    function createVirtualizedList(host, opts = {}) {
        const createVirtualList = window.AhmadIDEModules?.ui?.createVirtualList;
        if (!createVirtualList) {
            throw new Error('Virtual list missing: src/ui/virtual-list.js');
        }
        return createVirtualList(host, opts);
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.primitives = window.AhmadIDEModules.ui.primitives || {};
        window.AhmadIDEModules.ui.primitives.createVirtualizedList = createVirtualizedList;
    }
})();

