(() => {
    const loadScript = (src) => {
        if (document.readyState === 'loading') {
            document.write(`<script src="${src}"><\/script>`);
            return;
        }

        const existing = document.querySelector(`script[data-src="${src}"]`);
        if (existing) return;

        const s = document.createElement('script');
        s.src = src;
        s.async = false;
        s.dataset.src = src;
        document.head.appendChild(s);
    };

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        if (!window.AhmadIDEModules.ui.__iconsLoaded) {
            window.AhmadIDEModules.ui.__iconsLoaded = true;
            loadScript('./src/ui/icons/icon-map.js');
            loadScript('./src/ui/components/Icon.js');
        }
    }
})();
