(() => {
    const loadScript = (src) => {
        const existing = document.querySelector(`script[data-src="${src}"]`) || document.querySelector(`script[src="${src}"]`);
        if (existing) return;
        if (document.readyState === 'loading') {
            document.write(`<script src="${src}"><\/script>`);
            return;
        }
        const s = document.createElement('script');
        s.src = src;
        s.async = false;
        s.dataset.src = src;
        document.head.appendChild(s);
    };

    if (typeof document !== 'undefined') {
        loadScript('./src/ui/components/menu/MenuBar.js');
    }
})();
