(() => {
    function createFeatureRegistry() {
        const nativeGetElementById = document.getElementById.bind(document);
        const definitions = new Map(); // containerId -> { html?: string|() => string, mount?: (el) => void, onMount?: (el) => void }
        const mountCallbacks = new Map(); // containerId -> Set<fn>

        const isMounted = (containerId) => {
            const el = nativeGetElementById(containerId);
            return !!(el && el.dataset.lazyMounted === '1');
        };

        const register = (containerId, def) => {
            if (!containerId || !def) return;
            definitions.set(containerId, def);
        };

        const onMounted = (containerId, cb) => {
            if (!containerId || typeof cb !== 'function') return;
            if (isMounted(containerId)) {
                queueMicrotask(() => cb(nativeGetElementById(containerId)));
                return;
            }
            const set = mountCallbacks.get(containerId) || new Set();
            set.add(cb);
            mountCallbacks.set(containerId, set);
        };

        const flushMounted = (containerId, el) => {
            const set = mountCallbacks.get(containerId);
            if (set && set.size) {
                mountCallbacks.delete(containerId);
                set.forEach((cb) => {
                    try {
                        cb(el);
                    } catch (_) {
                        // ignore callback failures
                    }
                });
            }
            try {
                window.dispatchEvent(new CustomEvent('ahmadIDE:featureMounted', { detail: { id: containerId } }));
            } catch (_) {
                // ignore
            }
        };

        const ensureById = (containerId) => {
            const el = nativeGetElementById(containerId);
            if (!el) return false;
            if (el.dataset.lazyMounted === '1') return true;

            const def = definitions.get(containerId);

            // Back-compat: support <template> mounting via data-lazy-template="tplId"
            const templateId = el.dataset.lazyTemplate;
            const tpl = templateId ? nativeGetElementById(templateId) : null;

            try {
                if (def) {
                    if (typeof def.mount === 'function') {
                        def.mount(el);
                    } else {
                        const html = typeof def.html === 'function' ? def.html() : def.html;
                        if (typeof html === 'string' && html) {
                            const t = document.createElement('template');
                            t.innerHTML = html;
                            el.appendChild(t.content.cloneNode(true));
                        }
                    }
                    el.dataset.lazyMounted = '1';
                    if (typeof def.onMount === 'function') {
                        try {
                            def.onMount(el);
                        } catch (_) {
                            // ignore onMount failures
                        }
                    }
                    flushMounted(containerId, el);
                    return true;
                }

                if (tpl && tpl.tagName === 'TEMPLATE') {
                    el.appendChild(tpl.content.cloneNode(true));
                    el.dataset.lazyMounted = '1';
                    flushMounted(containerId, el);
                    return true;
                }
            } catch (_) {
                return false;
            }

            return false;
        };

        return { register, ensureById, onMounted, isMounted };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.app = window.AhmadIDEModules.app || {};
        if (!window.AhmadIDEModules.app.featureRegistry) {
            window.AhmadIDEModules.app.featureRegistry = createFeatureRegistry();
        }
    }
})();
