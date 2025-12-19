(() => {
    const LABEL_RX = /^([A-Za-z%][A-Za-z0-9]*)(?=\s|;|\(|$)/;

    function createMumpsLocalTagResolver() {
        const byUri = new Map(); // uri -> { versionId, map }

        const getUri = (model) => {
            try {
                const uri = model?.uri?.toString?.();
                return uri || '';
            } catch (_) {
                return '';
            }
        };

        const buildIndex = (model) => {
            const map = new Map();
            const lineCount = model.getLineCount();
            for (let i = 1; i <= lineCount; i += 1) {
                const line = model.getLineContent(i);
                if (!line) continue;
                const m = line.match(LABEL_RX);
                if (!m) continue;
                const label = m[1];
                if (!label) continue;
                map.set(label.toUpperCase(), i);
            }
            return map;
        };

        const ensure = (model) => {
            if (!model?.getVersionId) return null;
            const uri = getUri(model);
            const versionId = model.getVersionId();
            const existing = byUri.get(uri);
            if (existing && existing.versionId === versionId) return existing.map;
            const map = buildIndex(model);
            byUri.set(uri, { versionId, map });
            return map;
        };

        const getTagLine = (model, tag) => {
            const map = ensure(model);
            if (!map || !tag) return null;
            return map.get(String(tag).toUpperCase()) || null;
        };

        return {
            getTagLine,
            hasTag: (model, tag) => !!getTagLine(model, tag),
            invalidate: (model) => {
                const uri = getUri(model);
                if (uri) byUri.delete(uri);
            }
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
        window.AhmadIDEModules.mumps.createMumpsLocalTagResolver = createMumpsLocalTagResolver;
    }
})();

