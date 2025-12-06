(() => {
    const isDevEnv = (() => {
        try {
            if (typeof process !== 'undefined' && process.env) {
                if (process.env.NODE_ENV === 'development') return true;
                if (process.env.ELECTRON_IS_DEV === '1') return true;
                if (process.env.MIDE_ENABLE_LOGS === '1') return true;
            }
            if (typeof window !== 'undefined' && window.localStorage) {
                return window.localStorage.getItem('MIDE_ENABLE_LOGS') === '1';
            }
        } catch (_) {
            // ignore env detection errors
        }
        return false;
    })();

    const stamp = () => new Date().toISOString();
    const prefix = (level) => `[MIDE][${level}][${stamp()}]`;

    const createLogger = () => {
        const log = (level, ctx, details) => {
            if (!isDevEnv) return;
            const msg = `${prefix(level)} ${ctx || ''}`;
            if (level === 'ERROR') {
                console.error(msg, details);
            } else if (level === 'WARN') {
                console.warn(msg, details);
            } else if (level === 'INFO') {
                console.info(msg, details);
            } else {
                console.debug(msg, details);
            }
        };

        return {
            debug: (ctx, details) => log('DEBUG', ctx, details),
            info: (ctx, details) => log('INFO', ctx, details),
            warn: (ctx, details) => log('WARN', ctx, details),
            error: (ctx, details) => log('ERROR', ctx, details),
            isEnabled: () => isDevEnv
        };
    };

    const logger = createLogger();

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { logger };
    }
    if (typeof window !== 'undefined') {
        window.MIDELogger = logger;
    }
})();
