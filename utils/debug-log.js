// utils/debug-log.js
// Centralized debug logging for Monaco debugger

(() => {
    const isDebugEnabled = (() => {
        try {
            if (typeof process !== 'undefined' && process.env) {
                if (process.env.MIDE_DEBUG_TRACE === '1') return true;
            }
            if (typeof window !== 'undefined' && window.localStorage) {
                return window.localStorage.getItem('MIDE_DEBUG_TRACE') === '1';
            }
        } catch (_) {
            // ignore
        }
        return false;
    })();

    const timestamp = () => new Date().toISOString();

    const log = (...args) => {
        if (!isDebugEnabled) return;
        const ts = timestamp();
        console.debug(`[DBG ${ts}]`, ...args);
    };

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { log };
    }
    if (typeof window !== 'undefined') {
        window.MIDEDebugLog = log;
    }
})();
