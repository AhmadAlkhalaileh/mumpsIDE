(() => {
    const FOCUSABLE = [
        'a[href]',
        'button:not([disabled])',
        'input:not([disabled])',
        'select:not([disabled])',
        'textarea:not([disabled])',
        '[tabindex]:not([tabindex="-1"])'
    ].join(',');

    function createDialog(opts = {}) {
        const {
            ariaLabel = 'Dialog',
            closeOnEscape = true,
            closeOnBackdrop = false,
            onClose = null
        } = opts;

        const overlay = document.createElement('div');
        overlay.className = 'ui-dialog-overlay hidden';
        overlay.tabIndex = -1;

        const dialog = document.createElement('div');
        dialog.className = 'ui-dialog';
        dialog.setAttribute('role', 'dialog');
        dialog.setAttribute('aria-modal', 'true');
        dialog.setAttribute('aria-label', ariaLabel);

        overlay.appendChild(dialog);

        let isOpen = false;
        let lastActive = null;

        const getFocusable = () => Array.from(dialog.querySelectorAll(FOCUSABLE))
            .filter((el) => !!(el.offsetParent || el === document.activeElement));

        const focusFirst = () => {
            const els = getFocusable();
            (els[0] || dialog).focus();
        };

        const onKeyDown = (e) => {
            if (!isOpen) return;
            if (closeOnEscape && e.key === 'Escape') {
                e.preventDefault();
                close('escape');
                return;
            }
            if (e.key !== 'Tab') return;

            const els = getFocusable();
            if (!els.length) {
                e.preventDefault();
                dialog.focus();
                return;
            }
            const first = els[0];
            const last = els[els.length - 1];
            const active = document.activeElement;
            if (e.shiftKey) {
                if (active === first || active === dialog) {
                    e.preventDefault();
                    last.focus();
                }
            } else if (active === last) {
                e.preventDefault();
                first.focus();
            }
        };

        const onMouseDown = (e) => {
            if (!closeOnBackdrop) return;
            if (e.target === overlay) close('backdrop');
        };

        const open = () => {
            if (isOpen) return;
            isOpen = true;
            lastActive = document.activeElement;
            overlay.classList.remove('hidden');
            document.body.appendChild(overlay);
            document.addEventListener('keydown', onKeyDown, true);
            overlay.addEventListener('mousedown', onMouseDown);
            requestAnimationFrame(() => focusFirst());
        };

        const close = (reason = 'close') => {
            if (!isOpen) return;
            isOpen = false;
            document.removeEventListener('keydown', onKeyDown, true);
            overlay.removeEventListener('mousedown', onMouseDown);
            overlay.classList.add('hidden');
            try { overlay.remove(); } catch (_) { }
            try { lastActive?.focus?.(); } catch (_) { }
            if (typeof onClose === 'function') {
                try { onClose(reason); } catch (_) { }
            }
        };

        const setContent = (node) => {
            dialog.innerHTML = '';
            if (node) dialog.appendChild(node);
        };

        return { overlay, dialog, open, close, setContent, isOpen: () => isOpen };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.createDialog = createDialog;
    }
})();

