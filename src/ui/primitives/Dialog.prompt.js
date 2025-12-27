(() => {
    /**
     * Prompt & Confirm Dialog Helpers
     * Generic reusable dialogs for quick user input
     * Matches  2025.3 New UI prompt/confirm patterns
     *
     * Checklist: PRM-001 to PRM-005
     */
    function createPromptConfirmDialogs({ deps } = {}) {
        const createDialog = deps?.createDialog || window.AhmadIDEModules?.ui?.createDialog;
        const primitives = deps?.primitives || window.AhmadIDEModules?.ui?.primitives;

        if (!createDialog || !primitives) {
            throw new Error('PromptConfirmDialogs require ui primitives');
        }

        const buildDialog = ({ title, message, variant = 'default', inputPlaceholder = '', showInput = false }) => {
            const wrapper = document.createElement('div');
            wrapper.className = 'ui-dialog-layout';
            wrapper.style.cssText = 'max-width:380px;width:380px;';

            const header = document.createElement('div');
            header.className = 'ui-dialog-header';
            header.style.cssText = 'height:40px;padding:0 var(--ui-space-3);';
            const headerLeft = document.createElement('div');
            headerLeft.className = 'ui-dialog-header__left';
            const titleEl = document.createElement('div');
            titleEl.className = 'ui-dialog-title';
            titleEl.style.cssText = 'font-size:13px;';
            titleEl.textContent = title || 'Confirm';
            headerLeft.appendChild(titleEl);

            const headerRight = document.createElement('div');
            headerRight.className = 'ui-dialog-header__right';
            const closeBtn = document.createElement('button');
            closeBtn.className = 'ui-dialog-close';
            closeBtn.type = 'button';
            closeBtn.title = 'Close';
            closeBtn.style.cssText = 'width:24px;height:24px;font-size:14px;';
            closeBtn.textContent = '✕';
            headerRight.appendChild(closeBtn);

            header.appendChild(headerLeft);
            header.appendChild(headerRight);

            const body = document.createElement('div');
            body.style.cssText = 'padding:var(--ui-space-3);';

            const msg = document.createElement('div');
            msg.style.cssText = 'margin-bottom:var(--ui-space-2);color:var(--text);line-height:1.4;font-size:12px;';
            msg.textContent = message || '';
            body.appendChild(msg);

            let inputEl = null;
            if (showInput) {
                const { createInput } = primitives;
                inputEl = createInput({ placeholder: inputPlaceholder || '', value: '' });
                body.appendChild(inputEl);
            }

            const footer = document.createElement('div');
            footer.className = 'ui-dialog-footer';
            footer.style.cssText = 'padding:var(--ui-space-2) var(--ui-space-3);height:auto;min-height:42px;';

            wrapper.appendChild(header);
            wrapper.appendChild(body);
            wrapper.appendChild(footer);

            return { wrapper, closeBtn, footer, inputEl };
        };

        const showPrompt = ({ title, message, placeholder = '', defaultValue = '' } = {}) => {
            return new Promise((resolve) => {
                const dialog = createDialog({
                    ariaLabel: title || 'Prompt',
                    closeOnEscape: true,
                    closeOnBackdrop: false,
                    onClose: (reason) => {
                        if (reason === 'ok') return; // already resolved
                        resolve(null);
                    }
                });

                const { wrapper, closeBtn, footer, inputEl } = buildDialog({
                    title,
                    message,
                    inputPlaceholder: placeholder,
                    showInput: true
                });

                if (inputEl && defaultValue) {
                    inputEl.value = defaultValue;
                }

                const { createButton } = primitives;
                const cancelBtn = createButton({
                    label: 'Cancel',
                    variant: 'ghost',
                    onClick: () => {
                        resolve(null);
                        dialog.close('cancel');
                    }
                });
                const okBtn = createButton({
                    label: 'OK',
                    variant: 'primary',
                    onClick: () => {
                        resolve(inputEl?.value || '');
                        dialog.close('ok');
                    }
                });

                closeBtn.addEventListener('click', () => {
                    resolve(null);
                    dialog.close('x');
                });

                // Add Enter key support
                if (inputEl) {
                    inputEl.addEventListener('keydown', (e) => {
                        if (e.key === 'Enter' && !e.shiftKey) {
                            e.preventDefault();
                            resolve(inputEl.value || '');
                            dialog.close('ok');
                        }
                    });
                }

                footer.appendChild(cancelBtn);
                footer.appendChild(okBtn);

                dialog.setContent(wrapper);
                // Make the outer dialog container compact
                dialog.dialog.style.width = '380px';
                dialog.dialog.style.height = 'auto';
                dialog.open();

                requestAnimationFrame(() => inputEl?.focus?.());
            });
        };

        const showConfirm = ({ title, message, variant = 'default', confirmLabel = 'Yes', cancelLabel = 'No' } = {}) => {
            return new Promise((resolve) => {
                const dialog = createDialog({
                    ariaLabel: title || 'Confirm',
                    closeOnEscape: true,
                    closeOnBackdrop: false,
                    onClose: (reason) => {
                        if (reason === 'ok') return; // already resolved
                        resolve(false);
                    }
                });

                const { wrapper, closeBtn, footer } = buildDialog({
                    title,
                    message,
                    variant,
                    showInput: false
                });

                const { createButton } = primitives;
                const cancelBtn = createButton({
                    label: cancelLabel,
                    variant: 'ghost',
                    onClick: () => {
                        resolve(false);
                        dialog.close('cancel');
                    }
                });
                const okBtn = createButton({
                    label: confirmLabel,
                    variant: variant === 'danger' ? 'danger' : 'primary',
                    onClick: () => {
                        resolve(true);
                        dialog.close('ok');
                    }
                });

                closeBtn.addEventListener('click', () => {
                    resolve(false);
                    dialog.close('x');
                });

                footer.appendChild(cancelBtn);
                footer.appendChild(okBtn);

                dialog.setContent(wrapper);
                // Make the outer dialog container compact
                dialog.dialog.style.width = '380px';
                dialog.dialog.style.height = 'auto';
                dialog.open();
            });
        };

        return { showPrompt, showConfirm };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        const helpers = createPromptConfirmDialogs({});
        window.AhmadIDEModules.ui.showPrompt = helpers.showPrompt;
        window.AhmadIDEModules.ui.showConfirm = helpers.showConfirm;
    }
})();
