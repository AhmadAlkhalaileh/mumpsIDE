(() => {
    /**
     * About Dialog
     * Simple centered dialog with app info
     *
     * Checklist: ABT-001 to ABT-007
     */
    function createAboutDialog({ deps } = {}) {
        const createDialog = deps?.createDialog || window.AhmadIDEModules?.ui?.createDialog;
        const primitives = deps?.primitives || window.AhmadIDEModules?.ui?.primitives;

        if (!createDialog || !primitives) {
            throw new Error('AboutDialog requires ui primitives');
        }

        let dialogApi = null;

        const buildContent = () => {
            const container = document.createElement('div');
            container.className = 'about-dialog-content';
            container.innerHTML = `
                <style>
                .about-dialog-content {
                    padding: var(--ui-space-7);
                    text-align: center;
                    min-width: 420px;
                }
                .about-dialog-content__logo {
                    width: 80px;
                    height: 80px;
                    margin: 0 auto var(--ui-space-5);
                    background: linear-gradient(135deg, var(--accent-soft-border), var(--accent-soft));
                    border-radius: var(--ui-radius-3);
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    font-size: 36px;
                    font-weight: 700;
                    color: var(--accent);
                }
                .about-dialog-content__title {
                    font-size: 24px;
                    font-weight: 600;
                    color: var(--text-bright);
                    margin-bottom: var(--ui-space-2);
                }
                .about-dialog-content__version {
                    font-size: 14px;
                    color: rgba(255, 255, 255, 0.65);
                    margin-bottom: var(--ui-space-5);
                }
                .about-dialog-content__section {
                    margin-bottom: var(--ui-space-4);
                    padding-top: var(--ui-space-4);
                    border-top: 1px solid var(--ui-border-subtle);
                }
                .about-dialog-content__section-title {
                    font-size: 12px;
                    text-transform: uppercase;
                    letter-spacing: 0.5px;
                    color: rgba(255, 255, 255, 0.5);
                    margin-bottom: var(--ui-space-2);
                }
                .about-dialog-content__section-value {
                    font-size: 14px;
                    color: rgba(255, 255, 255, 0.85);
                }
                .about-dialog-content__footer {
                    display: flex;
                    justify-content: center;
                    gap: var(--ui-space-3);
                    margin-top: var(--ui-space-6);
                }
                </style>
                <div class="about-dialog-content__logo">M</div>
                <div class="about-dialog-content__title">Mumps Studio</div>
                <div class="about-dialog-content__version">Version 1.1</div>

                <div class="about-dialog-content__section">
                    <div class="about-dialog-content__section-title">Author</div>
                    <div class="about-dialog-content__section-value">Ahmad Alkhalaileh</div>
                </div>

                <div class="about-dialog-content__section">
                    <div class="about-dialog-content__section-title">Build</div>
                    <div class="about-dialog-content__section-value">Build #MS-1.1.${new Date().toISOString().slice(0, 10).replace(/-/g, '')}</div>
                    <div class="about-dialog-content__section-value" style="margin-top:4px;font-size:12px;color:rgba(255,255,255,0.5)">
                        ${new Date().toLocaleDateString('en-US', { year: 'numeric', month: 'long', day: 'numeric' })}
                    </div>
                </div>

                <div class="about-dialog-content__section">
                    <div class="about-dialog-content__section-title">License</div>
                    <div class="about-dialog-content__section-value">Proprietary - All Rights Reserved</div>
                </div>

                <div class="about-dialog-content__section">
                    <div class="about-dialog-content__section-title">Runtime</div>
                    <div class="about-dialog-content__section-value">
                        Node.js ${process?.versions?.node || 'Unknown'}<br>
                        Electron ${process?.versions?.electron || 'Unknown'}<br>
                        Chrome ${process?.versions?.chrome || 'Unknown'}
                    </div>
                </div>

                <div class="about-dialog-content__footer">
                    <button class="ui-btn ui-btn--ghost" id="aboutCopyBtn">Copy Info</button>
                </div>
            `;

            container.querySelector('#aboutCopyBtn')?.addEventListener('click', () => {
                const info = `
Mumps Studio v1.1
Author: Ahmad Alkhalaileh
Build: MS-1.1.${new Date().toISOString().slice(0, 10).replace(/-/g, '')}
Node.js: ${process?.versions?.node || 'Unknown'}
Electron: ${process?.versions?.electron || 'Unknown'}
Chrome: ${process?.versions?.chrome || 'Unknown'}
                `.trim();
                navigator.clipboard?.writeText(info);
                const btn = container.querySelector('#aboutCopyBtn');
                if (btn) {
                    const orig = btn.textContent;
                    btn.textContent = 'Copied!';
                    setTimeout(() => { btn.textContent = orig; }, 1500);
                }
            });

            return container;
        };

        const ensureDialog = () => {
            if (dialogApi) return;

            dialogApi = createDialog({
                ariaLabel: 'About Mumps Studio',
                closeOnEscape: true,
                closeOnBackdrop: true,
                onClose: () => { }
            });

            const wrapper = document.createElement('div');
            wrapper.className = 'ui-dialog-layout';

            const header = document.createElement('div');
            header.className = 'ui-dialog-header';
            const headerLeft = document.createElement('div');
            headerLeft.className = 'ui-dialog-header__left';
            const title = document.createElement('div');
            title.className = 'ui-dialog-title';
            title.textContent = 'About';
            headerLeft.appendChild(title);

            const headerRight = document.createElement('div');
            headerRight.className = 'ui-dialog-header__right';
            const closeBtn = document.createElement('button');
            closeBtn.className = 'ui-dialog-close';
            closeBtn.type = 'button';
            closeBtn.title = 'Close';
            closeBtn.textContent = '✕';
            closeBtn.addEventListener('click', () => dialogApi.close('x'));
            headerRight.appendChild(closeBtn);

            header.appendChild(headerLeft);
            header.appendChild(headerRight);

            const body = document.createElement('div');
            body.style.cssText = 'flex:1;overflow:auto;';
            body.appendChild(buildContent());

            const footer = document.createElement('div');
            footer.className = 'ui-dialog-footer';
            const { createButton } = primitives;
            const okBtn = createButton({ label: 'OK', variant: 'primary', onClick: () => dialogApi.close('ok') });
            footer.appendChild(okBtn);

            wrapper.appendChild(header);
            wrapper.appendChild(body);
            wrapper.appendChild(footer);

            dialogApi.setContent(wrapper);
        };

        const open = () => {
            ensureDialog();
            dialogApi.open();
        };

        return { open };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.about = window.AhmadIDEModules.features.about || {};
        window.AhmadIDEModules.features.about.createAboutDialog = createAboutDialog;
    }
})();
