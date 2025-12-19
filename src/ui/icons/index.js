(() => {
    const ICONS = {
        'add': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M8 3.2v9.6M3.2 8h9.6" stroke="currentColor" stroke-width="1.6" stroke-linecap="round"/> </svg>',
        'arrow-right': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M4 8h7.5M9.2 5.8 11.5 8 9.2 10.2" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/> </svg>',
        'check': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M3.2 8.3 6.6 11.5 13 4.8" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round"/> </svg>',
        'chevron-right': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M6 3.5 10 8 6 12.5" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"/> </svg>',
        'close': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M4.2 4.2 11.8 11.8M11.8 4.2 4.2 11.8" stroke="currentColor" stroke-width="1.6" stroke-linecap="round"/> </svg>',
        'comment': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M4 4.5h8c.8 0 1.5.7 1.5 1.5v4c0 .8-.7 1.5-1.5 1.5H8l-2.6 2v-2H4c-.8 0-1.5-.7-1.5-1.5V6c0-.8.7-1.5 1.5-1.5z" stroke="currentColor" stroke-width="1.5" stroke-linejoin="round"/> </svg>',
        'copy': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <rect x="5" y="5" width="8" height="8" rx="1.6" stroke="currentColor" stroke-width="1.5"/> <rect x="3" y="3" width="8" height="8" rx="1.6" stroke="currentColor" stroke-width="1.5" opacity="0.65"/> </svg>',
        'cut': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M6.2 6.2 13 13" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/> <path d="M13 3 8.6 7.4" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/> <circle cx="4" cy="5" r="1.6" stroke="currentColor" stroke-width="1.5"/> <circle cx="4" cy="11" r="1.6" stroke="currentColor" stroke-width="1.5"/> </svg>',
        'debug': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <circle cx="8" cy="8" r="3" stroke="currentColor" stroke-width="1.5"/> <path d="M8 2.5v2M8 11.5v2M2.5 8h2M11.5 8h2" stroke="currentColor" stroke-width="1.2" stroke-linecap="round"/> </svg>',
        'download': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M8 3.2v8.6M5.2 9 8 11.8 10.8 9" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/> </svg>',
        'error': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <circle cx="8" cy="8" r="5.2" stroke="currentColor" stroke-width="1.4" fill="none"/> <path d="M6 6L10 10M10 6L6 10" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/> </svg>',
        'exit': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M6 3.5h5.5c.6 0 1 .4 1 1v7c0 .6-.4 1-1 1H6" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/> <path d="M7.5 8H3.5M5 6.5 3.5 8 5 9.5" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round"/> </svg>',
        'folder-open': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M3 5h4l1 1h5c.6 0 1 .4 1 1v4.8c0 .7-.6 1.2-1.2 1.2H3.9c-.7 0-1.3-.6-1.3-1.3V6.2C2.6 5.5 3.2 5 4 5z" stroke="currentColor" stroke-width="1.4" stroke-linejoin="round"/> </svg>',
        'format': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M3 4h10" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/> <path d="M3 8h7" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/> <path d="M3 12h10" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/> </svg>',
        'git': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <circle cx="5" cy="5" r="1.6" fill="currentColor"/> <circle cx="11" cy="11" r="1.6" fill="currentColor"/> <path d="M6.4 6.4 9.6 9.6" stroke="currentColor" stroke-width="1.6" stroke-linecap="round"/> </svg>',
        'history': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <circle cx="8" cy="8" r="5" stroke="currentColor" stroke-width="1.4"/> <path d="M8 5.2v3.2l2.2 1.4" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round"/> </svg>',
        'info': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <circle cx="8" cy="8" r="5.2" stroke="currentColor" stroke-width="1.4" fill="none"/> <path d="M8 7.2V10.6" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/> <circle cx="8" cy="5.2" r="0.9" fill="currentColor"/> </svg>',
        'link': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M6.2 6.2a2.2 2.2 0 0 1 3.1 0l.5.5a2.2 2.2 0 0 1 0 3.1l-1.1 1.1a2.2 2.2 0 0 1-3.1 0" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round"/> <path d="M9.8 9.8a2.2 2.2 0 0 1-3.1 0l-.5-.5a2.2 2.2 0 0 1 0-3.1l1.1-1.1a2.2 2.2 0 0 1 3.1 0" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round"/> </svg>',
        'paste': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M6 4.5h4.2c.6 0 1 .4 1 1V13c0 .7-.6 1.3-1.3 1.3H6.8c-.8 0-1.3-.6-1.3-1.3V5.5c0-.6.4-1 1-1z" stroke="currentColor" stroke-width="1.5" stroke-linejoin="round"/> <rect x="6.2" y="2.8" width="3.6" height="2" rx="1" stroke="currentColor" stroke-width="1.5"/> </svg>',
        'radio': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <circle cx="8" cy="8" r="2.4" fill="currentColor"/> </svg>',
        'refresh': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M12.5 6.2A4.8 4.8 0 1 0 13 8" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/> <path d="M12.8 3.8v2.8H10" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round"/> </svg>',
        'run': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M5 3.5v9l8-4.5-8-4.5z" fill="currentColor"/> </svg>',
        'save': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M3.5 3.5h8.2l.8.8v8.2c0 .6-.5 1-1 1H4.5c-.6 0-1-.5-1-1V3.5z" stroke="currentColor" stroke-width="1.4" stroke-linejoin="round"/> <path d="M5 3.5v3h6v-3" stroke="currentColor" stroke-width="1.4" stroke-linejoin="round"/> </svg>',
        'search': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <circle cx="7" cy="7" r="3.6" stroke="currentColor" stroke-width="1.5"/> <path d="M10.2 10.2 13 13" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/> </svg>',
        'settings': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <circle cx="8" cy="8" r="2" stroke="currentColor" stroke-width="1.5"/> <path d="M8 2.8v1.2M8 12v1.2M2.8 8h1.2M12 8h1.2M3.7 3.7l.9.9M11.4 11.4l.9.9M12.3 3.7l-.9.9M4.6 11.4l-.9.9" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/> </svg>',
        'stop': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <rect x="4.2" y="4.2" width="7.6" height="7.6" rx="1.2" stroke="currentColor" stroke-width="1.6"/> </svg>',
        'terminal': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <rect x="2.8" y="3.5" width="10.4" height="9" rx="1.4" stroke="currentColor" stroke-width="1.4"/> <path d="M5 6.6l2 1.4-2 1.4" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round"/> <path d="M8.5 9.4h2" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/> </svg>',
        'upload': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M8 12.8V4.2M5.2 7 8 4.2 10.8 7" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/> </svg>',
        'warning': '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none"> <path d="M8 2.5L14 13.5H2L8 2.5Z" stroke="currentColor" stroke-width="1.4" stroke-linejoin="round"/> <path d="M8 6.2V9.4" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/> <circle cx="8" cy="11.6" r="0.8" fill="currentColor"/> </svg>'
    };

    const normalizeName = (name) => String(name || '').trim().toLowerCase();

    const parseSvg = (markup) => {
        const tpl = document.createElement('template');
        tpl.innerHTML = markup.trim();
        return tpl.content.firstElementChild;
    };

    const sizeToVar = (size) => {
        if (size === 12) return 'var(--ui-icon-12)';
        if (size === 16) return 'var(--ui-icon-16)';
        if (size === 20) return 'var(--ui-icon-20)';
        return `${size}px`;
    };

    const createSvgElement = (name, opts = {}) => {
        const iconName = normalizeName(name);
        const markup = ICONS[iconName];
        if (!markup) return document.createElement('span');

        const size = Number(opts.size || 16) || 16;
        const title = opts.title ? String(opts.title) : '';
        const cls = String(opts.className || '').trim();

        const svg = parseSvg(markup);
        if (!svg) return document.createElement('span');

        svg.setAttribute('width', String(size));
        svg.setAttribute('height', String(size));
        svg.style.width = sizeToVar(size);
        svg.style.height = sizeToVar(size);
        svg.setAttribute('aria-hidden', title ? 'false' : 'true');
        svg.setAttribute('focusable', 'false');
        svg.classList.add('ui-icon', `ui-icon--${size}`);
        if (cls) cls.split(/\s+/).forEach((c) => c && svg.classList.add(c));

        if (title) {
            const titleEl = document.createElementNS('http://www.w3.org/2000/svg', 'title');
            titleEl.textContent = title;
            svg.insertBefore(titleEl, svg.firstChild);
        }

        return svg;
    };

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.icons = window.AhmadIDEModules.ui.icons || {};
        window.AhmadIDEModules.ui.icons.map = ICONS;
        window.AhmadIDEModules.ui.icons.createSvgElement = createSvgElement;
    }
})();
