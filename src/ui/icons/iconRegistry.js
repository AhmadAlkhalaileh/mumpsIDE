(() => {
    const SVG_NS = 'http://www.w3.org/2000/svg';

    const make = (tag) => document.createElementNS(SVG_NS, tag);

    const iconDefs = {
        'chevron-right': (svg) => {
            const p = make('path');
            p.setAttribute('d', 'M6 3.5 L10 8 L6 12.5');
            p.setAttribute('fill', 'none');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.6');
            p.setAttribute('stroke-linecap', 'round');
            p.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(p);
        },
        'check': (svg) => {
            const p = make('path');
            p.setAttribute('d', 'M3.2 8.3 L6.6 11.5 L13 4.8');
            p.setAttribute('fill', 'none');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.8');
            p.setAttribute('stroke-linecap', 'round');
            p.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(p);
        },
        'radio': (svg) => {
            const c = make('circle');
            c.setAttribute('cx', '8');
            c.setAttribute('cy', '8');
            c.setAttribute('r', '2.4');
            c.setAttribute('fill', 'currentColor');
            svg.appendChild(c);
        },
        'cut': (svg) => {
            const p1 = make('path');
            p1.setAttribute('d', 'M6.2 6.2 L13 13');
            p1.setAttribute('fill', 'none');
            p1.setAttribute('stroke', 'currentColor');
            p1.setAttribute('stroke-width', '1.5');
            p1.setAttribute('stroke-linecap', 'round');
            svg.appendChild(p1);
            const p2 = make('path');
            p2.setAttribute('d', 'M13 3 L8.6 7.4');
            p2.setAttribute('fill', 'none');
            p2.setAttribute('stroke', 'currentColor');
            p2.setAttribute('stroke-width', '1.5');
            p2.setAttribute('stroke-linecap', 'round');
            svg.appendChild(p2);
            const c1 = make('circle');
            c1.setAttribute('cx', '4');
            c1.setAttribute('cy', '5');
            c1.setAttribute('r', '1.6');
            c1.setAttribute('fill', 'none');
            c1.setAttribute('stroke', 'currentColor');
            c1.setAttribute('stroke-width', '1.5');
            svg.appendChild(c1);
            const c2 = make('circle');
            c2.setAttribute('cx', '4');
            c2.setAttribute('cy', '11');
            c2.setAttribute('r', '1.6');
            c2.setAttribute('fill', 'none');
            c2.setAttribute('stroke', 'currentColor');
            c2.setAttribute('stroke-width', '1.5');
            svg.appendChild(c2);
        },
        'copy': (svg) => {
            const r1 = make('rect');
            r1.setAttribute('x', '5');
            r1.setAttribute('y', '5');
            r1.setAttribute('width', '8');
            r1.setAttribute('height', '8');
            r1.setAttribute('rx', '1.6');
            r1.setAttribute('fill', 'none');
            r1.setAttribute('stroke', 'currentColor');
            r1.setAttribute('stroke-width', '1.5');
            svg.appendChild(r1);
            const r2 = make('rect');
            r2.setAttribute('x', '3');
            r2.setAttribute('y', '3');
            r2.setAttribute('width', '8');
            r2.setAttribute('height', '8');
            r2.setAttribute('rx', '1.6');
            r2.setAttribute('fill', 'none');
            r2.setAttribute('stroke', 'currentColor');
            r2.setAttribute('stroke-width', '1.5');
            r2.setAttribute('opacity', '0.65');
            svg.appendChild(r2);
        },
        'paste': (svg) => {
            const p = make('path');
            p.setAttribute('d', 'M6 4.5h4.2c.6 0 1 .4 1 1V13c0 .7-.6 1.3-1.3 1.3H6.8C6 14.3 5.5 13.7 5.5 13V5.5c0-.6.4-1 1-1z');
            p.setAttribute('fill', 'none');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.5');
            p.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(p);
            const clip = make('rect');
            clip.setAttribute('x', '6.2');
            clip.setAttribute('y', '2.8');
            clip.setAttribute('width', '3.6');
            clip.setAttribute('height', '2');
            clip.setAttribute('rx', '1');
            clip.setAttribute('fill', 'none');
            clip.setAttribute('stroke', 'currentColor');
            clip.setAttribute('stroke-width', '1.5');
            svg.appendChild(clip);
        },
        'format': (svg) => {
            const p1 = make('path');
            p1.setAttribute('d', 'M3 4h10');
            p1.setAttribute('stroke', 'currentColor');
            p1.setAttribute('stroke-width', '1.5');
            p1.setAttribute('stroke-linecap', 'round');
            svg.appendChild(p1);
            const p2 = make('path');
            p2.setAttribute('d', 'M3 8h7');
            p2.setAttribute('stroke', 'currentColor');
            p2.setAttribute('stroke-width', '1.5');
            p2.setAttribute('stroke-linecap', 'round');
            svg.appendChild(p2);
            const p3 = make('path');
            p3.setAttribute('d', 'M3 12h10');
            p3.setAttribute('stroke', 'currentColor');
            p3.setAttribute('stroke-width', '1.5');
            p3.setAttribute('stroke-linecap', 'round');
            svg.appendChild(p3);
        },
        'comment': (svg) => {
            const p = make('path');
            p.setAttribute('d', 'M4 4.5h8c.8 0 1.5.7 1.5 1.5v4c0 .8-.7 1.5-1.5 1.5H8l-2.6 2v-2H4c-.8 0-1.5-.7-1.5-1.5V6c0-.8.7-1.5 1.5-1.5z');
            p.setAttribute('fill', 'none');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.5');
            p.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(p);
        },
        'settings': (svg) => {
            const c = make('circle');
            c.setAttribute('cx', '8');
            c.setAttribute('cy', '8');
            c.setAttribute('r', '2');
            c.setAttribute('fill', 'none');
            c.setAttribute('stroke', 'currentColor');
            c.setAttribute('stroke-width', '1.5');
            svg.appendChild(c);
            const p = make('path');
            p.setAttribute('d', 'M8 2.8v1.2M8 12v1.2M2.8 8h1.2M12 8h1.2M3.7 3.7l.9.9M11.4 11.4l.9.9M12.3 3.7l-.9.9M4.6 11.4l-.9.9');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.4');
            p.setAttribute('stroke-linecap', 'round');
            svg.appendChild(p);
        },
        'run': (svg) => {
            const p = make('path');
            p.setAttribute('d', 'M5 3.5v9l8-4.5-8-4.5z');
            p.setAttribute('fill', 'currentColor');
            svg.appendChild(p);
        },
        'debug': (svg) => {
            const c = make('circle');
            c.setAttribute('cx', '8');
            c.setAttribute('cy', '8');
            c.setAttribute('r', '3');
            c.setAttribute('fill', 'none');
            c.setAttribute('stroke', 'currentColor');
            c.setAttribute('stroke-width', '1.5');
            svg.appendChild(c);
            const p = make('path');
            p.setAttribute('d', 'M8 2.5v2M8 11.5v2M2.5 8h2M11.5 8h2');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.2');
            p.setAttribute('stroke-linecap', 'round');
            svg.appendChild(p);
        },
        'git': (svg) => {
            const c1 = make('circle');
            c1.setAttribute('cx', '5');
            c1.setAttribute('cy', '5');
            c1.setAttribute('r', '1.6');
            c1.setAttribute('fill', 'currentColor');
            svg.appendChild(c1);
            const c2 = make('circle');
            c2.setAttribute('cx', '11');
            c2.setAttribute('cy', '11');
            c2.setAttribute('r', '1.6');
            c2.setAttribute('fill', 'currentColor');
            svg.appendChild(c2);
            const p = make('path');
            p.setAttribute('d', 'M6.4 6.4 L9.6 9.6');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.6');
            p.setAttribute('stroke-linecap', 'round');
            svg.appendChild(p);
        },
        'history': (svg) => {
            const c = make('circle');
            c.setAttribute('cx', '8');
            c.setAttribute('cy', '8');
            c.setAttribute('r', '5');
            c.setAttribute('fill', 'none');
            c.setAttribute('stroke', 'currentColor');
            c.setAttribute('stroke-width', '1.4');
            svg.appendChild(c);
            const p = make('path');
            p.setAttribute('d', 'M8 5.2v3.2l2.2 1.4');
            p.setAttribute('fill', 'none');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.4');
            p.setAttribute('stroke-linecap', 'round');
            p.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(p);
        },
        'search': (svg) => {
            const c = make('circle');
            c.setAttribute('cx', '7');
            c.setAttribute('cy', '7');
            c.setAttribute('r', '3.6');
            c.setAttribute('fill', 'none');
            c.setAttribute('stroke', 'currentColor');
            c.setAttribute('stroke-width', '1.5');
            svg.appendChild(c);
            const p = make('path');
            p.setAttribute('d', 'M10.2 10.2 L13 13');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.5');
            p.setAttribute('stroke-linecap', 'round');
            svg.appendChild(p);
        },
        'refresh': (svg) => {
            const p = make('path');
            p.setAttribute('d', 'M12.5 6.2A4.8 4.8 0 1 0 13 8');
            p.setAttribute('fill', 'none');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.4');
            p.setAttribute('stroke-linecap', 'round');
            svg.appendChild(p);
            const a = make('path');
            a.setAttribute('d', 'M12.8 3.8v2.8H10');
            a.setAttribute('fill', 'none');
            a.setAttribute('stroke', 'currentColor');
            a.setAttribute('stroke-width', '1.4');
            a.setAttribute('stroke-linecap', 'round');
            a.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(a);
        },
        'save': (svg) => {
            const p = make('path');
            p.setAttribute('d', 'M3.5 3.5h8.2l.8.8v8.2c0 .6-.5 1-1 1H4.5c-.6 0-1-.5-1-1V3.5z');
            p.setAttribute('fill', 'none');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.4');
            p.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(p);
            const p2 = make('path');
            p2.setAttribute('d', 'M5 3.5v3h6v-3');
            p2.setAttribute('fill', 'none');
            p2.setAttribute('stroke', 'currentColor');
            p2.setAttribute('stroke-width', '1.4');
            p2.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(p2);
        },
        'add': (svg) => {
            const p = make('path');
            p.setAttribute('d', 'M8 3.2v9.6M3.2 8h9.6');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.6');
            p.setAttribute('stroke-linecap', 'round');
            svg.appendChild(p);
        },
        'folder-open': (svg) => {
            const p = make('path');
            p.setAttribute('d', 'M3 5h4l1 1h5c.6 0 1 .4 1 1v4.8c0 .7-.6 1.2-1.2 1.2H3.9c-.7 0-1.3-.6-1.3-1.3V6.2C2.6 5.5 3.2 5 4 5z');
            p.setAttribute('fill', 'none');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.4');
            p.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(p);
        },
        'close': (svg) => {
            const p = make('path');
            p.setAttribute('d', 'M4.2 4.2 L11.8 11.8 M11.8 4.2 L4.2 11.8');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.6');
            p.setAttribute('stroke-linecap', 'round');
            svg.appendChild(p);
        },
        'exit': (svg) => {
            const p = make('path');
            p.setAttribute('d', 'M6 3.5h5.5c.6 0 1 .4 1 1v7c0 .6-.4 1-1 1H6');
            p.setAttribute('fill', 'none');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.4');
            p.setAttribute('stroke-linecap', 'round');
            svg.appendChild(p);
            const a = make('path');
            a.setAttribute('d', 'M7.5 8H3.5M5 6.5L3.5 8 5 9.5');
            a.setAttribute('fill', 'none');
            a.setAttribute('stroke', 'currentColor');
            a.setAttribute('stroke-width', '1.4');
            a.setAttribute('stroke-linecap', 'round');
            a.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(a);
        },
        'terminal': (svg) => {
            const r = make('rect');
            r.setAttribute('x', '2.8');
            r.setAttribute('y', '3.5');
            r.setAttribute('width', '10.4');
            r.setAttribute('height', '9');
            r.setAttribute('rx', '1.4');
            r.setAttribute('fill', 'none');
            r.setAttribute('stroke', 'currentColor');
            r.setAttribute('stroke-width', '1.4');
            svg.appendChild(r);
            const p1 = make('path');
            p1.setAttribute('d', 'M5 6.6l2 1.4-2 1.4');
            p1.setAttribute('fill', 'none');
            p1.setAttribute('stroke', 'currentColor');
            p1.setAttribute('stroke-width', '1.4');
            p1.setAttribute('stroke-linecap', 'round');
            p1.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(p1);
            const p2 = make('path');
            p2.setAttribute('d', 'M8.5 9.4h2');
            p2.setAttribute('stroke', 'currentColor');
            p2.setAttribute('stroke-width', '1.4');
            p2.setAttribute('stroke-linecap', 'round');
            svg.appendChild(p2);
        },
        'upload': (svg) => {
            const p = make('path');
            p.setAttribute('d', 'M8 12.8V4.2M5.2 7 8 4.2 10.8 7');
            p.setAttribute('fill', 'none');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.5');
            p.setAttribute('stroke-linecap', 'round');
            p.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(p);
        },
        'download': (svg) => {
            const p = make('path');
            p.setAttribute('d', 'M8 3.2v8.6M5.2 9 8 11.8 10.8 9');
            p.setAttribute('fill', 'none');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.5');
            p.setAttribute('stroke-linecap', 'round');
            p.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(p);
        },
        'link': (svg) => {
            const p = make('path');
            p.setAttribute('d', 'M6.2 6.2a2.2 2.2 0 0 1 3.1 0l.5.5a2.2 2.2 0 0 1 0 3.1l-1.1 1.1a2.2 2.2 0 0 1-3.1 0');
            p.setAttribute('fill', 'none');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.4');
            p.setAttribute('stroke-linecap', 'round');
            p.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(p);
            const p2 = make('path');
            p2.setAttribute('d', 'M9.8 9.8a2.2 2.2 0 0 1-3.1 0l-.5-.5a2.2 2.2 0 0 1 0-3.1l1.1-1.1a2.2 2.2 0 0 1 3.1 0');
            p2.setAttribute('fill', 'none');
            p2.setAttribute('stroke', 'currentColor');
            p2.setAttribute('stroke-width', '1.4');
            p2.setAttribute('stroke-linecap', 'round');
            p2.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(p2);
        },
        'stop': (svg) => {
            const r = make('rect');
            r.setAttribute('x', '4.2');
            r.setAttribute('y', '4.2');
            r.setAttribute('width', '7.6');
            r.setAttribute('height', '7.6');
            r.setAttribute('rx', '1.2');
            r.setAttribute('fill', 'none');
            r.setAttribute('stroke', 'currentColor');
            r.setAttribute('stroke-width', '1.6');
            svg.appendChild(r);
        },
        'arrow-right': (svg) => {
            const p = make('path');
            p.setAttribute('d', 'M4 8h7.5M9.2 5.8 11.5 8 9.2 10.2');
            p.setAttribute('fill', 'none');
            p.setAttribute('stroke', 'currentColor');
            p.setAttribute('stroke-width', '1.5');
            p.setAttribute('stroke-linecap', 'round');
            p.setAttribute('stroke-linejoin', 'round');
            svg.appendChild(p);
        }
    };

    const normalizeName = (name) => String(name || '').trim().toLowerCase();

    const createIcon = (name, opts = {}) => {
        const size = Number(opts.size || 16) || 16;
        const title = opts.title ? String(opts.title) : '';
        const cls = String(opts.className || '');

        const iconName = normalizeName(name);
        const def = iconDefs[iconName];
        if (!def) return document.createElement('span');

        const svg = make('svg');
        svg.setAttribute('viewBox', '0 0 16 16');
        svg.setAttribute('width', String(size));
        svg.setAttribute('height', String(size));
        svg.setAttribute('aria-hidden', title ? 'false' : 'true');
        svg.className = `ui-icon ui-icon--${size}`.trim() + (cls ? ` ${cls}` : '');
        if (title) {
            const t = make('title');
            t.textContent = title;
            svg.appendChild(t);
        }
        def(svg);
        return svg;
    };

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.icons = window.AhmadIDEModules.ui.icons || {};
        window.AhmadIDEModules.ui.icons.createIcon = createIcon;
        window.AhmadIDEModules.ui.icons._defs = iconDefs;
    }
})();

