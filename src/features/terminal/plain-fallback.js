(() => {
    function createPlainTerminalTab({ id, name, container, state, deps } = {}) {
        const terminalConfig = deps?.terminalConfig || {};
        const getActiveEditor = deps?.getActiveEditor || (() => null);
        const sendCtrlC = deps?.sendCtrlC || (async () => { });

        if (!container) return null;

        container.classList.add('plain-terminal');
        const output = document.createElement('div');
        output.className = 'plain-terminal-output';
        output.textContent = '';
        const hiddenInput = document.createElement('textarea');
        hiddenInput.className = 'plain-terminal-input-hidden';
        hiddenInput.setAttribute('aria-label', 'Terminal input');
        container.appendChild(output);
        container.appendChild(hiddenInput);

        const maxLines = 2000;
        let carry = '';
        let pending = [];
        let flushHandle = null;

        const scheduleFlush = () => {
            if (flushHandle) return;
            const run = () => {
                flushHandle = null;
                if (!pending.length) return;
                const frag = document.createDocumentFragment();
                for (const line of pending) {
                    const div = document.createElement('div');
                    div.className = 'plain-terminal-line';
                    div.textContent = line;
                    frag.appendChild(div);
                }
                pending = [];
                output.appendChild(frag);

                const overflow = output.childNodes.length - maxLines;
                if (overflow > 0) {
                    for (let i = 0; i < overflow; i += 1) {
                        output.removeChild(output.firstChild);
                    }
                }
                output.scrollTop = output.scrollHeight;
            };
            if (typeof requestAnimationFrame === 'function') {
                flushHandle = requestAnimationFrame(run);
            } else {
                flushHandle = setTimeout(run, 16);
            }
        };

        const pushText = (data, { forceLine = false } = {}) => {
            const raw = data == null ? '' : String(data);
            const normalized = raw.replace(/\r\n/g, '\n').replace(/\r/g, '\n');
            const parts = normalized.split('\n');
            if (!parts.length) return;
            parts[0] = carry + parts[0];
            carry = parts.pop() ?? '';
            for (const line of parts) {
                pending.push(line);
            }
            if (forceLine && carry.length) {
                pending.push(carry);
                carry = '';
            }
            scheduleFlush();
        };

        const tab = {
            id,
            name,
            buffer: [],
            sessionId: null,
            term: null,
            container,
            lastSize: null,
            _plain: true
        };

        const send = async (data) => {
            if (tab.sessionId && window.ahmadIDE.terminalWrite) {
                await window.ahmadIDE.terminalWrite(tab.sessionId, data);
            }
        };

        tab.term = {
            write: (data) => {
                pushText(data);
            },
            writeln: (data) => {
                pushText(`${data ?? ''}\n`, { forceLine: true });
            },
            reset: () => {
                carry = '';
                pending = [];
                output.textContent = '';
            },
            resize: () => { },
            dispose: () => { },
            focus: () => hiddenInput.focus()
        };

        const handleKey = async (e) => {
            if (e.key === 'Escape' && terminalConfig.escapeToEditor && !terminalConfig.overrideIdeShortcuts) {
                e.preventDefault();
                getActiveEditor()?.focus();
                return;
            }
            if (e.ctrlKey && e.key.toLowerCase() === 'c') {
                e.preventDefault();
                await sendCtrlC(state);
                return;
            }
            if (!tab.sessionId) return;
            if (e.key === 'Enter') {
                e.preventDefault();
                await send('\r');
                return;
            }
            if (e.key === 'Backspace') {
                e.preventDefault();
                await send('\x7f');
                return;
            }
            if (e.key === 'Tab') {
                e.preventDefault();
                await send('\t');
                return;
            }
            if (e.key.length === 1 && !e.metaKey) {
                e.preventDefault();
                await send(e.key);
            }
        };

        hiddenInput.addEventListener('keydown', handleKey);
        container.addEventListener('mousedown', () => hiddenInput.focus());
        hiddenInput.tabIndex = 0;

        pushText('Plain terminal ready. (xterm not available)\n', { forceLine: true });
        return tab;
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.terminal = window.AhmadIDEModules.features.terminal || {};
        window.AhmadIDEModules.features.terminal.createPlainTerminalTab = createPlainTerminalTab;
    }
})();

