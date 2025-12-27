(() => {
    let appendOutput = null;
    let loadRoutineList = null;

    function createDockerUi({ deps } = {}) {
        appendOutput = deps?.appendOutput;
        loadRoutineList = deps?.loadRoutineList;
        return { renderDocker, setConnStatus };
    }

    function renderDocker(containers, routineState, editor, opts) {
        const host = document.getElementById('dockerList');
        if (!host) return;
        const collapseEl = opts instanceof Element
            ? opts
            : (opts && opts.collapseEl instanceof Element ? opts.collapseEl : null);
        const onSelect = opts && typeof opts.onSelect === 'function' ? opts.onSelect : null;
        host.innerHTML = '';
        if (!containers || !containers.length) {
            host.textContent = 'No running containers.';
            return;
        }
        // Load saved Docker config once
        let dockerConfig = {};
        try {
            dockerConfig = JSON.parse(localStorage.getItem('ahmadIDE:dockerConfig') || '{}');
        } catch (e) {
            dockerConfig = {};
        }

        containers.forEach(c => {
            const div = document.createElement('div');
            div.className = 'docker-item';
            div.style.display = 'flex';
            div.style.justifyContent = 'space-between';
            div.style.alignItems = 'center';

            const infoSpan = document.createElement('span');
            infoSpan.textContent = `${c.name} (${c.id}) :: ${c.status}`;
            div.appendChild(infoSpan);

            // Default Toggle (Star)
            const isDefault = dockerConfig.defaultContainerId === c.id;
            const star = document.createElement('span');
            star.textContent = isDefault ? '★' : '☆';
            star.className = 'docker-default-star'; // Add class for potential styling
            star.style.marginLeft = '8px';
            star.style.cursor = 'pointer';
            star.style.fontSize = '1.2em';
            star.style.color = isDefault ? '#f0a35c' : '#6a7280'; // accent-orange vs muted
            star.title = isDefault ? 'Unset Default' : 'Set as Default (Auto-Connect)';

            star.onclick = (e) => {
                e.stopPropagation();
                // Toggle default
                if (isDefault) {
                    delete dockerConfig.defaultContainerId;
                } else {
                    dockerConfig.defaultContainerId = c.id;
                    dockerConfig.defaultContainerName = c.name; // Store name for UI status
                }
                localStorage.setItem('ahmadIDE:dockerConfig', JSON.stringify(dockerConfig));
                // Re-render to update stars
                renderDocker(containers, routineState, editor, opts);
            };

            div.appendChild(star);

            div.onclick = async () => {
                appendOutput(`🐳 Using container ${c.name} (${c.id})`);
                // Save this container ID for later use
                try {
                    localStorage.setItem('ahmadIDE:lastContainerId', c.id);
                } catch (e) {
                    // ignore
                }

                // Use latest config (reload to be safe, or use modified object)
                let currentConfig = {};
                try {
                    currentConfig = JSON.parse(localStorage.getItem('ahmadIDE:dockerConfig') || '{}');
                } catch (e) { }

                await window.ahmadIDE.setConnection('docker', { docker: { containerId: c.id, ...currentConfig } });
                const modeLabel = currentConfig.ydbPath ? 'Docker (configured)' : 'Docker (universal)';
                setConnStatus(`Docker: ${c.name} (${modeLabel})`, 'success');
                await loadRoutineList(
                    routineState,
                    editor,
                    document.getElementById('routineSearch')?.value || '',
                    null
                );
                if (onSelect) onSelect();
                if (collapseEl) collapseEl.classList.add('collapsed');
                if (window.MIDE?.scheduleEditorLayout) window.MIDE.scheduleEditorLayout('docker-select');
            };
            host.appendChild(div);
        });
        // Listing containers does not mean we are connected to one.
    }

    function setConnStatus(text, severity) {
        const pill = document.getElementById('connStatus');
        if (!pill) return;
        pill.textContent = text;
        if (severity === 'subtle') {
            pill.style.background = '';
            pill.style.color = '';
            return;
        }
        pill.style.background = severity === 'error'
            ? 'var(--accent-orange)'
            : 'var(--accent-soft)';
        pill.style.color = severity === 'error'
            ? 'var(--text-bright)'
            : 'var(--accent-blue)';
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.docker = window.AhmadIDEModules.renderer.docker || {};
        window.AhmadIDEModules.renderer.docker.createDockerUi = createDockerUi;
    }
})();
