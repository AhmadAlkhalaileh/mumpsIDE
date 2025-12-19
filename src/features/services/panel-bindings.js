(() => {
    function wireServicesPanel() {
        const dockerListBtn = document.getElementById('svcDockerListBtn');
        const dockerRefreshBtn = document.getElementById('svcDockerRefreshBtn');
        const sshRunBtn = document.getElementById('svcSshRunBtn');

        if (!dockerListBtn || !dockerRefreshBtn || !sshRunBtn) return;

        dockerListBtn.addEventListener('click', async () => {
            const out = document.getElementById('svcDockerOutput');
            if (out) out.textContent = 'Loading...';
            const res = await window.ahmadIDE.listDocker?.();
            if (!out) return;
            if (res?.ok) {
                out.textContent =
                    res.containers?.map(c => `${c.name} (${c.id}) :: ${c.status}`).join('\n') ||
                    'No running containers.';
            } else {
                out.textContent = res?.error || res?.stderr || 'Docker query failed';
            }
        });

        dockerRefreshBtn.addEventListener('click', () => {
            dockerListBtn.click();
        });

        sshRunBtn.addEventListener('click', async () => {
            const cmd = (document.getElementById('svcSshCmd')?.value || '').trim();
            const out = document.getElementById('svcSshOutput');
            if (!cmd) {
                if (out) out.textContent = 'Enter a command.';
                return;
            }
            if (out) out.textContent = `Running: ${cmd}`;
            const res = await window.ahmadIDE.hostExec(cmd);
            if (!out) return;
            if (res.ok) out.textContent = res.stdout || '(no output)';
            else out.textContent = res.error || res.stderr || 'SSH command failed';
        });
    }

    function bootstrap() {
        const featureRegistry = window.AhmadIDEModules?.app?.featureRegistry;
        if (!featureRegistry || typeof featureRegistry.onMounted !== 'function') return;
        featureRegistry.onMounted('servicesPanel', () => {
            wireServicesPanel();
        });
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', bootstrap, { once: true });
    } else {
        bootstrap();
    }
})();

