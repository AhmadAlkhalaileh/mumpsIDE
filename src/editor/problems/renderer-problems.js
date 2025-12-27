(() => {
    function createProblemsManager({ state, deps } = {}) {
        if (!state || typeof state.maxProblemItems !== 'number') {
            throw new Error('createProblemsManager requires { state.maxProblemItems }');
        }

        const maxProblemItems = state.maxProblemItems;
        const revealLine = deps?.revealLine || (() => { });
        const normalizeSeverity = deps?.normalizeSeverity || ((sev) => {
            if (!sev) return 'info';
            const lower = (sev + '').toLowerCase();
            if (lower.startsWith('err')) return 'error';
            if (lower.startsWith('warn')) return 'warning';
            return 'info';
        });
        const createIcon = deps?.createIcon
            || window.AhmadIDEModules?.ui?.icons?.createIcon
            || window.AhmadIDEModules?.ui?.components?.Icon?.createIcon
            || null;
        const setActiveDebugTab = deps?.setActiveDebugTab || (() => { });
        const getActiveDebugTab = deps?.getActiveDebugTab || (() => null);
        let lastProblems = [];

        const ensureProblemListDelegation = (listEl) => {
            if (!listEl) return;
            if (listEl.dataset.problemsDelegated === '1') return;
            listEl.dataset.problemsDelegated = '1';
            listEl.addEventListener('click', (e) => {
                const li = e.target?.closest ? e.target.closest('li.problem-item') : null;
                if (!li || !listEl.contains(li)) return;
                const ln = parseInt(li.dataset.line || '0', 10);
                if (ln) revealLine(ln);
            });
        };

        function updateProblemSummary(items) {
            const pill = document.getElementById('problemsSummary');
            if (!pill) return;
            const problems = Array.isArray(items) ? items : [];
            const total = problems.length;
            const highest = problems.reduce((acc, cur) => {
                const sev = normalizeSeverity(cur.severity);
                if (sev === 'error') return 'error';
                if (sev === 'warning' && acc !== 'error') return 'warning';
                return acc;
            }, 'info');

            pill.innerHTML = '';
            const iconName = highest === 'error' ? 'error' : (highest === 'warning' ? 'warning' : 'info');
            const iconWrap = document.createElement('span');
            iconWrap.className = 'icon';
            if (createIcon) {
                iconWrap.appendChild(createIcon(iconName, { size: 12 }));
            } else {
                iconWrap.textContent = highest === 'error' ? '⛔' : (highest === 'warning' ? '⚠' : 'ℹ');
            }
            pill.appendChild(iconWrap);
            pill.appendChild(document.createTextNode(` Problems: ${total}`));
            if (highest === 'error') {
                pill.style.background = 'rgba(248,113,113,0.18)';
                pill.style.color = '#fecdd3';
                pill.style.borderColor = 'rgba(248,113,113,0.35)';
            } else if (highest === 'warning') {
                pill.style.background = 'rgba(252,211,77,0.18)';
                pill.style.color = '#fcd34d';
                pill.style.borderColor = 'rgba(252,211,77,0.35)';
            } else {
                pill.style.background = '';
                pill.style.color = '';
                pill.style.borderColor = '';
            }
        }

        const buildFragment = ({ items, trimmed } = {}) => {
            const problems = Array.isArray(items) ? items : [];
            const fragment = document.createDocumentFragment();

            if (!problems.length) {
                const li = document.createElement('li');
                li.textContent = 'No problems.';
                fragment.appendChild(li);
                return fragment;
            }

            const iconFor = (sev) => {
                const s = (sev || 'info').toLowerCase();
                if (s.startsWith('err')) return { name: 'error', fallback: '⛔' };
                if (s.startsWith('warn')) return { name: 'warning', fallback: '⚠' };
                return { name: 'info', fallback: 'ℹ' };
            };

            problems.forEach(item => {
                const li = document.createElement('li');
                const sev = normalizeSeverity(item?.severity || 'info');
                li.className = `problem-item ${sev}`;
                li.dataset.line = item?.line || '';

                const icon = document.createElement('span');
                icon.className = 'problem-icon';
                const iconMeta = iconFor(sev);
                if (createIcon) {
                    icon.appendChild(createIcon(iconMeta.name, { size: 16 }));
                } else {
                    icon.textContent = iconMeta.fallback;
                }

                const text = document.createElement('span');
                text.className = 'problem-text';
                const lineInfo = item?.line ? ` (line ${item.line})` : '';
                const codeInfo = item?.code ? ` [${item.code}]` : '';
                const msg = item?.message || '';
                text.textContent = `${sev}${codeInfo}: ${msg}${lineInfo}`;
                li.title = `${sev.toUpperCase()}${codeInfo} ${msg}${lineInfo}`;

                li.appendChild(icon);
                li.appendChild(text);
                fragment.appendChild(li);
            });

            if (trimmed) {
                const li = document.createElement('li');
                li.className = 'problem-item info';
                li.textContent = `Showing first ${maxProblemItems} issues...`;
                fragment.appendChild(li);
            }

            return fragment;
        };

        const renderLastToDom = () => {
            const primaryList = document.getElementById('problemsList') || document.getElementById('problemsListStandalone');
            if (!primaryList) return;

            const secondaryList = primaryList.id === 'problemsList'
                ? document.getElementById('problemsListStandalone')
                : document.getElementById('problemsList');

            ensureProblemListDelegation(primaryList);
            ensureProblemListDelegation(secondaryList);

            const limited = lastProblems.slice(0, maxProblemItems);
            const trimmed = lastProblems.length > limited.length;

            primaryList.innerHTML = '';
            primaryList.appendChild(buildFragment({ items: limited, trimmed }));
            if (secondaryList) secondaryList.innerHTML = primaryList.innerHTML;

            setActiveDebugTab(getActiveDebugTab());
        };

        function renderProblems(items) {
            lastProblems = Array.isArray(items) ? items : [];
            updateProblemSummary(lastProblems);
            renderLastToDom();
        }

        // Ensure Problems UI renders even when panels are lazy-mounted.
        try {
            const fr = window.AhmadIDEModules?.app?.featureRegistry;
            fr?.onMounted?.('debugPanel', () => renderLastToDom());
            fr?.onMounted?.('problemsPanel', () => renderLastToDom());
        } catch (_) { }

        return {
            renderProblems,
            updateProblemSummary
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.problems = window.AhmadIDEModules.problems || {};
        window.AhmadIDEModules.problems.createProblemsManager = createProblemsManager;
    }
})();
