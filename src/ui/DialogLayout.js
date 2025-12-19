(() => {
    function createDialogLayout(opts = {}) {
        const {
            title = '',
            searchPlaceholder = 'Search',
            showCloseX = true
        } = opts;

        const root = document.createElement('div');
        root.className = 'ui-dialog-layout';

        const header = document.createElement('div');
        header.className = 'ui-dialog-header';

        const headerLeft = document.createElement('div');
        headerLeft.className = 'ui-dialog-header__left';
        const h1 = document.createElement('div');
        h1.className = 'ui-dialog-title';
        h1.textContent = title;
        headerLeft.appendChild(h1);

        const headerRight = document.createElement('div');
        headerRight.className = 'ui-dialog-header__right';
        const closeBtn = document.createElement('button');
        closeBtn.className = 'ui-dialog-close';
        closeBtn.type = 'button';
        closeBtn.title = 'Close';
        closeBtn.innerHTML = '✕';
        if (!showCloseX) closeBtn.classList.add('hidden');
        headerRight.appendChild(closeBtn);

        header.appendChild(headerLeft);
        header.appendChild(headerRight);

        const body = document.createElement('div');
        body.className = 'ui-dialog-body';

        const sidebar = document.createElement('div');
        sidebar.className = 'ui-dialog-sidebar';

        const searchRow = document.createElement('div');
        searchRow.className = 'ui-dialog-search';
        const searchInput = document.createElement('input');
        searchInput.className = 'ui-input ui-dialog-search__input';
        searchInput.type = 'text';
        searchInput.placeholder = searchPlaceholder;
        const clearBtn = document.createElement('button');
        clearBtn.type = 'button';
        clearBtn.className = 'ui-btn ui-btn--ghost ui-btn--sm ui-dialog-search__clear';
        clearBtn.textContent = 'Clear';
        searchRow.appendChild(searchInput);
        searchRow.appendChild(clearBtn);

        const nav = document.createElement('div');
        nav.className = 'ui-dialog-nav';
        sidebar.appendChild(searchRow);
        sidebar.appendChild(nav);

        const contentWrap = document.createElement('div');
        contentWrap.className = 'ui-dialog-content';
        const content = document.createElement('div');
        content.className = 'ui-dialog-content__inner';
        contentWrap.appendChild(content);

        body.appendChild(sidebar);
        body.appendChild(contentWrap);

        const footer = document.createElement('div');
        footer.className = 'ui-dialog-footer';

        root.appendChild(header);
        root.appendChild(body);
        root.appendChild(footer);

        const state = {
            items: [],
            activeId: null,
            query: ''
        };

        const renderNav = () => {
            nav.innerHTML = '';
            state.items.forEach((it) => {
                const btn = document.createElement('button');
                btn.type = 'button';
                btn.className = 'ui-dialog-nav__item' + (it.id === state.activeId ? ' active' : '');
                btn.textContent = it.label;
                btn.addEventListener('click', () => api.setActive(it.id));
                nav.appendChild(btn);
            });
        };

        const api = {
            root,
            elements: { closeBtn, searchInput, content, footer, titleEl: h1 },
            setTitle: (t) => { h1.textContent = t || ''; },
            setItems: (items) => {
                state.items = Array.isArray(items) ? items : [];
                if (!state.activeId && state.items.length) state.activeId = state.items[0].id;
                renderNav();
            },
            setActive: (id) => {
                if (!id || id === state.activeId) return;
                state.activeId = id;
                renderNav();
                root.dispatchEvent(new CustomEvent('ui:dialog-nav-change', { detail: { id } }));
            },
            getActive: () => state.activeId,
            setSearch: (q) => {
                state.query = String(q || '');
                root.dispatchEvent(new CustomEvent('ui:dialog-search', { detail: { query: state.query } }));
            },
            setFooter: (nodes = []) => {
                footer.innerHTML = '';
                (Array.isArray(nodes) ? nodes : [nodes]).filter(Boolean).forEach((n) => footer.appendChild(n));
            }
        };

        searchInput.addEventListener('input', () => api.setSearch(searchInput.value));
        clearBtn.addEventListener('click', () => {
            searchInput.value = '';
            api.setSearch('');
            searchInput.focus();
        });

        return api;
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.createDialogLayout = createDialogLayout;
    }
})();
