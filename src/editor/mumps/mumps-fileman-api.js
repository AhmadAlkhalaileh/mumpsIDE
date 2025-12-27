(() => {
    'use strict';

    const normalizeTriggerText = (s) => String(s || '').toLowerCase().replace(/\s+/g, ' ').trimEnd();

    const APIS = Object.freeze({
        'FIND1^DIC': Object.freeze({
            signature: '$$FIND1^DIC(FILE,IENS,FLAGS,[.]VALUE,[.]INDEXES,[.]SCREEN,MSG_ROOT)',
            description: 'Finds a single matching record; if more than one match is found, FileMan returns an error.',
            params: [
                ['FILE', 'File number (e.g., 2 for PATIENT)'],
                ['IENS', 'IENS for subfiles ("" for top-level)'],
                ['VALUE', 'Lookup value or array of values (by reference)'],
                ['MSG_ROOT', 'Closed root name for errors (e.g., "MSG")']
            ],
            gotcha: 'Errors if more than one match is found.',
            url: 'https://www.hardhats.org/fileman/pm/db_dicf1.htm'
        }),
        'FIND^DIC': Object.freeze({
            signature: 'FIND^DIC(FILE,IENS,FIELDS,FLAGS,[.]VALUE,NUMBER,[.]INDEXES,[.]SCREEN,IDENTIFIER,TARGET_ROOT,MSG_ROOT)',
            description: 'Finds entries matching input value(s); controls returned fields, hit count, indexes, identifiers, and screening.',
            params: [
                ['FILE', 'File number'],
                ['FIELDS', 'Field list to return (e.g., "@;.01;2")'],
                ['VALUE', 'Lookup value(s) (by reference)'],
                ['TARGET_ROOT', 'Closed root name for results (e.g., "DILIST")']
            ],
            url: 'https://www.hardhats.org/fileman/pm/db_dicf.htm'
        }),
        'LIST^DIC': Object.freeze({
            signature: 'LIST^DIC(FILE,IENS,FIELDS,FLAGS,NUMBER,[.]FROM,[.]PART,INDEX,[.]SCREEN,IDENTIFIER,TARGET_ROOT,MSG_ROOT)',
            description: 'Returns a sorted list of entries; supports index sorting, start point, partial match, screening, and identifiers.',
            params: [
                ['FILE', 'File number'],
                ['NUMBER', 'Max entries to return (e.g., 50)'],
                ['INDEX', 'Index to sort by (e.g., "B")'],
                ['TARGET_ROOT', 'Closed root name for results (e.g., "DILIST")']
            ],
            url: 'https://www.hardhats.org/fileman/pm/db_dic_l.htm'
        }),
        'GETS^DIQ': Object.freeze({
            signature: 'GETS^DIQ(FILE,IENS,FIELD,FLAGS,TARGET_ROOT,MSG_ROOT)',
            description: 'Retrieves one or more fields from a record/subrecord(s) into a target array.',
            params: [
                ['FILE', 'File number'],
                ['IENS', 'IENS for the record/subrecord'],
                ['FIELD', 'Field number(s) or field list'],
                ['TARGET_ROOT', 'Closed root name for output (e.g., "OUT")']
            ],
            url: 'https://www.hardhats.org/fileman/pm/db_diqgs.htm'
        }),
        'GET1^DIQ': Object.freeze({
            signature: '$$GET1^DIQ(FILE,IENS,FIELD,FLAGS,TARGET_ROOT,MSG_ROOT)',
            description: 'Retrieves a single field value (extrinsic).',
            params: [
                ['FILE', 'File number'],
                ['IENS', 'IENS for the record/subrecord'],
                ['FIELD', 'Field number'],
                ['FLAGS', 'Include "I" for internal or "E" for external']
            ],
            gotcha: 'Pass "I" to force internal values when needed.',
            url: 'https://www.hardhats.org/fileman/pm/db_diqg1.htm'
        }),
        'UPDATE^DIE': Object.freeze({
            signature: 'UPDATE^DIE(FLAGS,FDA_ROOT,IEN_ROOT,MSG_ROOT)',
            description: 'Adds new entries using an FDA; returns new IENs; indexing/auditing occurs.',
            params: [
                ['FLAGS', 'FileMan flags (often "")'],
                ['FDA_ROOT', 'Closed root name containing FDA data (e.g., "FDA")'],
                ['IEN_ROOT', 'Closed root name to receive new IENs (e.g., "IEN")'],
                ['MSG_ROOT', 'Closed root name for errors (e.g., "MSG")']
            ],
            url: 'https://www.hardhats.org/fileman/pm/db_die_u.htm'
        }),
        'FILE^DIE': Object.freeze({
            signature: 'FILE^DIE(FLAGS,FDA_ROOT,MSG_ROOT)',
            description: 'Files data into existing entries; enforces key checks (unless U flag); fires xrefs and audits.',
            params: [
                ['FLAGS', 'FileMan flags (e.g., "E" or "U")'],
                ['FDA_ROOT', 'Closed root name containing FDA data (e.g., "FDA")'],
                ['MSG_ROOT', 'Closed root name for errors (e.g., "MSG")'],
                ['FDA', 'FDA should target existing IENS values']
            ],
            url: 'https://www.hardhats.org/fileman/pm/db_die_f.htm'
        }),
        'HELP^DIE': Object.freeze({
            signature: 'HELP^DIE(FILE,IENS,FIELD,FLAGS,MSG_ROOT)',
            description: 'Returns FileMan help text for a field into a target array.',
            params: [
                ['FILE', 'File number'],
                ['IENS', 'IENS ("" for top-level)'],
                ['FIELD', 'Field number'],
                ['MSG_ROOT', 'Closed root name for output (e.g., "MSG")']
            ],
            url: 'https://www.hardhats.org/fileman/pm/db_die_h.htm'
        }),
        '^DIC': Object.freeze({
            signature: '^DIC (classic lookup/add)',
            description: 'Classic FileMan lookup/add call using the DIC/X/Y family of variables.',
            params: [
                ['DIC', 'Global root/file reference'],
                ['DIC(0)', 'Lookup flags (e.g., "AEMQ")'],
                ['X', 'Lookup value'],
                ['Y', 'Result (IEN^name or -1)']
            ],
            url: 'https://www.hardhats.org/fileman/pm/cl_dic.htm'
        }),
        '^DIE': Object.freeze({
            signature: '^DIE (classic edit)',
            description: 'Classic FileMan edit call using DIE/DA/DR variables (edits an existing entry).',
            params: [
                ['DIE', 'Global root/file reference'],
                ['DA', 'Entry IEN'],
                ['DR', 'Edit string (field list and prompts)'],
                ['(DA)', 'DA array for subfiles']
            ],
            url: 'https://www.hardhats.org/fileman/pm/cl_die.htm'
        })
    });

    const buildApiMarkdown = (api) => {
        if (!api) return '';
        const out = [];
        out.push(`**${api.signature}**`);
        out.push('');
        out.push(String(api.description || '').trim());
        const params = Array.isArray(api.params) ? api.params : [];
        if (params.length) {
            out.push('');
            out.push('**Key Params**');
            params.slice(0, 4).forEach(([name, desc]) => {
                out.push(`- \`${name}\`: ${desc}`);
            });
        }
        if (api.gotcha) {
            out.push('');
            out.push(`_Gotcha_: ${String(api.gotcha).trim()}`);
        }
        if (api.url) {
            out.push('');
            out.push(`[More…](${api.url})`);
        }
        return out.join('\n');
    };

    const buildDefaultSnippets = () => {
        const mk = (key, { name, prefixes, body }) => {
            const api = APIS[key];
            return {
                name,
                prefix: prefixes,
                description: api?.description || name,
                documentation: api ? { value: buildApiMarkdown(api) } : undefined,
                body
            };
        };

        return {
            'fileman-find1-dic-call': mk('FIND1^DIC', {
                name: 'FileMan: $$FIND1^DIC(...)',
                prefixes: ['FIND1^DIC', '$$FIND1^DIC'],
                body: [
                    '$$FIND1^DIC(${1:FILE},${2:\"\"},${3:\"QX\"},${4:.VALUE},${5:.INDEXES},${6:.SCREEN},${7:\"MSG\"})$0'
                ]
            }),
            'fileman-find1-dic-set': mk('FIND1^DIC', {
                name: 'FileMan: SET IEN=$$FIND1^DIC(...)',
                prefixes: ['fileman find1', 'fileman find1 dic'],
                body: [
                    'SET ${1:IEN}=$$FIND1^DIC(${2:FILE},${3:\"\"},${4:\"QX\"},${5:.VALUE},${6:.INDEXES},${7:.SCREEN},${8:\"MSG\"})$0'
                ]
            }),
            'fileman-find-dic-call': mk('FIND^DIC', {
                name: 'FileMan: FIND^DIC(...)',
                prefixes: ['FIND^DIC'],
                body: [
                    'FIND^DIC(${1:FILE},${2:\"\"},${3:\"@;.01\"},${4:\"QX\"},${5:.VALUE},${6:50},${7:.INDEXES},${8:.SCREEN},${9:\"\"},${10:\"DILIST\"},${11:\"MSG\"})$0'
                ]
            }),
            'fileman-find-dic-do': mk('FIND^DIC', {
                name: 'FileMan: DO FIND^DIC(...)',
                prefixes: ['fileman find', 'fileman find dic'],
                body: [
                    'DO FIND^DIC(${1:FILE},${2:\"\"},${3:\"@;.01\"},${4:\"QX\"},${5:.VALUE},${6:50},${7:.INDEXES},${8:.SCREEN},${9:\"\"},${10:\"DILIST\"},${11:\"MSG\"})$0'
                ]
            }),
            'fileman-find-dic-setup': mk('FIND^DIC', {
                name: 'FileMan: FIND^DIC (with TARGET_ROOT + screening)',
                prefixes: ['fileman find setup', 'fileman find screen', 'fileman find indexes'],
                body: [
                    'NEW VALUE,INDEXES,SCREEN,DILIST,MSG',
                    'KILL VALUE,INDEXES,SCREEN,DILIST,MSG',
                    'SET VALUE=${1:\"\"}',
                    'DO FIND^DIC(${2:FILE},${3:\"\"},${4:\"@;.01\"},${5:\"QX\"},.VALUE,${6:50},.INDEXES,.SCREEN,${7:\"\"},\"DILIST\",\"MSG\")',
                    '$0'
                ]
            }),
            'fileman-list-dic-call': mk('LIST^DIC', {
                name: 'FileMan: LIST^DIC(...)',
                prefixes: ['LIST^DIC'],
                body: [
                    'LIST^DIC(${1:FILE},${2:\"\"},${3:\"@;.01\"},${4:\"Q\"},${5:50},${6:.FROM},${7:.PART},${8:\"B\"},${9:.SCREEN},${10:\"\"},${11:\"DILIST\"},${12:\"MSG\"})$0'
                ]
            }),
            'fileman-list-dic-do': mk('LIST^DIC', {
                name: 'FileMan: DO LIST^DIC(...)',
                prefixes: ['fileman list', 'fileman list dic'],
                body: [
                    'DO LIST^DIC(${1:FILE},${2:\"\"},${3:\"@;.01\"},${4:\"Q\"},${5:50},${6:.FROM},${7:.PART},${8:\"B\"},${9:.SCREEN},${10:\"\"},${11:\"DILIST\"},${12:\"MSG\"})$0'
                ]
            }),
            'fileman-list-dic-from': mk('LIST^DIC', {
                name: 'FileMan: LIST^DIC (with FROM/PART)',
                prefixes: ['fileman list from', 'fileman list part'],
                body: [
                    'NEW FROM,PART,DILIST,MSG',
                    'KILL FROM,PART,DILIST,MSG',
                    'SET FROM=${1:\"\"}',
                    'SET PART=${2:\"\"}',
                    'DO LIST^DIC(${3:FILE},${4:\"\"},${5:\"@;.01\"},${6:\"Q\"},${7:50},.FROM,.PART,${8:\"B\"},${9:.SCREEN},${10:\"\"},\"DILIST\",\"MSG\")',
                    '$0'
                ]
            }),
            'fileman-gets-diq-call': mk('GETS^DIQ', {
                name: 'FileMan: GETS^DIQ(...)',
                prefixes: ['GETS^DIQ'],
                body: [
                    'GETS^DIQ(${1:FILE},${2:IENS},${3:FIELD},${4:\"IE\"},${5:\"OUT\"},${6:\"MSG\"})$0'
                ]
            }),
            'fileman-gets-diq-do': mk('GETS^DIQ', {
                name: 'FileMan: DO GETS^DIQ(...)',
                prefixes: ['fileman gets', 'fileman gets diq'],
                body: [
                    'DO GETS^DIQ(${1:FILE},${2:IENS},${3:FIELD},${4:\"IE\"},${5:\"OUT\"},${6:\"MSG\"})$0'
                ]
            }),
            'fileman-get1-diq-call': mk('GET1^DIQ', {
                name: 'FileMan: $$GET1^DIQ(...)',
                prefixes: ['GET1^DIQ', '$$GET1^DIQ'],
                body: [
                    '$$GET1^DIQ(${1:FILE},${2:IENS},${3:FIELD},${4:\"I\"},${5:\"OUT\"},${6:\"MSG\"})$0'
                ]
            }),
            'fileman-get1-diq-set': mk('GET1^DIQ', {
                name: 'FileMan: SET VALUE=$$GET1^DIQ(...)',
                prefixes: ['fileman get1', 'fileman get1 diq'],
                body: [
                    'SET ${1:VALUE}=$$GET1^DIQ(${2:FILE},${3:IENS},${4:FIELD},${5:\"I\"},${6:\"OUT\"},${7:\"MSG\"})$0'
                ]
            }),
            'fileman-update-die-call': mk('UPDATE^DIE', {
                name: 'FileMan: UPDATE^DIE(...)',
                prefixes: ['UPDATE^DIE'],
                body: [
                    'UPDATE^DIE(${1:\"\"},${2:\"FDA\"},${3:\"IEN\"},${4:\"MSG\"})$0'
                ]
            }),
            'fileman-update-die-do': mk('UPDATE^DIE', {
                name: 'FileMan: DO UPDATE^DIE(...)',
                prefixes: ['fileman update', 'fileman update die'],
                body: [
                    'NEW FDA,IEN,MSG',
                    'KILL FDA,IEN,MSG',
                    '; SET FDA(file,\"+1,\",field)=value',
                    'DO UPDATE^DIE(${1:\"\"},\"FDA\",\"IEN\",\"MSG\")',
                    '$0'
                ]
            }),
            'fileman-file-die-call': mk('FILE^DIE', {
                name: 'FileMan: FILE^DIE(...)',
                prefixes: ['FILE^DIE'],
                body: [
                    'FILE^DIE(${1:\"\"},${2:\"FDA\"},${3:\"MSG\"})$0'
                ]
            }),
            'fileman-file-die-do': mk('FILE^DIE', {
                name: 'FileMan: DO FILE^DIE(...)',
                prefixes: ['fileman file', 'fileman file die'],
                body: [
                    'NEW FDA,MSG',
                    'KILL FDA,MSG',
                    '; SET FDA(file,iens,field)=value',
                    'DO FILE^DIE(${1:\"\"},\"FDA\",\"MSG\")',
                    '$0'
                ]
            }),
            'fileman-help-die-call': mk('HELP^DIE', {
                name: 'FileMan: HELP^DIE(...)',
                prefixes: ['HELP^DIE'],
                body: [
                    'HELP^DIE(${1:FILE},${2:\"\"},${3:FIELD},${4:\"\"},${5:\"MSG\"})$0'
                ]
            }),
            'fileman-help-die-do': mk('HELP^DIE', {
                name: 'FileMan: DO HELP^DIE(...)',
                prefixes: ['fileman help', 'fileman help die'],
                body: [
                    'DO HELP^DIE(${1:FILE},${2:\"\"},${3:FIELD},${4:\"\"},${5:\"MSG\"})$0'
                ]
            }),
            'fileman-classic-dic': mk('^DIC', {
                name: 'FileMan: ^DIC (classic lookup)',
                prefixes: ['^DIC', 'fileman dic', 'fileman lookup'],
                body: [
                    'NEW DIC,DIC(0),X,Y',
                    'SET DIC=${1:\"^DIC(\"},DIC(0)=${2:\"AEMQ\"},X=${3:\"\"}',
                    'DO ^DIC',
                    'IF Y<0 QUIT',
                    'SET ${4:IEN}=+Y',
                    '$0'
                ]
            }),
            'fileman-classic-die': mk('^DIE', {
                name: 'FileMan: ^DIE (classic edit)',
                prefixes: ['^DIE', 'fileman die', 'fileman edit'],
                body: [
                    'NEW DIE,DA,DR',
                    'SET DIE=${1:\"^DPT(\"},DA=${2:IEN},DR=${3:\".01;\"}',
                    'DO ^DIE',
                    '$0'
                ]
            })
        };
    };

    const isHoverEnabled = (settings) => (settings?.mumps?.fileman?.hoverDocsEnabled !== false);

    const findApiAt = (line, idx0) => {
        const s = String(line || '');
        const idx = Number(idx0) || 0;
        if (!s || idx < 0 || idx >= s.length) return null;

        const candidates = [];
        const addMatches = (re) => {
            re.lastIndex = 0;
            let m;
            while ((m = re.exec(s))) {
                const start = m.index;
                const end = start + m[0].length;
                if (idx >= start && idx <= end) {
                    candidates.push({ text: m[0], start, end });
                }
            }
        };

        addMatches(/(\$\$)?[A-Za-z%][A-Za-z0-9]*\^[A-Za-z%][A-Za-z0-9]*/g);
        addMatches(/\^[A-Za-z%][A-Za-z0-9]*/g);

        if (!candidates.length) return null;
        candidates.sort((a, b) => (b.end - b.start) - (a.end - a.start));
        const best = candidates[0];

        const normalized = String(best.text || '').toUpperCase().replace(/^\$\$/, '');
        const api = APIS[normalized] || null;
        if (!api) return null;

        return { api, start: best.start, end: best.end };
    };

    let hoverDisposable = null;
    let unsubscribe = null;
    let enabledCache = true;

    const registerHoverProvider = ({ monaco, settingsService } = {}) => {
        if (!monaco?.languages?.registerHoverProvider) return null;

        try { hoverDisposable?.dispose?.(); } catch (_) { }
        hoverDisposable = null;

        try { unsubscribe?.(); } catch (_) { }
        unsubscribe = null;

        enabledCache = isHoverEnabled(settingsService?.get?.());
        if (settingsService?.subscribe) {
            unsubscribe = settingsService.subscribe((next) => {
                enabledCache = isHoverEnabled(next);
            });
        }

        hoverDisposable = monaco.languages.registerHoverProvider('mumps', {
            provideHover: (model, position) => {
                if (!enabledCache) return null;
                if (!model || !position) return null;

                const line = model.getLineContent?.(position.lineNumber)
                    ?? model.getValueInRange?.({
                        startLineNumber: position.lineNumber,
                        startColumn: 1,
                        endLineNumber: position.lineNumber,
                        endColumn: model.getLineMaxColumn?.(position.lineNumber) || 1
                    })
                    ?? '';

                const idx0 = Math.max(0, (position.column || 1) - 1);
                const found = findApiAt(line, idx0);
                if (!found) return null;

                const startColumn = found.start + 1;
                const endColumn = found.end + 1;

                return {
                    range: new monaco.Range(position.lineNumber, startColumn, position.lineNumber, endColumn),
                    contents: [{ value: buildApiMarkdown(found.api) }]
                };
            }
        });

        return hoverDisposable;
    };

    const getDefaultSnippets = () => buildDefaultSnippets();

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
        window.AhmadIDEModules.mumps.fileman = window.AhmadIDEModules.mumps.fileman || {};
        window.AhmadIDEModules.mumps.fileman.APIS = APIS;
        window.AhmadIDEModules.mumps.fileman.buildApiMarkdown = buildApiMarkdown;
        window.AhmadIDEModules.mumps.fileman.getDefaultSnippets = getDefaultSnippets;
        window.AhmadIDEModules.mumps.fileman.registerHoverProvider = registerHoverProvider;
        window.AhmadIDEModules.mumps.fileman.normalizeTriggerText = normalizeTriggerText;
    }
})();
