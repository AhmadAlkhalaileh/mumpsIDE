const test = require('node:test');
const assert = require('node:assert/strict');

function setupWindow() {
    global.window = global.window || {};
    window.AhmadIDEModules = window.AhmadIDEModules || {};
    window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
    window.AhmadIDEModules.diagnostics = window.AhmadIDEModules.diagnostics || {};
}

setupWindow();

require('../src/editor/mumps/mumps-fileman-api.js');
require('../src/editor/mumps/mumps-new-autocomplete.js');
require('../src/editor/mumps/mumps-routine-compare.js');
require('../src/editor/mumps/mumps-snippets-service.js');
require('../src/editor/mumps/renderer-mumps-monaco.js');
require('../src/editor/diagnostics/renderer-diagnostics.js');

test('NEW autocomplete: detects NEW context after trailing space', () => {
    const Provider = window.AhmadIDEModules?.mumps?.MumpsNewAutocompleteProvider;
    assert.equal(typeof Provider, 'function');

    const provider = new Provider();

    assert.equal(provider.isInNewContext('NEW '), true);
    assert.equal(provider.isInNewContext('    NEW '), true);
    assert.equal(provider.isInNewContext('N '), true);
    assert.equal(provider.isInNewContext('NEW X,'), true);
    assert.equal(provider.isInNewContext('SET X=1'), false);
});

test('Routine compare: loadRoutine reads via ahmadIDE.readRoutine', async () => {
    const Compare = window.AhmadIDEModules?.mumps?.MumpsRoutineCompare;
    assert.equal(typeof Compare, 'function');

    const compare = new Compare({
        monaco: {},
        ahmadIDE: {
            readRoutine: async (name) => ({ ok: true, code: `CODE:${name}` })
        }
    });

    const code = await compare.loadRoutine('TEST1');
    assert.equal(code, 'CODE:TEST1');
});

test('Snippets: completion items expand variables and escape MUMPS $', () => {
    const Service = window.AhmadIDEModules?.mumps?.MumpsSnippetsService;
    assert.equal(typeof Service, 'function');

    const monacoStub = {
        languages: {
            CompletionItemKind: { Snippet: 27 },
            CompletionItemInsertTextRule: { InsertAsSnippet: 4 },
            registerCompletionItemProvider: (_lang, provider) => provider
        }
    };
    window.monaco = monacoStub;

    const snippets = new Service({
        settingsService: { get: (k, d) => (k === 'user.name' ? 'Ahmad' : d) }
    });

    const provider = snippets.registerCompletionProvider(monacoStub);

    const modelStub = {
        uri: { toString: () => 'docker:/localr/HELLO' },
        getValueInRange: () => '',
        getWordUntilPosition: () => ({ word: 'merror', startColumn: 1, endColumn: 7 })
    };

    const res = provider.provideCompletionItems(modelStub, { lineNumber: 1, column: 7 });
    const errSnippet = res.suggestions.find((s) => s.label === 'merror');
    assert.ok(errSnippet, 'Expected merror snippet suggestion');

    assert.match(errSnippet.insertText, /\\\$GET/i);
    assert.match(errSnippet.insertText, /\\\$ECODE/i);
    assert.match(errSnippet.insertText, /\\\$PIECE/i);
    assert.match(errSnippet.insertText, /\\\$TEXT/i);

    const modelEntryStub = {
        uri: { toString: () => 'docker:/localr/HELLO' },
        getValueInRange: () => '',
        getWordUntilPosition: () => ({ word: 'mentry', startColumn: 1, endColumn: 7 })
    };
    const resEntry = provider.provideCompletionItems(modelEntryStub, { lineNumber: 1, column: 7 });
    const entrySnippet = resEntry.suggestions.find((s) => s.label === 'mentry');
    assert.ok(entrySnippet, 'Expected mentry snippet suggestion');
    assert.match(entrySnippet.insertText, /Routine: HELLO/);
    assert.ok(!entrySnippet.insertText.includes('${ROUTINE}'));
});

test('FileMan: snippets can be triggered by full name (TAG^ROUTINE)', () => {
    const Service = window.AhmadIDEModules?.mumps?.MumpsSnippetsService;
    assert.equal(typeof Service, 'function');

    const monacoStub = {
        languages: {
            CompletionItemKind: { Snippet: 27 },
            CompletionItemInsertTextRule: { InsertAsSnippet: 4 },
            registerCompletionItemProvider: (_lang, provider) => provider
        }
    };

    const snippets = new Service({
        settingsService: { get: () => ({ mumps: { fileman: { snippetsEnabled: true } } }) }
    });
    const provider = snippets.registerCompletionProvider(monacoStub);

    const line = 'DO FIND^DIC';
    const modelStub = {
        uri: { toString: () => 'docker:/localr/HELLO' },
        getValueInRange: () => line,
        getWordUntilPosition: () => ({ word: 'DIC', startColumn: 9, endColumn: 12 })
    };

    const res = provider.provideCompletionItems(modelStub, { lineNumber: 1, column: line.length + 1 });
    const findCall = res.suggestions.find((s) => s.label === 'FIND^DIC');
    assert.ok(findCall, 'Expected FIND^DIC FileMan snippet suggestion');
    assert.ok(findCall.insertText.includes('FIND^DIC('));
    assert.ok(String(findCall.documentation?.value || '').toLowerCase().includes('hardhats.org/fileman/pm/db_dicf.htm'));
});

test('FileMan: snippets honor settings toggle (snippetsEnabled=false)', () => {
    const Service = window.AhmadIDEModules?.mumps?.MumpsSnippetsService;
    assert.equal(typeof Service, 'function');

    const monacoStub = {
        languages: {
            CompletionItemKind: { Snippet: 27 },
            CompletionItemInsertTextRule: { InsertAsSnippet: 4 },
            registerCompletionItemProvider: (_lang, provider) => provider
        }
    };

    const snippets = new Service({
        settingsService: { get: () => ({ mumps: { fileman: { snippetsEnabled: false } } }) }
    });
    const provider = snippets.registerCompletionProvider(monacoStub);

    const line = 'DO FIND^DIC';
    const modelStub = {
        uri: { toString: () => 'docker:/localr/HELLO' },
        getValueInRange: () => line,
        getWordUntilPosition: () => ({ word: 'DIC', startColumn: 9, endColumn: 12 })
    };

    const res = provider.provideCompletionItems(modelStub, { lineNumber: 1, column: line.length + 1 });
    assert.equal(res.suggestions.some((s) => s.label === 'FIND^DIC'), false);
});

test('FileMan: hover provider returns signature + HardHats link', () => {
    const fileman = window.AhmadIDEModules?.mumps?.fileman;
    assert.ok(fileman && typeof fileman.registerHoverProvider === 'function');

    let hoverProvider = null;
    const monacoStub = {
        Range: function Range(startLineNumber, startColumn, endLineNumber, endColumn) {
            this.startLineNumber = startLineNumber;
            this.startColumn = startColumn;
            this.endLineNumber = endLineNumber;
            this.endColumn = endColumn;
        },
        languages: {
            registerHoverProvider: (_lang, provider) => {
                hoverProvider = provider;
                return { dispose: () => { } };
            }
        }
    };

    fileman.registerHoverProvider({
        monaco: monacoStub,
        settingsService: { get: () => ({ mumps: { fileman: { hoverDocsEnabled: true } } }) }
    });

    assert.ok(hoverProvider && typeof hoverProvider.provideHover === 'function');

    const line = 'SET X=$$FIND1^DIC(FILE,\"\",\"QX\",.VALUE,.INDEXES,.SCREEN,\"MSG\")';
    const idx = line.indexOf('FIND1^DIC');
    const modelStub = {
        getLineContent: () => line
    };

    const hover = hoverProvider.provideHover(modelStub, { lineNumber: 1, column: idx + 2 });
    assert.ok(hover && hover.contents && hover.contents.length);
    const md = hover.contents[0].value;
    assert.ok(md.includes('$$FIND1^DIC('));
    assert.ok(String(md || '').toLowerCase().includes('hardhats.org/fileman/pm/db_dicf1.htm'));
});

test('Diagnostics: applyLintMarkers includes marker.code for quick fixes', () => {
    const createDiagnosticsManager = window.AhmadIDEModules?.diagnostics?.createDiagnosticsManager;
    assert.equal(typeof createDiagnosticsManager, 'function');

    const recorded = { markers: null };
    const monacoStub = {
        MarkerSeverity: { Error: 8, Warning: 4, Info: 2 },
        editor: {
            setModelMarkers: (_model, _owner, markers) => {
                recorded.markers = markers;
            }
        }
    };

    const manager = createDiagnosticsManager({
        state: {
            maxLintTextLength: 20000,
            maxProblemItems: 100,
            lastValidatedVersionIdRef: { value: null },
            lintSkipNotifiedRef: { value: false },
            regex: {
                RE_DQUOTE: /\"/g,
                RE_PAREN_OPEN: /\(/g,
                RE_PAREN_CLOSE: /\)/g,
                RE_LINE_START: /^[^A-Za-z%;\s]/,
                RE_SUSPICIOUS: /[{}\[\]\\]/
            }
        },
        deps: {
            getMonaco: () => monacoStub
        }
    });

    const modelStub = {};
    manager.applyLintMarkers(modelStub, [{
        severity: 'warning',
        message: 'Trailing whitespace',
        line: 2,
        column: 10,
        ruleId: 'M030'
    }]);

    assert.ok(recorded.markers && recorded.markers.length === 1);
    assert.equal(recorded.markers[0].code, 'M030');
});

test('Tag folding: folds tag block when it has QUIT before next tag', () => {
    const createMgr = window.AhmadIDEModules?.mumpsMonaco?.createMumpsMonacoManager;
    assert.equal(typeof createMgr, 'function');

    let foldingProvider = null;
    const monacoStub = {
        languages: {
            register: () => { },
            setLanguageConfiguration: () => { },
            setMonarchTokensProvider: () => { },
            registerDocumentFormattingEditProvider: () => { },
            registerDocumentRangeFormattingEditProvider: () => { },
            registerFoldingRangeProvider: (_lang, provider) => {
                foldingProvider = provider;
            },
            FoldingRangeKind: { Region: 'region' }
        },
        Range: function Range() { }
    };

    const mgr = createMgr({ deps: { getMonaco: () => monacoStub } });
    mgr.registerMumpsLanguage();

    assert.ok(foldingProvider && typeof foldingProvider.provideFoldingRanges === 'function');

    const lines = [
        'TAG1 ; comment',
        '    NEW X',
        '    QUIT',
        'TAG2 DO QUIT',
        ' . SET Y=1'
    ];
    const modelStub = {
        getLineCount: () => lines.length,
        getLineContent: (i) => lines[i - 1]
    };

    const ranges = foldingProvider.provideFoldingRanges(modelStub, {}, {});
    assert.ok(Array.isArray(ranges));

    const hasTag1 = ranges.some(r => r.start === 1 && r.end === 3);
    const hasTag2 = ranges.some(r => r.start === 4 && r.end === 5);
    assert.equal(hasTag1, true);
    assert.equal(hasTag2, true);
});
