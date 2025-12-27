const test = require('node:test');
const assert = require('node:assert/strict');

function setupWindow() {
    global.window = global.window || {};
    window.AhmadIDEModules = window.AhmadIDEModules || {};
    window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
}

setupWindow();

require('../src/editor/mumps/mumps-tag-header-generator.js');

test('Tag header generator: parseTagLine extracts tag name without params', () => {
    const Gen = window.AhmadIDEModules?.mumps?.MumpsTagHeaderGenerator;
    assert.equal(typeof Gen, 'function');

    const gen = new Gen({ showToast: () => { } });

    const result = gen.parseTagLine('SEARCH S X=1');
    assert.deepEqual(result, {
        name: 'SEARCH',
        params: [],
        signature: 'SEARCH'
    });
});

test('Tag header generator: parseTagLine extracts tag name with params', () => {
    const Gen = window.AhmadIDEModules?.mumps?.MumpsTagHeaderGenerator;
    const gen = new Gen({ showToast: () => { } });

    const result = gen.parseTagLine('SEARCH(USRNID,DUZ) S X=1');
    assert.deepEqual(result, {
        name: 'SEARCH',
        params: ['USRNID', 'DUZ'],
        signature: 'SEARCH(USRNID,DUZ)'
    });
});

test('Tag header generator: parseTagLine returns null for non-tag lines', () => {
    const Gen = window.AhmadIDEModules?.mumps?.MumpsTagHeaderGenerator;
    const gen = new Gen({ showToast: () => { } });

    // Indented line
    assert.equal(gen.parseTagLine('    S X=1'), null);

    // Comment line
    assert.equal(gen.parseTagLine('; This is a comment'), null);

    // Empty line
    assert.equal(gen.parseTagLine(''), null);
});

test('Tag header generator: parseTagLine handles by-reference params', () => {
    const Gen = window.AhmadIDEModules?.mumps?.MumpsTagHeaderGenerator;
    const gen = new Gen({ showToast: () => { } });

    const result = gen.parseTagLine('REFTAG(.ARG,X)');
    assert.deepEqual(result, {
        name: 'REFTAG',
        params: ['.ARG', 'X'],
        signature: 'REFTAG(.ARG,X)'
    });
});

test('Tag header generator: generateHeader produces correct format', () => {
    const Gen = window.AhmadIDEModules?.mumps?.MumpsTagHeaderGenerator;
    const gen = new Gen({ showToast: () => { } });

    const header = gen.generateHeader('SEARCH', ['USRNID', 'DUZ'], 'Ahmad Alkhalaileh');

    // Check that name is included
    assert.ok(header.includes('; Name: SEARCH'));

    // Check that author is included
    assert.ok(header.includes('; Author: Ahmad Alkhalaileh'));

    // Check that parameters are on separate lines
    assert.ok(header.includes('; In: USRNID'));
    assert.ok(header.includes(';     DUZ'));

    // Check header lines exist
    assert.ok(header.includes(';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;'));
});

test('Tag header generator: generateHeader handles no params', () => {
    const Gen = window.AhmadIDEModules?.mumps?.MumpsTagHeaderGenerator;
    const gen = new Gen({ showToast: () => { } });

    const header = gen.generateHeader('SIMPLETAG', [], 'Developer');

    assert.ok(header.includes('; Name: SIMPLETAG'));
    assert.ok(header.includes('; In: N/A'));
    assert.ok(header.includes('; Author: Developer'));
});

test('Tag header generator: generateHeader marks by-reference params', () => {
    const Gen = window.AhmadIDEModules?.mumps?.MumpsTagHeaderGenerator;
    const gen = new Gen({ showToast: () => { } });

    const header = gen.generateHeader('REFTAG', ['.ARG'], 'Developer');

    assert.ok(header.includes('(By Reference)'));
});
