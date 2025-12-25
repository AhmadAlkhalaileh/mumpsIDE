const test = require('node:test');
const assert = require('node:assert/strict');

const {
    parseFindOutput,
    calculateLineDiff,
    mapChangesToProblems
} = require('../src/extensions/compare-with-release/compareWithReleaseUtils');

test('parseFindOutput: parses valid find output with multiple paths', () => {
    const output = `
./moh/localr/ORWPCE1.m
./moh/routines/ORWPCE1.m
./backup/2024/ORWPCE1.m
    `.trim();

    const paths = parseFindOutput(output);

    assert.equal(paths.length, 3);
    assert.equal(paths[0], './moh/localr/ORWPCE1.m');
    assert.equal(paths[1], './moh/routines/ORWPCE1.m');
    assert.equal(paths[2], './backup/2024/ORWPCE1.m');
});

test('parseFindOutput: filters out empty lines', () => {
    const output = `
./moh/localr/ORWPCE1.m

./moh/routines/ORWPCE1.m
    `.trim();

    const paths = parseFindOutput(output);

    assert.equal(paths.length, 2);
});

test('parseFindOutput: filters out non-.m files', () => {
    const output = `
./moh/localr/ORWPCE1.m
./moh/localr/ORWPCE1.txt
./moh/routines/ORWPCE1.m
./backup/ORWPCE1.log
    `.trim();

    const paths = parseFindOutput(output);

    assert.equal(paths.length, 2);
    assert.ok(paths.every((p) => p.endsWith('.m')));
});

test('parseFindOutput: handles empty output', () => {
    assert.deepEqual(parseFindOutput(''), []);
});

test('parseFindOutput: drops permission errors / find noise', () => {
    const output = `
find: '/root': Permission denied
./moh/localr/ORWPCE1.m
find: '/var/log': Permission denied
./moh/routines/ORWPCE1.m
    `.trim();

    const paths = parseFindOutput(output);

    assert.deepEqual(paths, ['./moh/localr/ORWPCE1.m', './moh/routines/ORWPCE1.m']);
});

test('parseFindOutput: handles paths with spaces', () => {
    const output = './my folder/routines/ORWPCE1.m';
    const paths = parseFindOutput(output);

    assert.deepEqual(paths, ['./my folder/routines/ORWPCE1.m']);
});

test('mapChangesToProblems: maps side/severity for change types', () => {
    const changes = [
        { lineNumber: 1, type: 'added', isWhitespaceOnly: false },
        { lineNumber: 2, type: 'removed', isWhitespaceOnly: false },
        { lineNumber: 3, type: 'modified', isWhitespaceOnly: false },
        { lineNumber: 4, type: 'modified', isWhitespaceOnly: true }
    ];

    const problems = mapChangesToProblems(changes);

    assert.equal(problems.length, 4);
    assert.equal(problems[0].side, 'local');
    assert.equal(problems[1].side, 'remote');
    assert.equal(problems[2].side, 'both');
    assert.equal(problems[3].severity, 'info');
    assert.equal(problems[3].isWhitespaceOnly, true);
});

test('calculateLineDiff + mapChangesToProblems: keeps whitespace-only flagged', () => {
    const local = 'line1\n\tindented';
    const remote = 'line1\n    indented';

    const changes = calculateLineDiff(local, remote);
    assert.equal(changes.length, 1);
    assert.equal(changes[0].isWhitespaceOnly, true);

    const problems = mapChangesToProblems(changes);
    assert.equal(problems[0].severity, 'info');
});

