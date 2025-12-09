#!/usr/bin/env node

// Lightweight performance probe for the core MUMPS parsing utilities.
// Uses an expanded copy of AHMDBG.m to stress the lexer, parser, linter, and validator.

const fs = require('fs');
const path = require('path');

const repoRoot = path.resolve(__dirname, '..');
process.chdir(repoRoot); // keep relative fetch() paths working for linter/validator assets

const { MUMPSLexer } = require('../assets/mumps/mumps-lexer.js');
const { MUMPSParser } = require('../assets/mumps/mumps-parser.js');
const { MUMPSLinter } = require('../assets/mumps/mumps-linter.js');
const { MUMPSValidator } = require('../assets/mumps/mumps-validator.js');

const SAMPLE_PATH = path.join(repoRoot, 'AHMDBG.m');
const sample = fs.readFileSync(SAMPLE_PATH, 'utf8');
const REPEAT_COUNT = 20; // multiply the sample to increase workload
const workload = Array.from({ length: REPEAT_COUNT }, () => sample).join('\n');
const workloadLines = workload.split('\n').length;
const workloadSizeKb = (Buffer.byteLength(workload) / 1024).toFixed(1);

function percentile(sorted, p) {
    if (!sorted.length) return 0;
    const idx = (sorted.length - 1) * p;
    const lower = Math.floor(idx);
    const upper = Math.ceil(idx);
    if (lower === upper) return sorted[lower];
    return sorted[lower] + (sorted[upper] - sorted[lower]) * (idx - lower);
}

function summarize(samples) {
    if (!samples.length) {
        return { avg: 0, min: 0, max: 0, p50: 0, p95: 0 };
    }
    const sorted = [...samples].sort((a, b) => a - b);
    const sum = samples.reduce((a, b) => a + b, 0);
    return {
        avg: sum / samples.length,
        min: sorted[0],
        max: sorted[sorted.length - 1],
        p50: percentile(sorted, 0.5),
        p95: percentile(sorted, 0.95)
    };
}

async function bench(label, iterations, fn) {
    const times = [];
    for (let i = 0; i < iterations; i += 1) {
        const start = process.hrtime.bigint();
        await fn();
        const end = process.hrtime.bigint();
        times.push(Number(end - start) / 1e6);
    }
    return { label, iterations, stats: summarize(times) };
}

function logResult(result) {
    const { label, iterations, stats } = result;
    console.log(`${label} (${iterations} runs): avg ${stats.avg.toFixed(2)}ms | p95 ${stats.p95.toFixed(2)}ms | min ${stats.min.toFixed(2)}ms | max ${stats.max.toFixed(2)}ms`);
}

async function main() {
    console.log('Performance bench for Ahmad IDE core parsing utilities');
    console.log(`Workload: ${SAMPLE_PATH} x${REPEAT_COUNT} -> ${workloadLines} lines (${workloadSizeKb} KB)`);

    const runs = 8;
    const results = [];

    results.push(await bench('Lexer tokenize + filter', runs, () => {
        const lexer = new MUMPSLexer();
        lexer.tokenize(workload);
        lexer.getSignificantTokens();
    }));

    results.push(await bench('Parser build AST', runs, () => {
        const parser = new MUMPSParser();
        parser.parse(workload);
    }));

    const linter = new MUMPSLinter();
    await linter.loadRules(); // ensure rules from linter-rules.json are in memory
    results.push(await bench('Linter ruleset check', runs, () => {
        linter.lint(workload);
    }));

    const validator = new MUMPSValidator();
    await validator.loadTokens(); // ensure token set ready
    const names = ['FOO', 'BAR1', 'BAZ2', 'QWERTYUIOPASDF', 'abc', '1BAD', 'A$B', ''];
    results.push(await bench('Validator batch (8 names)', runs, () => {
        names.forEach((name) => validator.validateRoutineName(name));
    }));

    console.log('\nResults:');
    results.forEach(logResult);
}

main().catch((err) => {
    console.error('Benchmark failed:', err);
    process.exitCode = 1;
});
