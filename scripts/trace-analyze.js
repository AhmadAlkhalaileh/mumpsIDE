#!/usr/bin/env node
/* eslint-disable no-console */

const fs = require('fs');

function readJson(path) {
    const raw = fs.readFileSync(path, 'utf8');
    return JSON.parse(raw);
}

function ms(us) {
    return us / 1000;
}

function pickMainThread(events) {
    const meta = events.filter(e => e && e.ph === 'M' && e.name === 'thread_name' && e.args && typeof e.args.name === 'string');
    const rendererMain = meta.find(e => /CrRendererMain/i.test(e.args.name));
    if (rendererMain) return { pid: rendererMain.pid, tid: rendererMain.tid, name: rendererMain.args.name };

    // Fallback: pick the thread with the most "Task" duration.
    const buckets = new Map(); // key pid:tid -> totalDur
    for (const e of events) {
        if (!e || e.ph !== 'X' || typeof e.dur !== 'number') continue;
        if (e.name !== 'Task' && e.name !== 'RunTask') continue;
        const key = `${e.pid}:${e.tid}`;
        buckets.set(key, (buckets.get(key) || 0) + e.dur);
    }
    let best = null;
    for (const [key, total] of buckets.entries()) {
        if (!best || total > best.total) best = { key, total };
    }
    if (!best) return null;
    const [pidStr, tidStr] = best.key.split(':');
    return { pid: Number(pidStr), tid: Number(tidStr), name: 'unknown' };
}

function summarize(events, { pid, tid }) {
    const main = events.filter(e => e && e.pid === pid && e.tid === tid && e.ph === 'X' && typeof e.ts === 'number');
    const tsMin = Math.min(...main.map(e => e.ts));
    const tsMax = Math.max(...main.map(e => e.ts + (e.dur || 0)));

    const longTasks = main
        .filter(e => (e.name === 'Task' || e.name === 'RunTask') && typeof e.dur === 'number' && e.dur >= 50_000)
        .map(e => ({ name: e.name, durMs: ms(e.dur), tsMs: ms(e.ts - tsMin) }))
        .sort((a, b) => b.durMs - a.durMs)
        .slice(0, 20);

    const sumByName = (names) => {
        const set = new Set(names);
        let total = 0;
        for (const e of main) {
            if (!set.has(e.name)) continue;
            if (typeof e.dur !== 'number') continue;
            total += e.dur;
        }
        return ms(total);
    };

    const metrics = {
        durationMs: ms(tsMax - tsMin),
        longTasksCount: main.filter(e => (e.name === 'Task' || e.name === 'RunTask') && typeof e.dur === 'number' && e.dur >= 50_000).length,
        topLongTasks: longTasks,
        totals: {
            scriptMs: sumByName(['EvaluateScript', 'FunctionCall', 'V8.Execute', 'RunMicrotasks']),
            styleMs: sumByName(['RecalculateStyles', 'UpdateLayoutTree']),
            layoutMs: sumByName(['Layout']),
            paintMs: sumByName(['Paint', 'PaintImage', 'CompositeLayers', 'UpdateLayerTree']),
            gcMs: sumByName(['V8.GCScavenger', 'V8.GCIncrementalMarking', 'V8.GCFinalizeMC', 'V8.GCCompactor'])
        }
    };
    return metrics;
}

function main() {
    const path = process.argv[2];
    if (!path) {
        console.error('Usage: node scripts/trace-analyze.js perf/trace-lag.before.json');
        process.exitCode = 2;
        return;
    }
    const json = readJson(path);
    const events = Array.isArray(json) ? json : (json.traceEvents || []);
    if (!Array.isArray(events) || !events.length) {
        console.error('Trace has no traceEvents');
        process.exitCode = 1;
        return;
    }

    const mainThread = pickMainThread(events);
    if (!mainThread) {
        console.error('Could not locate renderer main thread');
        process.exitCode = 1;
        return;
    }

    const metrics = summarize(events, mainThread);
    console.log(JSON.stringify({ file: path, mainThread, metrics }, null, 2));
}

main();

