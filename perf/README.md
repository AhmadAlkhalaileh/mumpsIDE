# Performance Traces

Put your exported Chrome DevTools **Performance** trace JSON files here so we can do evidence-based perf work.

## Capture (Renderer)
1. Open DevTools → **Performance**
2. Click **Record**
3. Do this sequence (10–15s total):
   - Open/close a top menu 3×
   - Toggle a bottom tool window (Git/Terminal) 3×
   - Click editor and type ~30 chars quickly
   - Open/close Settings once
4. Stop recording → **Export** → save as:
   - `perf/trace-lag.before.json` (before changes)
   - `perf/trace-lag.after.json` (after changes)

## Analyze
Run:
`node scripts/trace-analyze.js perf/trace-lag.before.json`

Compare with:
`node scripts/trace-analyze.js perf/trace-lag.after.json`

