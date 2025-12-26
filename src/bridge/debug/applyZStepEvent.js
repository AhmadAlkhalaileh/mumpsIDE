const { log: dbgLog } = require('../../../utils/debug-log');
const { sourceMapCache, loadRoutineSourceMap } = require('./sourceMaps');
const { parseZPos, nextExecutableLine } = require('./sourceMapUtils');

async function applyZStepEvent(session, evt) {
  // Depth from AHMDBG is $STACK-1 (0-based). Treat 0 as a real frame and normalize the stack to this depth.
  const reportedDepth = Number.isInteger(evt.depth) ? evt.depth : null;
  const targetDepth = reportedDepth !== null
    ? Math.max(1, reportedDepth)
    : (session.callStack.length || 1);
  const currentDepth = session.callStack.length || 0;
  const posInfo = parseZPos(evt.pos || '');
  const routine = (evt.routine || posInfo.routine || (session.callStack[currentDepth - 1] || {}).routine || '').toUpperCase();
  const tag = (evt.tag || posInfo.tag || '').toUpperCase();
  const offset = Number.isInteger(evt.offset) ? evt.offset : posInfo.offset;

  // Normalize call stack length to reported depth
  while (session.callStack.length > targetDepth) session.callStack.pop();

  const pushFrame = () => {
    const caller = session.callStack[session.callStack.length - 1] || {};
    const callerSmap = caller.routine === 'TMPDBG' ? session.sourceMap : sourceMapCache[caller.routine];
    const retLine = nextExecutableLine(callerSmap, (caller.line || 0) + 1);
    session.callStack.push({
      routine: routine || caller.routine || 'TMPDBG',
      line: evt.line || 1,
      tag,
      returnRoutine: caller.routine || null,
      returnLine: retLine,
      returnTag: caller.tag || ''
    });
  };

  while (session.callStack.length < targetDepth) {
    pushFrame();
  }

  const top = session.callStack[session.callStack.length - 1] || {};
  top.routine = routine || top.routine;
  top.tag = tag || top.tag;

  let smap = sourceMapCache[top.routine];
  if (!smap) {
    smap = await loadRoutineSourceMap(top.routine, top.routine === 'TMPDBG' ? session.sourceMap : null);
    if (smap) sourceMapCache[top.routine] = smap;
  }

  let targetLine = evt.line || 0;
  let computedFromTagOffset = false;

  // LINE COMPUTATION FIX:
  // ALWAYS prefer tag+offset from $ZPOSITION over evt.line when available,
  // because tag+offset is the authoritative source from the runtime.
  // evt.line from AHMDBG's LINENUM can be incorrect when:
  // - Returning from external calls
  // - Stepping through routines with complex control flow
  // - Source file differs from compiled routine
  if (smap && (tag || Number.isInteger(offset))) {
    let tagLine = 1;
    if (tag) {
      // Find the tag definition line in the source map
      for (const entry of smap.map) {
        if (entry.tag && entry.tag.toUpperCase() === tag.toUpperCase()) {
          tagLine = entry.line;
          break;
        }
      }
    }
    // Compute line from tag+offset
    // offset is relative to the tag line (tag+0 = tag definition line)
    targetLine = tagLine + offset;
    computedFromTagOffset = true;
    dbgLog(`[DEBUG] Computed line from tag+offset: routine=${top.routine}, tag=${tag}, offset=${offset}, tagLine=${tagLine}, targetLine=${targetLine}`);

    // Skip comment-only lines to find the next executable line
    targetLine = nextExecutableLine(smap, targetLine || 1);
    dbgLog(`[DEBUG] After skipping comments: targetLine=${targetLine}`);
  }

  // Fallback to evt.line if tag+offset not available
  if (!targetLine) targetLine = evt.line || top.line || 1;

  top.line = targetLine;
  session.currentRoutine = top.routine;
  session.currentLine = targetLine;
  session.currentTag = top.tag || '';
}

module.exports = {
  applyZStepEvent
};

