const { log: dbgLog } = require('../../../utils/debug-log');
const { debugSessions } = require('../state/sessions');
const { sourceMapCache } = require('./sourceMaps');
const { payloadLineToUserLine, formatCallStackForClient } = require('./sourceMapUtils');
const { decodeMString, normalizeDebuggerVars } = require('./vars');
const { waitForEvent, waitForZStepEvent, consumeSessionOutput, pullQueuedEvent } = require('./zstepEventQueue');
const { applyZStepEvent } = require('./applyZStepEvent');

async function fetchZStepVariables(session) {
  if (!session || session.engine !== 'zstep') return {};
  if (session.procExited) return {};

  try {
    dbgLog('[DEBUG] Fetching variables via GETVARS...');
    session.proc.stdin.write('GETVARS\n');

    // Wait for vars event (or exit/error)
    const evt = await waitForEvent(session, ['vars', 'exit', 'error'], 4000);
    dbgLog('[DEBUG] GETVARS response:', JSON.stringify(evt));

    if (evt.event === 'vars' && evt.vars) {
      dbgLog('[DEBUG] Variables fetched:', Object.keys(evt.vars).length, 'variables');
      return normalizeDebuggerVars(evt.vars);
    }

    dbgLog('[DEBUG] No variables returned from GETVARS');
    return {};
  } catch (err) {
    dbgLog('[DEBUG] Error fetching variables:', err.message);
    return {};
  }
}

async function sendZStepCommand(sessionId, command) {
  const session = debugSessions[sessionId];
  if (!session || session.engine !== 'zstep') return { ok: false, error: 'Session not found' };
  if (session.procExited) {
    return { ok: false, error: 'Program finished', output: (session.output || []).join('\n') };
  }

  // Show what line we're currently at in the source code
  // Use the correct source map based on the current routine
  const currentRoutineSmap = session.currentRoutine === 'TMPDBG'
    ? session.sourceMap
    : sourceMapCache[session.currentRoutine];
  const currentLineText = currentRoutineSmap?.lines?.[session.currentLine - 1] || '(unknown)';
  dbgLog('[DEBUG] Current position before step:', {
    line: session.currentLine,
    routine: session.currentRoutine,
    tag: session.currentTag,
    text: currentLineText.trim()
  });

  dbgLog('[DEBUG] Sending step command:', command);
  dbgLog('[runtime] Sending command', {
    command,
    currentLine: session.currentLine,
    currentRoutine: session.currentRoutine,
    currentLineText: currentLineText.trim(),
    breakpoints: Array.from(session.manualBreakpoints || [])
  });

  try {
    session.proc.stdin.write(`${command}\n`);
  } catch (err) {
    dbgLog('[DEBUG] ERROR writing to stdin:', err.message);
    return { ok: false, error: 'Failed to send command: ' + err.message };
  }

  dbgLog('[DEBUG] Waiting for response to:', command);
  const evt = await waitForZStepEvent(session);
  dbgLog('[DEBUG] Received event for', command, ':', JSON.stringify(evt));

  dbgLog('[runtime] Received event', {
    command,
    event: evt.event,
    pos: evt.pos,
    depth: evt.depth,
    message: evt.message
  });

  if (evt.event === 'error') {
    dbgLog('[runtime] Error event', { error: evt.message });
    return { ok: false, error: evt.message || 'Runtime error', output: consumeSessionOutput(session) };
  }
  if (evt.event === 'exit') {
    session.procExited = true;
    dbgLog('[runtime] Program exited', { output: session.output });
    return { ok: false, error: 'Program finished', output: consumeSessionOutput(session) };
  }
  if (evt.event === 'stopped') {
    await applyZStepEvent(session, evt);

    // Fetch current variables from GT.M
    session.locals = await fetchZStepVariables(session);

    dbgLog('[runtime] Stopped after command', {
      command,
      currentLine: session.currentLine,
      currentRoutine: session.currentRoutine,
      currentTag: session.currentTag,
      atBreakpoint: session.manualBreakpoints?.has(session.currentLine),
      localsCount: Object.keys(session.locals || {}).length
    });
  }

  // Map TMPDBG payload line back to user's editor coordinates
  const isHeaderPos = session.currentRoutine === 'TMPDBG' &&
    Number.isInteger(session.headerLines) &&
    session.headerLines > 0 &&
    session.currentLine <= session.headerLines;

  // Auto-step over header lines for any step command (CONTINUE, INTO, OVER, OUTOF)
  // BUT: Only during initial startup (before breakpoints are installed).
  // Once breakpoints are installed, we're in user code and shouldn't auto-step.
  // This prevents the "double step" bug where the first step from a breakpoint doesn't advance.
  dbgLog('[DEBUG] Checking auto-step: isHeaderPos=', isHeaderPos, 'command=', command, 'currentLine=', session.currentLine, 'headerLines=', session.headerLines, 'breakpointsInstalled=', session.breakpointsInstalled);
  if (isHeaderPos && !session.breakpointsInstalled && (command === 'CONTINUE' || command === 'INTO' || command === 'OVER' || command === 'OUTOF')) {
    dbgLog('[DEBUG] *** AUTO-STEPPING ACTIVATED *** Stopped in header at line', session.currentLine, '- auto-stepping to reach user code');

    const installPendingBreakpoints = async () => {
      if (!session.pendingBreakpoints || session.pendingBreakpoints.length === 0 || session.breakpointsInstalled) return;
      dbgLog('[DEBUG] Installing', session.pendingBreakpoints.length, 'breakpoints now (after header reach)...');
      let installedCount = 0;
      for (const bp of session.pendingBreakpoints) {
        const sendBpCmd = async (cmd, label) => {
          try {
            session.proc.stdin.write(`${cmd}\n`);
            const cmdParts = cmd.split(';');
            const offForLog = cmdParts.length >= 4 ? cmdParts[3] : '(unknown)';
            dbgLog('[DEBUG] Pre-installed breakpoint', label, ':', cmd.trim(), `(line ${bp.payloadLine} tag ${bp.tag} offset ${offForLog})`);
            // Wait briefly for bp-set/bp-error so we can retry if needed
            const evt = await waitForEvent(session, ['bp-set', 'bp-error', 'error', 'exit'], 800);
            if (evt && (evt.event === 'bp-set' || evt.event === 'bp-error' || evt.event === 'error' || evt.event === 'exit')) {
              return evt;
            }
          } catch (err) {
            dbgLog('[DEBUG] ERROR pre-installing breakpoint:', err.message);
          }
          return null;
        };

        // Try multiple candidates to maximize compatibility with GT.M offset rules.
        // Prefer routine-based offsets first because they are always valid, then tag-based forms.
        const candidates = [];
        const routineOffset = Math.max(0, bp.payloadLine - 1); // payload line N => TMPDBG+(N-1)
        candidates.push({ cmd: `SETBP;TMPDBG;TMPDBG;${routineOffset}`, label: 'primary-routine' });

        // Tag-based command computed from mapping (may fail on param labels)
        const primaryCmd = bp.command || `SETBP;TMPDBG;${bp.tag};${bp.offset}`;
        candidates.push({ cmd: primaryCmd, label: 'tag-mapped' });

        if (bp.labelIsNonExecutable && Number.isInteger(bp.rawOffset) && bp.rawOffset !== bp.offset) {
          candidates.push({ cmd: `SETBP;TMPDBG;${bp.tag};${bp.rawOffset}`, label: 'tag-raw' });
        }

        // Some GT.M builds only accept offset 0 after a tag-only line; try it explicitly
        if (bp.labelIsNonExecutable && bp.offset !== 0) {
          candidates.push({ cmd: `SETBP;TMPDBG;${bp.tag};0`, label: 'tag-zero' });
        }

        let success = false;
        for (const cand of candidates) {
          const evt = await sendBpCmd(cand.cmd, cand.label);
          if (!evt) continue;
          if (evt.event === 'bp-set') {
            success = true;
            installedCount += 1;
            break;
          }
          if (evt.event === 'error' || evt.event === 'exit') {
            dbgLog('[DEBUG] Breakpoint install aborted due to process state:', evt.event);
            break;
          }
          // bp-error -> try next candidate
        }

        if (!success) {
          dbgLog('[DEBUG] Breakpoint still failed after trying all strategies:', bp);
        }
      }
      session.breakpointsInstalled = true;
      session.breakpointsSetCount = installedCount;
      // Give AHMDBG time to process breakpoints
      await new Promise(resolve => setTimeout(resolve, 100));
    };

    let stepCount = 0;
    const maxSteps = 20; // Prevent infinite loops

    while (session.currentLine <= session.headerLines && stepCount < maxSteps) {
      dbgLog('[DEBUG] Auto-step', stepCount + 1, ': stepping from header line', session.currentLine);
      try {
        session.proc.stdin.write('INTO\n');
        const nextEvt = await waitForZStepEvent(session);
        dbgLog('[DEBUG] Auto-step received event:', nextEvt.event, 'line:', nextEvt.line);

        if (nextEvt.event === 'stopped') {
          await applyZStepEvent(session, nextEvt);
          session.locals = await fetchZStepVariables(session);
          stepCount++;

          // Check if we're now past the header
          if (session.currentLine > session.headerLines) {
            dbgLog('[DEBUG] *** AUTO-STEP COMPLETE *** Reached user code at line', session.currentLine);
            break;
          }
        } else if (nextEvt.event === 'exit') {
          session.procExited = true;
          return { ok: false, error: 'Program finished', output: consumeSessionOutput(session) };
        } else if (nextEvt.event === 'error') {
          return { ok: false, error: nextEvt.message || 'Runtime error', output: consumeSessionOutput(session) };
        } else {
          break; // Unknown event, stop stepping
        }
      } catch (err) {
        dbgLog('[DEBUG] Error auto-stepping over header:', err.message);
        break;
      }
    }

    if (stepCount >= maxSteps) {
      dbgLog('[DEBUG] WARNING: Reached max auto-step limit, still in header');
    }

    // Now that we're at user code, install breakpoints (after header) before continuing
    if (session.currentLine > session.headerLines && session.pendingBreakpoints && session.pendingBreakpoints.length > 0 && !session.breakpointsInstalled) {
      await installPendingBreakpoints();
    }

    // If user clicked CONTINUE and we have breakpoints, continue to the first breakpoint
    if (command === 'CONTINUE' && session.breakpointsInstalled && session.pendingBreakpoints && session.pendingBreakpoints.length > 0) {
      if (session.breakpointsSetCount && session.breakpointsSetCount > 0) {
        dbgLog('[DEBUG] Continuing to first breakpoint...');
        session.proc.stdin.write('CONTINUE\n');
        const bpEvt = await waitForZStepEvent(session);

        if (bpEvt.event === 'stopped') {
          await applyZStepEvent(session, bpEvt);
          session.locals = await fetchZStepVariables(session);
          dbgLog('[DEBUG] Stopped at breakpoint, line:', session.currentLine);
        } else if (bpEvt.event === 'exit') {
          session.procExited = true;
          return { ok: false, error: 'Program finished', output: consumeSessionOutput(session) };
        } else if (bpEvt.event === 'error') {
          return { ok: false, error: bpEvt.message || 'Runtime error', output: consumeSessionOutput(session) };
        }
      } else {
        dbgLog('[DEBUG] No breakpoints were installed (all attempts failed); performing manual run-to-line fallback.');
        const targetPayload = (session.pendingBreakpoints[0] || {}).payloadLine;
        if (Number.isInteger(targetPayload) && targetPayload > session.currentLine) {
          const maxSteps = Math.max(50, (targetPayload - session.currentLine) + 10);
          let steps = 0;
          let done = false;
          while (steps < maxSteps && !done) {
            try {
              session.proc.stdin.write('INTO\n');
              const stepEvt = await waitForZStepEvent(session);
              if (stepEvt.event === 'stopped') {
                await applyZStepEvent(session, stepEvt);
                session.locals = await fetchZStepVariables(session);
                steps += 1;
                if (session.currentLine >= targetPayload) {
                  dbgLog('[DEBUG] Manual run-to-line reached target payload line', targetPayload, 'after', steps, 'steps');
                  done = true;
                  break;
                }
              } else if (stepEvt.event === 'exit' || stepEvt.event === 'error') {
                dbgLog('[DEBUG] Manual run-to-line aborted due to event:', stepEvt.event);
                done = true;
                break;
              } else {
                break;
              }
            } catch (err) {
              dbgLog('[DEBUG] Manual run-to-line error:', err.message);
              break;
            }
          }
          if (!done) {
            dbgLog('[DEBUG] Manual run-to-line fallback did not reach target (steps:', steps, 'target:', targetPayload, ')');
          }
        } else {
          dbgLog('[DEBUG] No valid target payload line for manual run-to-line fallback.');
        }
      }
    }
  } else {
    dbgLog('[DEBUG] Auto-step NOT triggered');
  }

  // Recalculate isHeaderPos after potential auto-stepping
  let stillInHeader = session.currentRoutine === 'TMPDBG' &&
    Number.isInteger(session.headerLines) &&
    session.headerLines > 0 &&
    session.currentLine <= session.headerLines;

  // EXTERNAL ROUTINE FIX:
  // Only convert payload line to user line for TMPDBG, not for external routines
  // External routines already have the correct line numbers from the source map
  let userLine;
  if (session.currentRoutine === 'TMPDBG') {
    // For TMPDBG, convert payload line to user line
    userLine = payloadLineToUserLine(session, session.currentLine);
    if (stillInHeader && session.lastUserLine) {
      // Stay on the last real user line instead of bouncing to TMPDBG header
      userLine = session.lastUserLine;
    }
    // Update last user line
    if (stillInHeader && session.lastUserLine) {
      userLine = session.lastUserLine;
    } else {
      session.lastUserLine = userLine;
    }
  } else {
    // For external routines, use the line number as-is (already correct from applyZStepEvent)
    userLine = session.currentLine;
    dbgLog(`[DEBUG] External routine ${session.currentRoutine}: using line ${userLine} directly (no payload mapping)`);
  }

  const clientCallStack = (session.callStack || []).map((frame) => {
    // Only convert TMPDBG lines, leave external routine lines as-is
    const frameRoutine = frame.routine || 'TMPDBG';
    return {
      ...frame,
      line: frameRoutine === 'TMPDBG'
        ? payloadLineToUserLine(session, frame.line || frame.returnLine || 1)
        : (frame.line || frame.returnLine || 1),
      returnLine: frameRoutine === 'TMPDBG'
        ? payloadLineToUserLine(session, frame.returnLine || null)
        : (frame.returnLine || null)
    };
  });

  dbgLog(`[DEBUG] Command ${command} completed. Routine: ${session.currentRoutine}, Line: ${userLine} (raw: ${session.currentLine})`);

  return {
    ok: true,
    currentLine: userLine,
    currentRoutine: session.currentRoutine,
    currentTag: session.currentTag,
    callStack: clientCallStack,
    stack: formatCallStackForClient(clientCallStack),
    locals: session.locals || {},
    output: consumeSessionOutput(session)
  };
}

async function sendZStepEval(sessionId, code = '') {
  const session = debugSessions[sessionId];
  if (!session || session.engine !== 'zstep') return { ok: false, error: 'Session not found' };
  if (session.procExited) {
    return { ok: false, error: 'Program finished', output: consumeSessionOutput(session) };
  }

  const prevPayloadLine = session.currentLine || 0;
  const prevUserLine = payloadLineToUserLine(session, prevPayloadLine);
  const safeCode = (code || '').replace(/\r/g, '');
  try {
    session.proc.stdin.write(`EVAL;${safeCode}\n`);
  } catch (err) {
    return { ok: false, error: 'Failed to send eval: ' + err.message };
  }
  const EVAL_TIMEOUT_MS = 15000;
  let evt = await waitForEvent(session, ['eval', 'error', 'exit'], EVAL_TIMEOUT_MS);
  // If the M side is a bit slow, give one more chance to consume a late eval event
  if (evt && evt.event === 'error' && evt.message === 'Timeout waiting for debugger event') {
    const queuedEval = pullQueuedEvent(session, 'eval');
    if (queuedEval) {
      evt = queuedEval;
    } else {
      const graceEvt = await waitForEvent(session, ['eval', 'error', 'exit'], 4000);
      if (graceEvt) evt = graceEvt;
    }
  }
  if (!evt) return { ok: false, error: 'Eval timeout' };
  if (evt.event === 'exit') {
    session.procExited = true;
    return { ok: false, error: 'Program finished', output: consumeSessionOutput(session) };
  }
  if (evt.event === 'error') {
    return { ok: false, error: evt.message || 'Runtime error', output: consumeSessionOutput(session) };
  }
  if (evt.event === 'eval') {
    if (evt.ok === 0) {
      return { ok: false, error: evt.error || 'Eval failed', output: consumeSessionOutput(session) };
    }
    const locals = normalizeDebuggerVars(evt.locals || {});
    session.locals = locals;
    return {
      ok: true,
      output: `${consumeSessionOutput(session)}${decodeMString(evt.output || '')}`,
      locals
    };
  }
  return { ok: false, error: 'Unexpected eval response' };
}

module.exports = {
  fetchZStepVariables,
  sendZStepCommand,
  sendZStepEval
};

