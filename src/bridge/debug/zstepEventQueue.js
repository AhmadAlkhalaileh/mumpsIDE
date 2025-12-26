const { log: dbgLog } = require('../../../utils/debug-log');

function resolvePending(session, evt) {
  // If someone is waiting, resolve ONE pending callback
  if (session.pending && session.pending.length > 0) {
    const resolver = session.pending.shift();
    dbgLog('[DEBUG] resolvePending: resolving pending callback with event:', evt.event);
    resolver(evt);
  } else {
    // No one waiting - queue the event for later
    session.eventQueue = session.eventQueue || [];
    session.eventQueue.push(evt);
    dbgLog('[DEBUG] resolvePending: queued event (no pending callbacks):', evt.event, 'queue length:', session.eventQueue.length);
  }
}

function pullQueuedEvent(session, eventName) {
  if (!session || !session.eventQueue || !eventName) return null;
  for (let i = 0; i < session.eventQueue.length; i += 1) {
    if ((session.eventQueue[i] || {}).event === eventName) {
      return session.eventQueue.splice(i, 1)[0];
    }
  }
  return null;
}

function tryParseDebuggerEvent(line) {
  if (!line) return null;

  const attempts = [line];

  // If stdout had leading noise before the JSON, try parsing from the first brace onward
  const braceIdx = line.indexOf('{');
  if (braceIdx > 0) {
    attempts.push(line.slice(braceIdx));
  }

  // Attempt to strip non-printable control chars that can break JSON.parse
  const noControl = line.replace(/[\u0000-\u001F]+/g, '');
  if (noControl !== line) attempts.push(noControl);

  // If we see backslash-escaped quotes, also try a variant where we collapse any over-escaped sequences.
  const collapsed = line.replace(/\\\\+"/g, '\\"');
  if (collapsed !== line) attempts.push(collapsed);

  for (const candidate of attempts) {
    try {
      return JSON.parse(candidate);
    } catch (_) {
      // keep trying
    }
  }

  // Last-chance fallback: extract the event type and output so we can still resolve the pending promise
  const eventMatch = line.match(/"event"\s*:\s*"([^"]+)"/);
  if (!eventMatch) return null;
  const evtType = eventMatch[1];
  const fallback = { event: evtType, raw: line };

  const okMatch = line.match(/"ok"\s*:\s*(\d+)/);
  if (okMatch) fallback.ok = parseInt(okMatch[1], 10);

  const outputMatch = line.match(/"output"\s*:\s*"((?:\\.|[^"])*)"/);
  if (outputMatch) {
    // Preserve the escaped content; decode minimal backslash-escaped quotes so UI can show it.
    fallback.output = outputMatch[1].replace(/\\\\/g, '\\').replace(/\\"/g, '"');
  }

  // Note: locals parsing is intentionally skipped in fallback to keep it simple.
  return fallback;
}

function waitForEvent(session, allowedEvents = ['stopped', 'exit', 'error'], timeoutMs = 10000) {
  return new Promise((resolve) => {
    const allowed = new Set(allowedEvents || []);
    const isAllowed = (evt) => allowed.size === 0 || allowed.has(evt.event);

    const findMatchingQueuedEvent = () => {
      if (!session.eventQueue || session.eventQueue.length === 0) return null;
      for (let i = 0; i < session.eventQueue.length; i++) {
        const evt = session.eventQueue[i];
        if (isAllowed(evt)) {
          session.eventQueue.splice(i, 1);
          return evt;
        }
      }
      return null;
    };

    // Check queue first
    const queuedEvt = findMatchingQueuedEvent();
    if (queuedEvt) {
      resolve(queuedEvt);
      return;
    }

    // No queued event, so wait for one to arrive via resolvePending
    const timer = setTimeout(() => {
      dbgLog('[DEBUG] waitForEvent TIMEOUT after', timeoutMs, 'ms');
      session.pending = (session.pending || []).filter(r => r !== resolver);
      resolve({ event: 'error', message: 'Timeout waiting for debugger event' });
    }, timeoutMs);

    const resolver = (evt) => {
      if (isAllowed(evt)) {
        clearTimeout(timer);
        session.pending = (session.pending || []).filter(r => r !== resolver);
        resolve(evt);
        return;
      }
      // Not for us: keep it queued and keep waiting
      session.eventQueue = session.eventQueue || [];
      session.eventQueue.push(evt);
      session.pending = session.pending || [];
      session.pending.push(resolver);
    };

    // Register this resolver as pending
    session.pending = session.pending || [];
    session.pending.push(resolver);
    dbgLog('[DEBUG] waitForEvent: registered pending callback, queue depth:', session.pending.length);

    // CRITICAL: Check queue again AFTER registering - event might have arrived in between
    const lateEvt = findMatchingQueuedEvent();
    if (lateEvt) {
      clearTimeout(timer);
      session.pending = session.pending.filter(r => r !== resolver);
      dbgLog('[DEBUG] waitForEvent: found late-arriving event in queue:', lateEvt.event);
      resolve(lateEvt);
      return;
    }
  });
}

function waitForZStepEvent(session, timeoutMs = 10000) {
  return waitForEvent(session, ['stopped', 'exit', 'error'], timeoutMs);
}

function handleZStepStdout(session) {
  return (chunk) => {
    const chunkStr = chunk.toString();
    dbgLog('[DEBUG] AHMDBG stdout chunk received, length:', chunkStr.length);
    dbgLog('[DEBUG] AHMDBG stdout raw:', chunkStr.substring(0, 200));
    session.buffer += chunkStr;
    let idx;
    while ((idx = session.buffer.indexOf('\n')) >= 0) {
      const line = session.buffer.slice(0, idx).trim();
      session.buffer = session.buffer.slice(idx + 1);
      if (!line) continue;
      dbgLog('[DEBUG] AHMDBG stdout line:', line);
      try {
        const evt = tryParseDebuggerEvent(line);
        if (evt) {
          dbgLog('[DEBUG] AHMDBG parsed event:', evt.event);
          resolvePending(session, evt);
        } else {
          dbgLog('[DEBUG] AHMDBG output (unparsed):', line);
          session.output.push(line);
        }
      } catch (e) {
        dbgLog('[DEBUG] AHMDBG output (non-JSON):', line);
        session.output.push(line);
      }
    }
  };
}

function consumeSessionOutput(session) {
  if (!session) return '';
  if (session.buffer && session.buffer.trim().length) {
    session.output = session.output || [];
    session.output.push(session.buffer.trim());
    session.buffer = '';
  }
  const outArr = Array.isArray(session.output) ? session.output : [];
  const start = Number.isInteger(session.outputCursor) ? session.outputCursor : 0;
  const slice = outArr.slice(start);
  session.outputCursor = outArr.length;
  return slice.join('\n');
}

module.exports = {
  resolvePending,
  pullQueuedEvent,
  tryParseDebuggerEvent,
  waitForEvent,
  waitForZStepEvent,
  handleZStepStdout,
  consumeSessionOutput
};

