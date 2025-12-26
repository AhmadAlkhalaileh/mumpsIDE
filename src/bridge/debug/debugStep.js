const { debugSessions } = require('../state/sessions');
const { executeYDBDirect } = require('../ydb/executeYDB');
const { isSkippableDebugLine, advanceToNextExecutableLine } = require('./legacyStepping');
const { sendZStepCommand } = require('./zstepCommands');

module.exports = {
  async debugStep(sessionId, stepType = 'into') {
    const session = debugSessions[sessionId];
    if (!session) return { ok: false, error: 'Session not found' };

    if (session.engine === 'zstep') {
      const cmdMap = { into: 'INTO', over: 'OVER', out: 'OUTOF' };
      // Simplified: Let ZSTEP engine handle the stepping natively.
      // Trying to manually parse and set breakpoints on DO/$$ logic was causing NOPLACE errors.
      return sendZStepCommand(sessionId, cmdMap[stepType]);
    }

    // Ensure call stack exists
    if (!Array.isArray(session.callStack) || !session.callStack.length) {
      session.callStack = [{
        routine: session.currentRoutine || 'TMPDBG',
        line: session.currentLine || 1,
        returnLine: null,
        locals: session.locals || {}
      }];
    }

    // Align to executable line
    advanceToNextExecutableLine(session);
    const topFrame = session.callStack[session.callStack.length - 1];
    if (topFrame) topFrame.line = session.currentLine;
    if (session.currentLine > session.lines.length) {
      return { ok: false, error: 'End of code reached' };
    }

    const currentLineText = session.lines[session.currentLine - 1] || '';
    const trimmed = currentLineText.replace(/^[\t ]+/, '');

    // External routine call, step into: return target without executing
    if (stepType === 'into') {
      const externalCallMatch =
        trimmed.match(/^(?:D|DO)\s+(?:([A-Za-z%][A-Za-z0-9]*)\s*)?\^([A-Za-z%][A-Za-z0-9]+)/i) ||
        trimmed.match(/^SET\s+\w+\s*=\s*\$\$([A-Za-z%][A-Za-z0-9]*)\s*\^([A-Za-z%][A-Za-z0-9]+)/i);
      if (externalCallMatch) {
        const tag = (externalCallMatch[1] || externalCallMatch[3] || '').trim();
        const routine = externalCallMatch[2] || externalCallMatch[4];
        return {
          ok: true,
          isExternalCall: true,
          callTarget: { routine, tag: tag || '' },
          currentLine: session.currentLine,
          locals: session.locals,
          stack: session.callStack.map(f => `${f.routine}:${f.line}`)
        };
      }

      // Local tag call (same routine)
      const localTagMatch = trimmed.match(/^(?:D|DO)\s+([A-Za-z%][A-Za-z0-9]*)\b(?!\s*\^)/i);
      if (localTagMatch) {
        const tag = localTagMatch[1];
        let tagLine = null;
        const tagDefRe = new RegExp(`^${tag}(\\s|;|\\(|$)`, 'i');
        for (let i = 0; i < session.lines.length; i++) {
          const tline = (session.lines[i] || '').replace(/^[\t ]+/, '');
          if (tagDefRe.test(tline)) {
            tagLine = i + 1;
            break;
          }
        }
        if (tagLine) {
          const returnLine = session.currentLine + 1;
          session.callStack.push({
            routine: session.callStack[session.callStack.length - 1].routine,
            line: tagLine,
            tag,
            returnLine,
            locals: { ...session.locals }
          });
          session.currentLine = tagLine;
          advanceToNextExecutableLine(session);
          return {
            ok: true,
            isLocalTagCall: true,
            tagLine: session.currentLine,
            tagName: tag,
            currentLine: session.currentLine,
            locals: session.locals,
            stack: session.callStack.map(f => `${f.routine}:${f.line}`)
          };
        }
      }
    }

    // Build execution code up to current line, skipping comment-only lines
    const linesToExecute = (session.lines || [])
      .slice(0, session.currentLine)
      .map((line) => {
        if (isSkippableDebugLine(line)) return '';
        const raw = line.replace(/\r/g, '');
        const trimmedLine = raw.replace(/^[\t ]+/, '');
        if (/^\S/.test(raw)) {
          const parts = raw.split(/\s+/);
          if (parts.length > 1) return parts.slice(1).join(' ');
          return '';
        }
        return trimmedLine;
      })
      .filter(Boolean);

    let codeToExecute = linesToExecute.join('\n');

    // Extract global variable names from the executed code
    const globalRx = /\^([A-Za-z%][A-Za-z0-9]*)/g;
    const globalsUsed = new Set();
    linesToExecute.forEach(line => {
      let match;
      while ((match = globalRx.exec(line))) {
        globalsUsed.add('^' + match[1]);
      }
    });

    // ZWRITE all locals and explicitly ZWRITE each global
    let zwriteCmd = 'WRITE "<<<DEBUG_VARS_START>>>",!\nZWRITE\n';
    globalsUsed.forEach(g => {
      zwriteCmd += `ZWRITE ${g}\n`;
    });
    zwriteCmd += 'WRITE "<<<DEBUG_VARS_END>>>",!\n';

    codeToExecute += '\n' + zwriteCmd;

    const result = await executeYDBDirect(codeToExecute);
    if (!result.ok) {
      return { ok: false, error: result.error || 'Execution failed' };
    }

    const output = result.stdout || result.output || '';
    const lines = output.split('\n');
    const locals = {};
    const execOutput = [];

    for (const line of lines) {
      const t = line.trim();
      if (!t) continue;
      // Updated regex to capture globals (starting with ^)
      const match = t.match(/^(\^?[A-Za-z%][A-Za-z0-9]*)(\([^)]+\))?\s*=\s*(.*)$/);
      if (match) {
        const varName = match[1].toUpperCase();
        const subscript = match[2];
        let value = match[3].trim();
        if ((value.startsWith('"') && value.endsWith('"')) ||
          (value.startsWith("'") && value.endsWith("'"))) {
          value = value.slice(1, -1);
        }
        if (subscript) {
          if (!locals[varName]) locals[varName] = { _isArray: true, _elements: {} };
          locals[varName]._elements[subscript] = value;
        } else {
          locals[varName] = value;
        }
      } else if (
        !t.includes('ZWRITE') &&
        !t.includes('<<<DEBUG_VARS_START>>>') &&
        !t.includes('<<<DEBUG_VARS_END>>>') &&
        !t.includes('QUIT') &&
        !t.startsWith('>')
      ) {
        execOutput.push(line);
      }
    }

    const executedLineText = trimmed;
    const isQuit = /^\s*Q(?:UIT)?(?:\s|;|$)/i.test(executedLineText);

    if (isQuit && session.callStack.length > 1) {
      const frame = session.callStack.pop();
      session.currentLine = frame.returnLine || session.currentLine + 1;
    } else {
      session.currentLine += 1;
    }

    session.locals = locals;
    session.stack = session.callStack.map(f => `${f.routine}:${f.line}`);
    session.output = (session.output || []).concat(execOutput);

    // Skip comment-only lines after moving
    advanceToNextExecutableLine(session);

    return {
      ok: true,
      currentLine: session.currentLine,
      currentRoutine: (session.callStack[session.callStack.length - 1] || {}).routine || session.currentRoutine || 'TMPDBG',
      locals,
      stack: session.stack,
      output: execOutput.join('\n'),
      isReturn: isQuit && session.callStack.length >= 1
    };
  },
};

