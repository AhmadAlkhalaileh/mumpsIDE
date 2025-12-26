const { log: dbgLog } = require('../../../utils/debug-log');

const { debugSessions } = require('../state/sessions');

const { consumeSessionOutput } = require('./zstepEventQueue');

const { sendZStepCommand, sendZStepEval } = require('./zstepCommands');

module.exports = {
  async debugContinue(sessionId) {
    const session = debugSessions[sessionId];
    if (!session) return { ok: false, error: 'Session not found' };

    if (session.engine === 'zstep') {
      return sendZStepCommand(sessionId, 'CONTINUE');
    }

    // Execute all lines until next breakpoint or end
    while (session.currentLine <= session.lines.length) {
      // Check if current line is a breakpoint
      if (session.breakpoints.includes(session.currentLine)) {
        break;
      }

      // Execute current line
      const stepResult = await this.debugStep(sessionId, 'over');
      if (!stepResult.ok) break;
    }

    return {
      ok: true,
      currentLine: session.currentLine,
      currentRoutine: session.currentRoutine,
      locals: session.locals,
      stack: session.stack,
      output: session.output.join('\n')
    };
  },

  async debugEval(sessionId, code) {
    const session = debugSessions[sessionId];
    if (!session) return { ok: false, error: 'Session not found' };
    if (session.engine === 'zstep') {
      return sendZStepEval(sessionId, code);
    }
    return { ok: false, error: 'Eval not supported for this engine' };
  },

  async debugStop(sessionId) {
    const session = debugSessions[sessionId];
    if (session && session.engine === 'zstep') {
      try {
        // Send HALT command to allow clean exit and capture final output
        if (session.proc && !session.procExited && session.proc.stdin.writable) {
          session.proc.stdin.write('HALT\n');
          // Wait a bit for process to exit cleanly
          await new Promise(r => setTimeout(r, 300));
        }
        // Force kill if still running
        if (session.proc && !session.procExited) {
          session.proc.kill('SIGTERM');
          await new Promise(r => setTimeout(r, 150));
          if (!session.procExited) {
            session.proc.kill('SIGKILL');
          }
        }
      } catch (e) {
        dbgLog('[DEBUG] Error during stop:', e.message);
      }
      // Return accumulated output to show in terminal
      const output = consumeSessionOutput(session);
      session.procExited = true;
      delete debugSessions[sessionId];
      return { ok: true, output };
    }
    delete debugSessions[sessionId];
    return { ok: true };
  },
};

