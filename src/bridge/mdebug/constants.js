// Default MDEBUG host/port (using AHMDBG.m on port 9200 to avoid zombie connections on 9000)
const MDEBUG_DEFAULT_HOST = process.env.MDEBUG_HOST || '127.0.0.1';
const MDEBUG_DEFAULT_PORT = parseInt(process.env.MDEBUG_PORT || '9200', 10);

// --- MDEBUG TCP client (parity with mumps-debug-master) ---
const mdebugStates = {
  disconnected: 'disconnected',
  waitingForStart: 'waitingForStart',
  waitingForVars: 'waitingForVars',
  waitingForBreakpoints: 'waitingForBreakpoints',
  waitingForSingleVar: 'waitingForSingleVar',
  waitingForSingleVarContent: 'waitingForSingleVarContent',
  waitingForErrorReport: 'waitingForErrorReport',
  waitingForHints: 'waitingForHints',
  waitingForGlobals: 'waitingForGlobals'
};

module.exports = {
  MDEBUG_DEFAULT_HOST,
  MDEBUG_DEFAULT_PORT,
  mdebugStates
};
