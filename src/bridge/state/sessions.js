const debugSessions = {};
const sshSessions = {};
const mdebugSessions = {};

function hasActiveMdebugSession() {
  return Object.keys(mdebugSessions).length > 0;
}

function hasActiveSshSession() {
  return Object.keys(sshSessions).length > 0;
}

module.exports = {
  debugSessions,
  sshSessions,
  mdebugSessions,
  hasActiveMdebugSession,
  hasActiveSshSession
};
