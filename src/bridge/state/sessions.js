const debugSessions = {};
const sshSessions = {};

function hasActiveSshSession() {
  return Object.keys(sshSessions).length > 0;
}

module.exports = {
  debugSessions,
  sshSessions,
  hasActiveSshSession
};
