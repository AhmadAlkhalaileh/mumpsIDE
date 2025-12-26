const { DEFAULT_ENV_KEY, DOCKER_DEFAULT_ENV_KEY, buildSshPaths } = require('./paths');

const connectionConfig = {
  type: 'docker',
  docker: {
    containerId: null, // Will be set when user selects a container
    envKey: DOCKER_DEFAULT_ENV_KEY,
    ydbPath: null,  // null = universal mode (no YottaDB paths)
    gldPath: null,
    routinesPath: null,
    rpcRoutinesPath: null,
    basePath: null
  },
  ssh: {
    host: '',
    port: 22,
    username: '',
    password: '',
    ...buildSshPaths(DEFAULT_ENV_KEY)
  }
};

module.exports = {
  connectionConfig
};
