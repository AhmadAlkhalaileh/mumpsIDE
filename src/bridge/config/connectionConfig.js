const connectionConfig = {
  type: 'docker',
  docker: {
    containerId: null, // Will be set when user selects a container
    envKey: null,
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
    envKey: null,
    ydbPath: '/opt/fis-gtm/YDB136',
    gldPath: null,
    routinesPath: null,
    rpcRoutinesPath: null,
    basePath: null
  }
};

module.exports = {
  connectionConfig
};
