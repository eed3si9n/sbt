local localBackend(blockSizeBytes, keyEntries, newBlocks) = {
  'local': {
    keyLocationMap: {
      inMemory: { entries: keyEntries },
      maximumGetAttempts: 16,
      maximumPutAttempts: 64,
    },
    oldBlocks: 8,
    currentBlocks: 24,
    newBlocks: newBlocks,
    blocksInMemory: { blockSizeBytes: blockSizeBytes },
  },
};

{
  grpcServers: [{
    listenAddresses: ['127.0.0.1:2024'],
    authenticationPolicy: { allow: {} },
  }],
  maximumMessageSizeBytes: 4 * 1024 * 1024,
  global: {
    diagnosticsHttpServer: {
      httpServers: [{
        listenAddresses: ['127.0.0.1:8000'],
        authenticationPolicy: { allow: {} },
      }],
      enablePrometheus: true,
    },
  },
  contentAddressableStorage: {
    backend: localBackend(64 * 1024 * 1024, 1024 * 1024, 3),
    getAuthorizer: { allow: {} },
    putAuthorizer: { allow: {} },
    findMissingAuthorizer: { allow: {} },
  },
  actionCache: {
    backend: localBackend(4 * 1024 * 1024, 256 * 1024, 1),
    getAuthorizer: { allow: {} },
    putAuthorizer: { allow: {} },
  },
}
