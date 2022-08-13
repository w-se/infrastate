(ns infrastate.protocols.resources)

(defprotocol WrappedResourceFn
  (unwrap [_]))

(defprotocol ResourceFnMetaData
  (resource-keys [_]))
