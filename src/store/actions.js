export * from "./actor/action"
export * from "./editor/action"
export * from "./link/action"
export * from "./nav/action"
export * from "./nodes/action"
export * from "./variable/action"

export const setWarning = ({ warningMessage, warning }) => ({
  type: "WARNING_MESSAGE",
  warning,
  warningMessage
})

export const addCollapsedNode = ( nodeId ) => {
  return { type: "ADD_COL_NODE", nodeId }
}

export const removeCollapsedNode = (nodeId) => ({ type: "REMOVE_COL_NODE", nodeId })

export const updateDefaultActor = (actorId) => ({ type: "DEFAULT_ACTOR", actorId })
