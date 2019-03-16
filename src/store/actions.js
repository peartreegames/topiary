export * from "../node/NodeActions"
export * from "../editor/actor/ActorActions"
export * from "../editor/edit/EditActions"
export * from "../editor/variable/VariableActions"
export * from "../link/LinkActions"
export * from "../nav/NavActions"

export const setWarning = ({ warningMessage, warning }) => ({
  type: "WARNING_MESSAGE",
  warning,
  warningMessage
})

export const addCollapsedNode = ( nodeId ) => {
  return { type: "ADD_COL_NODE", nodeId }
}

export const removeCollapsedNode = (nodeId) => ({ type: "REMOVE_COL_NODE", nodeId })
