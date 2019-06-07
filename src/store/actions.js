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
