import { combineReducers } from "redux"

import { nodes, focusedNode } from "./nodes/reducer"
import { actors, colors } from "./actor/reducer"
import editor from "./editor/reducer"
import { links, focusedLink } from "./link/reducer"
import variables from "./variable/reducer"
import { scene, search } from "./nav/reducer"

const warning = (state = null, action) => {
  switch (action.type) {
    case "WARNING_MESSAGE":
      return {
        ...state,
        ...action
      }
    default:
      return state
  }
}

const scale = (state = 1, { type, scale }) => {
  if (type === "UPDATE_SCALE") {
    return scale
  }
  return state
}

const id = (state = "") => state

const collapsedNodes = (state = [], { type, nodeId }) => {
  switch (type) {
    case "ADD_COL_NODE":
      return [...state, nodeId]
    case "REMOVE_COL_NODE":
      return state.filter(id => id !== nodeId)
    default:
      return state
  }
}

const defaultActor = (state = "000000", { type, actorId}) => {
  if (type === "DEFAULT_ACTOR") return actorId
  return state
}

export default combineReducers({
  id,
  scene,
  search,
  nodes,
  links,
  defaultActor,
  collapsedNodes,
  focusedNode,
  focusedLink,
  actors,
  variables,
  colors,
  scale,
  editor,
  warning
})
