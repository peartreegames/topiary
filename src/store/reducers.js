import { combineReducers } from "redux"

import { nodes, focusedNode, collapsedNodes } from "./nodes/reducer"
import { actors, defaultActor } from "./actor/reducer"
import { editor } from "./editor/reducer"
import { links, focusedLink } from "./link/reducer"
import { variables } from "./variable/reducer"
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
  scale,
  editor,
  warning
})
