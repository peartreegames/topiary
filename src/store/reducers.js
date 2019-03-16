import { combineReducers } from "redux"

import { nodes, focusedNode } from "../node/NodeReducers"
import { actors, colors } from "../editor/actor/ActorReducers"
import { editor } from "../editor/edit/EditReducers"
import { links,focusedLink } from "../link/LinkReducers"
import variables from "../editor/variable/VariableReducers"
import { scene, search } from "../nav/NavReducers"

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

export default combineReducers({
  id,
  scene,
  search,
  nodes,
  links,
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
