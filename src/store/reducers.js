import { combineReducers } from "redux"

import { nodes, focusedNode, collapsedNodes } from "./nodes/reducer"
import { globalActors, sceneActors, defaultActor } from "./actor/reducer"
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
  sceneId: id,
  sceneName: scene,
  search,
  defaultActor,
  scale,
  editor,
  warning,
  scene: combineReducers({
    actors: sceneActors,
    collapsedNodes,
    focusedLink,
    focusedNode,
    links,
    nodes,
  }),
  globals: combineReducers({
    actors: globalActors,
    variables
  })
})
