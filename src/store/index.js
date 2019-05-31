import { createStore, applyMiddleware } from "redux"
import { logger } from "redux-logger"
import { composeWithDevTools } from "remote-redux-devtools"
import reducers from "./reducers"

const store = id => {
  const initialState = JSON.parse(localStorage.getItem(id)) || {
    id,
    scene: "",
    search: { status: false, text: "" },
    defaultActor: "000000",
    focusedNode: "",
    focusedLink: { status: false, from: "", to: "" },
    collapsedNodes: [],
    nodes: {},
    links: {},
    editor: true,
    scale: 1,
    warning: { status: false, warningMessage: "" }
  }

  const globals = JSON.parse(localStorage.getItem('globals')) || {
    actors: { "000000": { id: "000000", name: "Narrator", playable: false, color: "FFFFFF", relationship: "0" } },
    actorVariables: {},
    variables: {},
    factions: {},
  }

  const state = { ...initialState, ...globals}
  if (process.env.NODE_ENV === "development") {
    return createStore(
      reducers,
      state,
      composeWithDevTools(applyMiddleware(logger))
    )
  }

  return createStore(reducers, state)
}
export default store
