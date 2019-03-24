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
    colors: ["FFFFFF", "94E495", "85B7A1", "486B8D", "554A6E", "501D47"],
    editor: true,
    scale: 1,
    warning: { status: false, warningMessage: "" }
  }

  const globals = JSON.parse(localStorage.getItem('globals')) || {
    actors: {"000000": { id: "000000", name: "Narrator", playable: false, color: "FFFFFF", relationship: "0" }},
    variables: {}
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
