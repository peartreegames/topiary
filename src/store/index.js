import { createStore, applyMiddleware } from "redux"
import { logger } from "redux-logger"
import { composeWithDevTools } from "remote-redux-devtools"
import reducers from "./reducers"
import { defaultScene, globals as initialGlobals } from "./initialScene";

const store = id => {
  const initialState = JSON.parse(localStorage.getItem(id)) || defaultScene();

  const globals = JSON.parse(localStorage.getItem('globals')) || initialGlobals;

  const state = { ...initialState, ...globals }
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
