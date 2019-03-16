import React, { Fragment } from "react"
import PropTypes from "prop-types"
import { Provider } from "react-redux"
import Editor from "./Editor"
import Tree from "./Tree"
import Nav from "../nav/Nav"
import store from "../store"
import { saveState, throttle } from "../store/localStorage"

export let focusedStore
export default function Scene({ match }) {
  focusedStore = store(match.params.id)
  focusedStore.subscribe(
    throttle(() => {
      saveState(focusedStore.getState())
    }, 1000)
  )

  return (
    <Provider store={focusedStore}>
      <Fragment>
        <Tree />
        <Editor />
        <Nav />
      </Fragment>
    </Provider>
  )
}

Scene.propTypes = {
  match: PropTypes.object.isRequired
}
