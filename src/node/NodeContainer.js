import React, { Component } from "react"
import { connect } from "react-redux"
import PropTypes from "prop-types"
import Node from "./Node"
import { makeGetNode, makeConnectedNodes } from "../store/selectors"
import {
  updateNode,
  setFocusedLink,
  deleteAllLinks,
  deleteNode
} from "../store/actions"

class NodeContainer extends Component {
  static propTypes = {
    id: PropTypes.string.isRequired,
    node: PropTypes.object,
    updateNode: PropTypes.func.isRequired,
    setFocusedLink: PropTypes.func.isRequired,
    deleteAllLinks: PropTypes.func.isRequired,
    deleteNode: PropTypes.func.isRequired,
    focusedLink: PropTypes.object.isRequired,
    onCollapseNode: PropTypes.func.isRequired,
    isCollapsed: PropTypes.bool.isRequired
  }
  static defaultProps = {
    node: {}
  }

  componentWillReceiveProps(nextProps) {
    if (nextProps.focusedLink.status !== this.props.focusedLink.status) {
      const { id, updateNode, node } = this.props
      if (nextProps.node.id === nextProps.focusedLink.from) {
        nextProps.node.next.forEach(n => {
          updateNode({ id: n, payload: { linkable: false } })
        })
      }
      if (node.linkable === false) {
        updateNode({ id, payload: { linkable: true } })
      }
    }
  }

  render() {
    const {
      node,
      updateNode,
      setFocusedLink,
      deleteNode,
      deleteAllLinks,
      onCollapseNode,
      isCollapsed
    } = this.props
    return (
      <Node
        {...{
          ...node,
          updateNode,
          setFocusedLink,
          deleteAllLinks,
          deleteNode,
          onCollapseNode,
          isCollapsed
        }}
      />
    )
  }
}

const makeMapState = () => {
  const getNode = makeGetNode()
  const getConnected = makeConnectedNodes()
  return (
    { nodes, actors, focusedNode,focusedLink, links, search },
    { id }
  ) => ({
    node: {
      ...getNode({ nodes, actors, focusedNode }, { id }),
      ...getConnected({ links }, { id })
    },
    focusedLink,
    search
  })
}

export default connect(makeMapState, {
  updateNode,
  deleteNode,
  setFocusedLink,
  deleteAllLinks
})(NodeContainer)
