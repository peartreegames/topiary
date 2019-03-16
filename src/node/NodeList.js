import React, { Component } from "react"
import { connect } from "react-redux"
import Draggable from "react-draggable"
import PropTypes from "prop-types"
import NodeContainer from "./NodeContainer"
import {
  setFocusedNode,
  setFocusedLink,
  newLink,
  updateNode,
  addCollapsedNode,
  removeCollapsedNode,
} from "../store/actions"
import { gridSize } from "../lib/view"
import { makeGetNonCollapsedNodes, makeGetNonCollapsedLinks } from "../store/selectors"

const styles = {
  dragContainer: {
    width: "210px",
    position: "absolute"
  },
  overlay: {
    backgroundColor: "rgba(0,0,0,0.1)",
    position: "absolute",
    top: 0,
    bottom: 0,
    left: 0,
    right: 0
  }
}

class NodeList extends Component {
  static propTypes = {
    scale: PropTypes.number.isRequired,
    nodes: PropTypes.object.isRequired,
    collapsedNodes: PropTypes.array.isRequired,
    focusedLink: PropTypes.object.isRequired,
    focusedNode: PropTypes.string.isRequired,
    setFocusedNode: PropTypes.func.isRequired,
    setFocusedLink: PropTypes.func.isRequired,
    newLink: PropTypes.func.isRequired,
    updateNode: PropTypes.func.isRequired,
    search: PropTypes.object.isRequired
  }

  state = {
    xAdjust: 0,
    yAdjust: 0,
    nonLinkables: []
  }

  handleCollapseNode = (id) => {
    const { collapsedNodes = [], addCollapsedNode, removeCollapsedNode } = this.props
    if (collapsedNodes.includes(id)) {
      return removeCollapsedNode(id)
    }
    addCollapsedNode(id)
  }

  handleNodePositionAdjust(_, data) {
    const { scale } = this.props
    this.setState({
      xAdjust: data.deltaX / scale,
      yAdjust: data.deltaY / scale
    })
  }

  handleNodePositionUpdate(_, data, id) {
    this.props.updateNode({
      id,
      payload: { pos: [data.lastX, data.lastY] }
    })
    this.setState({ xAdjust: 0, yAdjust: 0 })
  }

  isFocusedNode = id => {
    return this.props.focusedNode === id
  }

  isInSearch = (body, title, tags) => {
    const { search: { text, status } } = this.props
    if (!status || !text) return {}
    const t = text.toLowerCase()
    if (
      body.toLowerCase().includes(t) ||
      title.toLowerCase().includes(t) ||
      tags
        .join(" ")
        .toLowerCase()
        .includes(t)
    ) {
      return {
        boxShadow: `0 0 20px 5px rgba(0,40,0,0.5)`,
        borderRadius: "4px"
      }
    }
    return {}
  }



  render() {
    const {
      nodes,
      focusedLink,
      setFocusedNode,
      setFocusedLink,
      newLink,
      focusedNode,
      collapsedNodes
    } = this.props
    return Object.values(nodes).map(n => {
      return (
        <Draggable
          key={n.id}
          position={{
            x: Math.round(n.pos[0] / gridSize) * gridSize,
            y: Math.round(n.pos[1] / gridSize) * gridSize
          }}
          handle=".draggable"
          grid={[gridSize, gridSize]}
          onMouseDown={() => {
            if (n.linkable) {
              if (focusedLink.status) {
                newLink({
                  from:focusedLink.from,
                  to: n.id
                })
                return setFocusedLink({ status: false })
              }
              if (focusedNode !== n.id) setFocusedNode({ id: n.id })
            }
          }}
          onDrag={(e, data) => this.handleNodePositionAdjust(e, data)}
          onStop={(e, data) => {
            if (this.state.xAdjust !== 0 || this.state.yAdjust !== 0)
              this.handleNodePositionUpdate(e, data, n.id)
          }}
        >
          <div
            id={n.id}
            style={{
              ...styles.dragContainer,
              ...this.isInSearch(n.body, n.title, n.tags),
              zIndex: this.isFocusedNode(n.id) ? 10 : 1
            }}
          >
            <NodeContainer id={n.id} onCollapseNode={this.handleCollapseNode} isCollapsed={collapsedNodes.includes(n.id)}/>
            <div
              style={{
                ...styles.overlay,
                display: n.linkable ? "none" : "block"
              }}
            />
          </div>
        </Draggable>
      )
    })
  }
}


const makeMapState = () => {
  const getNonCollapsedNodes = makeGetNonCollapsedNodes()
  const getNonCollapsedLinks = makeGetNonCollapsedLinks()
  return ({ scale, nodes, focusedLink, focusedNode, search, collapsedNodes, links }) => ({
    nodes: getNonCollapsedNodes({ nodes, collapsedNodes, links }),
    scale,
    focusedLink,
    focusedNode,
    search,
    collapsedNodes,
    links: getNonCollapsedLinks({ links, collapsedNodes })
  })
}


export default connect(makeMapState, {
  setFocusedNode,
  setFocusedLink,
  newLink,
  updateNode,
  addCollapsedNode,
  removeCollapsedNode
})(NodeList)
