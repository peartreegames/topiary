import React, { Component } from "react"
import { connect } from "react-redux"
import Draggable from "react-draggable"
import PropTypes from "prop-types"
import Node from "components/node"
import {
  setFocusedNode,
  setFocusedLink,
  newLink,
  updateNode,
  updateNodes,
  addCollapsedNode,
  removeCollapsedNode,
} from "store/actions"
import { gridSize } from "utils/view"
import { makeGetNonCollapsedNodes, getAllChildNodes } from "store/selectors"

const getNonCollapsedNodes = makeGetNonCollapsedNodes()

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

  static getDerivedStateFromProps({nodes}, {nodes: prevNodes}) {
    if (nodes === prevNodes) {
      return null
    }

    return {
      nodes,
      nodePositions: Object.values(nodes).reduce((positions, {id, pos}) => ({...positions, [id]: pos}), {})
    }
  }

  state = {
    xAdjust: 0,
    yAdjust: 0,
    nonLinkables: [],
    nodePositions: {},
    shiftKey: false
  }

  get nonCollapsedNodes() {
    return getNonCollapsedNodes({nodes: this.props.nodes, collapsedNodes: this.props.collapsedNodes, links: this.props.links})
  }

  componentDidMount() {
    window.document.addEventListener('keydown', this.handleKeyPress)
    window.document.addEventListener('keyup', this.handleKeyUp)
  }

  componentWillUnmount() {
    window.document.removeEventListener('keydown', this.handleKeyPress)
    window.document.removeEventListener('keyup', this.handleKeyUp)
  }

  handleKeyPress = ({key, shiftKey}) => {
    if (key === 'Shift' || shiftKey) {
      this.setState({shiftKey: true})
    }
  }

  handleKeyUp = ({key, shiftKey}) => {
    if (key === 'Shift' || shiftKey) {
      this.setState({shiftKey: false})
    }
  }

  handleCollapseNode = (id) => {
    const { collapsedNodes = [], addCollapsedNode, removeCollapsedNode } = this.props
    if (collapsedNodes.includes(id)) {
      return removeCollapsedNode(id)
    }
    addCollapsedNode(id)
  }

  handleNodePositionAdjust = (_, data, nodeId) => {
    const { scale } = this.props
    const { shiftKey, nodePositions: currentPositions } = this.state
    let update = { }
    if (shiftKey) {
      const children = getAllChildNodes(nodeId, this.props.links)
      const nodePositions = children.reduce((positions, node) => ({...positions, [node]: [currentPositions[node][0] + data.deltaX, currentPositions[node][1] + data.deltaY ]}), {})
      update = nodePositions
    }

    update[nodeId] = [currentPositions[nodeId][0] + data.deltaX, currentPositions[nodeId][1] + data.deltaY]
    this.setState({
      xAdjust: data.deltaX / scale,
      yAdjust: data.deltaY / scale,
      nodePositions: {...currentPositions, ...update}
    })
  }

  handleNodePositionUpdate = (_, data, id) => {
    const { nodePositions, nodes } = this.state
    const newNodes = { ...nodes }
    Object.entries(nodePositions).forEach(([nodeId, pos]) => newNodes[nodeId].pos = pos);
    this.props.updateNodes({
      payload: newNodes
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
      focusedLink,
      setFocusedNode,
      setFocusedLink,
      newLink,
      focusedNode,
      collapsedNodes
    } = this.props

    const {
      nodePositions,
    } = this.state

    return Object.values(this.nonCollapsedNodes).map(n => {
      return (
        <Draggable
          key={n.id}
          position={{
            x: Math.round(nodePositions[n.id][0] / gridSize) * gridSize,
            y: Math.round(nodePositions[n.id][1] / gridSize) * gridSize
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
          onDrag={(e, data) => this.handleNodePositionAdjust(e, data, n.id)}
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
            <Node id={n.id} onCollapseNode={this.handleCollapseNode} isCollapsed={collapsedNodes.includes(n.id)}/>
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
  return ({ scale, nodes, focusedLink, focusedNode, search, collapsedNodes, links }) => ({
    nodes,
    scale,
    focusedLink,
    focusedNode,
    search,
    collapsedNodes,
    links
  })
}


export default connect(makeMapState, {
  setFocusedNode,
  setFocusedLink,
  newLink,
  updateNode,
  updateNodes,
  addCollapsedNode,
  removeCollapsedNode
})(NodeList)
