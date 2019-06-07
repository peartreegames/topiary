import React, { Component, Fragment } from "react"
import { connect } from "react-redux"
import PropTypes from "prop-types"
import Link from "./component"
import { setFocusedLink, deleteLink, setFocusedNode } from "store/actions"
import { layers } from "utils/view"
import { getFocusedNode, getFocusedLink, getFlattenedLinks, getNodeColors } from "store/selectors"

const styles = {
  linkCreator: {
    position: "absolute",
    top: 0,
    left: 0,
    pointerEvents: "none",
    zIndex: layers.CURRENTARROW
  }
}

class LinkContainer extends Component {
  static propTypes = {
    links: PropTypes.array.isRequired,
    focusedLink: PropTypes.object.isRequired,
    focusedNode: PropTypes.string.isRequired,
    mouseEvent: PropTypes.object.isRequired,
    deleteLink: PropTypes.func.isRequired,
    setFocusedLink: PropTypes.func.isRequired,
    setFocusedNode: PropTypes.func.isRequired
  }

  state = {
    mounted: false
  }
  componentDidMount() {
    this.setState({ mounted: true })
  }

  isFocusedNode = from => {
    return this.props.focusedNode === from
  }

  createArrowHead = (color) => (
    <marker id={`arrow-${btoa(color)}`} markerWidth="6" viewBox="0 0 10 10" markerHeight="6" refX="9" refY="5" orient="auto" markerUnits="strokeWidth">
  <path d="M 0 0 L 10 5 L 0 10 z" fill={color === 'rgb(255, 255, 255)' ? 'black' : color} />
  </marker>)

  render() {
    const {
      links,
      focusedLink,
      mouseEvent,
      setFocusedLink,
      setFocusedNode,
      deleteLink,
      nodeColors
    } = this.props
    const { mounted } = this.state
    const markers = Array.from(new Set(Object.values(nodeColors))).reduce((acc, color) => ({ ...acc, [color]: this.createArrowHead(color)}), {})
    const linkList = links.map(link => (
      <Link
        key={`${link[0]}-${link[1]}`}
        from={link[0]}
        to={link[1]}
        focusedNode={this.isFocusedNode(link[0])}
        color={(nodeColors[link[0]] || 'black')}
        setFocusedLink={setFocusedLink}
        setFocusedNode={setFocusedNode}
        deleteLink={deleteLink}
      />
    ))

    return (
      <Fragment>
        <svg xmlns="http://www.w3.org/2000/svg" width="100%" height="100%">
          <defs>
            {Object.values(markers)}
          </defs>
          {mounted && linkList}
        </svg>
        {focusedLink.status && (
          <svg
            xmlns="http://www.w3.org/2000/svg"
            width="100%"
            height="100%"
            style={styles.linkCreator}
          >
            <Link
              from={focusedLink.from}
              linking={focusedLink.status}
              color={(nodeColors[focusedLink.from] || 'black')}
              mouse={mouseEvent}
              focusedNode={true}
              setFocusedLink={setFocusedLink}
              setFocusedNode={setFocusedNode}
              deleteLink={deleteLink}
              style={styles.linkCreator}
            />
          </svg>
        )}
      </Fragment>
    )
  }
}

const makeMapState = () => {
  return (state) => ({
    links: getFlattenedLinks(state),
    focusedNode: getFocusedNode(state),
    focusedLink: getFocusedLink(state),
    nodeColors: getNodeColors(state)
  })
}

export default connect(makeMapState, {
  setFocusedLink,
  setFocusedNode,
  deleteLink
})(LinkContainer)
