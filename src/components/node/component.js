import React, { Component } from "react"
import PropTypes from "prop-types"
import { Card, CardContent, Chip, Collapse } from "@material-ui/core"
import NodeHeader from "./partials/node-header"
import NodeFooter from "./partials/node-footer"

const styles = {
  body: {
    overflowWrap: "break-word",
    whiteSpace: "pre-wrap",
    padding: "2px 10px 5px",
    margin: "5px 0px 0px",
    fontSize: 11,
    overflowY: "hidden",
    maxHeight: "12em"
  },
  tagWrapper: {
    display: "flex",
    flexWrap: "wrap"
  },
  tagChip: {
    height: 20,
    margin: "2px 2px 5px",
    borderRadius: 8,
    padding: "0 8px"
  },
  tag: {
    fontSize: 11,
    lineHeight: "20px",
    padding: 0
  },
  divider: {
    margin: "5px 0px"
  }
}

class Node extends Component {
  static propTypes = {
    current: PropTypes.bool.isRequired,
    id: PropTypes.string.isRequired,
    type: PropTypes.string.isRequired,
    title: PropTypes.string,
    tags: PropTypes.arrayOf(PropTypes.string),
    body: PropTypes.string,
    color: PropTypes.string,
    actor: PropTypes.string,
    replay: PropTypes.bool,
    prev: PropTypes.array,
    next: PropTypes.array,
    pos: PropTypes.array,
    condition: PropTypes.array,
    updateNode: PropTypes.func.isRequired,
    setFocusedLink: PropTypes.func.isRequired,
    deleteAllLinks: PropTypes.func.isRequired,
    deleteNode: PropTypes.func.isRequired,
    onCollapseNode: PropTypes.func.isRequired,
    isCollapsed: PropTypes.bool.isRequired
  }
  static defaultProps = {
    title: "",
    tags: [],
    actor: "",
    color: "",
    body: "",
    current: false
  }
  state = {
    expanded: true,
    widthAdjustment: 0
  }

  handleExpandChange = expanded => {
    this.setState({ expanded })
  }

  render() {
    const {
      id,
      type,
      title,
      tags = [],
      body,
      color,
      actor,
      current,
      setFocusedLink,
      deleteAllLinks,
      deleteNode,
      conditions,
      effects,
      replay = false,
      onCollapseNode,
      isCollapsed
    } = this.props

    const chipTags = tags.map(tag => (
      <Chip key={tag} style={styles.tagChip} label={tag} />
    ))
    return (
      <Card>
        <NodeHeader
          {...{
            type,
            title,
            body,
            color,
            actor,
            expanded: this.state.expanded,
            expand: this.handleExpandChange,
            conditions: !!conditions,
            effects: !!effects,
            replay
          }}
        />
        <Collapse in={this.state.expanded} unmountOnExit>
          {type !== 'root' && <CardContent style={styles.body}>
            {tags && <div style={styles.tagWrapper}>{chipTags}</div>}
            {body}
          </CardContent>}
          <NodeFooter
            {...{
              id,
              current,
              expanded: this.state.expanded,
              isFocusedNode: this.isFocusedNode,
              setFocusedLink,
              deleteAllLinks,
              deleteNode,
              isCollapsed,
              collapse: () => onCollapseNode(id)
            }}
          />
        </Collapse>
      </Card>
    )
  }
}

export default Node
