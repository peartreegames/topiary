import React, { Component, Fragment } from "react"
import { connect } from "react-redux"
import PropTypes from "prop-types"
import { TextField, Chip, Typography, Switch } from "@material-ui/core"
import { makeGetNodeByCurrent } from "../../store/selectors"
import { updateNode, updateCondition, updateSets } from "../../store/actions"
import ActorSelect from '../partials/actor-select/ActorSelect'
import Condition from "../partials/condition/Condition"
import Sets from '../partials/sets/Sets'

const styles = {
  textStyle: {
    fontFamily: "Roboto Mono",
    fontSize: 12,
    marginTop: "20px"
  },
  tabContent: {
    margin: "20px"
  },
  tagWrapper: {
    // display: "inline-flex",
    // flexWrap: "wrap",
    width: "100%"
  }, 
  tagChip: {
    display: "inline-flex",
    flexWrap: "wrap",
    margin: "2px",
    height: "1.8em"
  }
}

class EditTab extends Component {
  static propTypes = {
    node: PropTypes.object,
    updateNode: PropTypes.func.isRequired
  }

  state = {
    node: {},
    tagsField: ""
  }

  handleTagsUpdate = e => {
    this.setState({ tagsField: this.state.tagsField + e.key })
    const { updateNode, node } = this.props
    const { tags } = node
    if (e.key === "Enter") {
      updateNode({
        id: node.id,
        payload: {
          tags: tags ? [...tags, e.target.value] : [e.target.value]
        }
      })
      this.setState({ tagsField: "" })
    }
  }

  handleDeleteTag = index => {
    const { updateNode, node } = this.props
    const { tags } = node
    updateNode({
      id: node.id,
      payload: {
        tags: tags.filter((tag, i) => i !== index)
      }
    })
  }

  handleActorUpdate = (e) => {
    const { updateNode, node } = this.props
    updateNode({
      id: node.id,
      payload: { actor: e.target.value }
    })
  }

  handleTextUpdate = (e, name) => {
    const { updateNode, node } = this.props
    updateNode({
      id: node.id,
      payload: { [name]: e.target.value }
    })
  }

  handleConditionUpdate = (e, name) => {
    const { updateCondition, node } = this.props
    updateCondition({
      id: node.id,
      payload: { [name]: e.target.value }
    })
  }

  handleSetsUpdate = (e, name) => {
    const { updateSets, node } = this.props
    updateSets({
      id: node.id,
      payload: { [name]: e.target.value }
    })
  }

  handleSwitchUpdate = (e, name) => {
    const { updateNode, node } = this.props;
    updateNode({
      id :node.id,
      payload: { [name]: e.target.checked }
    })
  }

  render() {
    const { node = {} } = this.props
    const {
      type = "",
      title = "",
      actor = "000000",
      body = "",
      tags = [],
      condition = {},
      replay = false,
      sets = {}
    } = node

    const chipTags =
      tags.map((tag, i) => (
        <Chip
          key={tag}
          label={tag}
          style={styles.tagChip}
          onDelete={() => this.handleDeleteTag(i)}
          deleteIcon={<Typography>x</Typography>}
        >
        </Chip>
      ))
      
    return (
      <div style={styles.tabContent}>
          <TextField
            fullWidth
            style={styles.textStyle}
            label={"Title"}
            value={node && title}
            onChange={e => this.handleTextUpdate(e, "title")}
          />
          <Typography>Replay <Switch
          checked={replay}
          onChange={e => this.handleSwitchUpdate(e, "replay")}
          value="Replay"
          color="primary"
        /></Typography>
        {type === "dialogue" && (
          <ActorSelect actorId={actor} onChange={this.handleActorUpdate} />)}
        <TextField
          fullWidth
          style={styles.textStyle}
          label={"Tags"}
          value={this.state.tagsField}
          onKeyPress={this.handleTagsUpdate}
        />
        <div style={styles.tagsWrapper}>{chipTags}</div>
        {type !== 'root' &&
        <Fragment>
        <Condition condition={condition} onChange={this.handleConditionUpdate} />
        {type === 'choice' && <Sets sets={sets} onChange={this.handleSetsUpdate} />}
        <TextField
          multiline
          fullWidth
          style={styles.textStyle}
          label={"Body"}
          value={body}
          onChange={e => this.handleTextUpdate(e, "body")}
        /></Fragment>}
      </div>
    )
  }
}

const makeMapState = () => {
  const getNode = makeGetNodeByCurrent()
  return ({ nodes, focusedNode }) => ({
    ...getNode({ nodes, focusedNode })
  })
}

export default connect(makeMapState, { updateNode, updateCondition, updateSets })(EditTab)
