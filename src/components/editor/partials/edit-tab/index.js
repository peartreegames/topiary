import React, { Component } from "react"
import { connect } from "react-redux"
import PropTypes from "prop-types"
import { TextField, Chip, Typography, Switch, IconButton } from "@material-ui/core"
import { Star } from '@material-ui/icons'
import Condition from "components/condition"
import { updateNode, updateDefaultActor } from "store/actions"
import { makeGetNodeByCurrent } from "store/selectors"
import ActorSelect from "../actor-select"
import Effects from "components/effects"

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
    defaultActor: PropTypes.string,
    updateNode: PropTypes.func.isRequired,
    updateDefaultActor: PropTypes.func.isRequired
  }

  state = {
    node: {},
    tagsField: ""
  }

  bodyInput = React.createRef()
  

  componentDidUpdate(prevProps) {
    const { node: prevNode = {} } = prevProps
    const { node = {} } = this.props
    if (node.id && prevNode.id !== node.id) {
      console.log(this.bodyInput)
      setTimeout(() => this.bodyInput.current.focus(), 100)
    }
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

  handleNodeUpdate = (field, value) => {
    const { updateNode, node } = this.props
    updateNode({
      id: node.id,
      payload: { [field]: value }
    })
  }

  render() {
    const { node = {}, updateDefaultActor, defaultActor } = this.props
    const {
      type = "",
      title = "",
      actor = "000000",
      body = "",
      tags = [],
      conditions,
      replay = false,
      effects
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

    const defaultActorStyle = {
      color: defaultActor === actor ? '#558b2f' : '#AAAAAA'
    }

    return (
      <div style={styles.tabContent}>

        <TextField
          fullWidth
          style={styles.textStyle}
          label={"Title"}
          value={node && title}
          onChange={e => this.handleNodeUpdate("title", e.target.value)}
        />

        <div style={{ display: 'flex' }}>
          <Typography>REPLAY<Switch
            checked={replay}
            onChange={e => this.handleNodeUpdate("replay", e.target.checked)}
            value="Replay"
            color="primary"
          /></Typography>
          <Condition conditions={conditions} onSave={this.handleNodeUpdate} />
          {type === 'choice' && <Effects effects={effects} onSave={this.handleNodeUpdate} />}
        </div>

        {type === "dialogue" && (
          <div style={{display: 'flex', alignItems: 'baseline'}}>
          <ActorSelect actorId={actor} onChange={(e) => this.handleNodeUpdate("actor", e.target.value)} /><IconButton onClick={() => updateDefaultActor(actor)}><Star fontSize="small" style={defaultActorStyle}/></IconButton>
          </div>)}

        <TextField
          fullWidth
          style={styles.textStyle}
          label={"Tags"}
          value={this.state.tagsField}
          onKeyPress={this.handleTagsUpdate}
        />
        <div style={styles.tagsWrapper}>{chipTags}</div>
        {type !== 'root' &&
          <TextField
            multiline
            fullWidth
            style={styles.textStyle}
            label={"Body"}
            value={body}
            onChange={e => this.handleNodeUpdate("body", e.target.value)}
            inputRef={this.bodyInput}
          />}
      </div>
    )
  }
}

const makeMapState = () => {
  const getNode = makeGetNodeByCurrent()
  return ({ nodes, focusedNode, defaultActor }) => ({
    ...getNode({ nodes, focusedNode }),
    defaultActor
  })
}

export default connect(makeMapState, { updateNode, updateDefaultActor })(EditTab)
