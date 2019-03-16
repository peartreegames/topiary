import React, { Component } from "react"
import { connect } from "react-redux"
import PropTypes from "prop-types"
import {
  Select,
  TextField,
  MenuItem,
  Button,
  InputLabel,
  FormControl,
  Typography,
  Switch
} from "@material-ui/core"
import {
  newActor,
  updateActor,
  deleteActor,
} from "../../store/actions"
import { rnd } from "../../lib/math";
import ActorSelect from "../partials/actor-select/ActorSelect";

const styles = {
  textStyle: {
    fontFamily: "Roboto Mono",
    fontSize: 12,
    marginTop: "20px"
  },
  tabContent: {
    margin: "20px",
    paddingTop: "20px"
  },
  root: {
    display: "flex",
    flexWrap: "wrap",
    justifyContent: "space-around"
  }
}

class ActorTab extends Component {
  static propTypes = {
    actors: PropTypes.object,
    colors: PropTypes.arrayOf(PropTypes.string),
    newActor: PropTypes.func.isRequired,
    updateActor: PropTypes.func.isRequired,
    deleteActor: PropTypes.func.isRequired,
  }
  static defaultProps = {
    actors: {},
    colors: []
  }
  state = {
    actorId: "000000",
    playableValue: false,
    selectedIndex: 0,
  }

  handleNewActor = () => {
    const { newActor } = this.props
    const actorId = rnd()
    newActor({ actor: { name: "new", playable: false, id: actorId } })
    this.setState({ actorId })
  }

  handleActorChange = (event) => {
    this.setState({ actorId: event.target.value })
  }

  handleActorNameUpdate = event => {
    this.props.updateActor({
      id: this.state.actorId,
      actor: { name: event.target.value }
    })
  }

  handleActorColorUpdate = (event) => {
    this.props.updateActor({
      id: this.state.actorId,
      actor: { color: event.target.value }
    })
  }

  handleActorPlayableUpdate = (event) => {
    this.props.updateActor({
      id: this.state.actorId,
      actor: { playable: event.target.checked }
    })
  }

  handleActorDescriptionUpdate = event => {
    this.props.updateActor({
      id: this.state.actorId,
      actor: { description: event.target.value }
    })
  }

  handleTextUpdate = (event, name) => {
  const { actorId } = this.state;
  const { updateActor } = this.props
    updateActor({
      id: actorId,
      actor: { [name]: event.target.value }
    })
  }

  render() {
    const { actors, deleteActor, colors } = this.props
    const { actorId } = this.state
    const currentActor = actors[actorId];
    if (!currentActor) return null;
    const color = (actors[actorId] && actors[actorId].color) || "FFFFFF"

    return (
      <div style={styles.tabContent}>
        <Button color="primary" onClick={this.handleNewActor}>New</Button>
        <Button
          color="primary"
          onClick={() => deleteActor(actorId)}
        >Delete</Button>
        <ActorSelect actorId={actorId} onChange={this.handleActorChange} />
        <TextField
          label="Name"
          fullWidth
          style={styles.textStyle}
          value={currentActor.name}
          onChange={e => this.handleTextUpdate(e, "name")}
        />
        <FormControl fullWidth style={styles.textStyle}>
          <InputLabel shrink htmlFor="color-select">
            Color
          </InputLabel>
          <Select
            name="Color"
            label={"Color"}
            value={currentActor.color || "FFFFFF"}
            style={{
              backgroundColor: `#${color}`
            }}
            onChange={this.handleActorColorUpdate}
            inputProps={{
              name: 'Color',
              id: 'color-select',
            }}
          >
            {colors.map(color => (
              <MenuItem
                key={color}
                value={color}
                title={color}
                style={{ backgroundColor: `#${color}` }}
              >{"   "}</MenuItem>
            ))}
          </Select>
        </FormControl>
        <Typography>Playable <Switch
          checked={currentActor.playable || false}
          onChange={this.handleActorPlayableUpdate}
          value="Playable"
          color="primary"
        /></Typography>
        <TextField
          label="Relationship"
          value={currentActor.relationship}
          style={styles.textStyle}
          onChange={e => this.handleTextUpdate(e, "relationship")}
        />

        <TextField
          label="Description"
          multiline
          fullWidth
          style={styles.textStyle}
          value={currentActor.description}
          onChange={e => this.handleTextUpdate(e, "description")}
        />
      </div>
    )
  }
}

const mapState = ({ actors, colors }) => ({
  actors,
  colors
})

export default connect(mapState, {
  newActor,
  updateActor,
  deleteActor
})(ActorTab)
