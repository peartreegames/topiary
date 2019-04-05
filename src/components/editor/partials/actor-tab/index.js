import React, { Component } from "react"
import { connect } from "react-redux"
import PropTypes from "prop-types"
import {
  TextField,
  Button,
  Typography,
  Switch,
} from "@material-ui/core"
import {
  newActor,
  updateActor,
  deleteActor,
} from "store/actions"
import { rnd } from "utils/math";
import ActorSelect from "../actor-select";
import ColorPicker from "components/color-picker";

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
    open: false
  }

  toggleModal = () => {
    this.setState({open: !this.state.open})
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

  handleActorUpdate = (field, value) => {
    this.props.updateActor({
      id: this.state.actorId,
      actor: { [field]: value }
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
          onChange={e => this.handleActorUpdate("name", e.target.value)}
        />
        <div style={{display: 'flex', alignItems: 'center'}}>
        <Typography>Playable </Typography><Switch
          checked={currentActor.playable || false}
          onChange={e => this.handleActorUpdate('playable', e.target.checked)}
          value="Playable"
          color="primary"
        />
        <ColorPicker color={color} colors={colors} onSave={this.handleActorUpdate} />
        </div>
        <TextField
          label="Relationship"
          value={currentActor.relationship}
          style={styles.textStyle}
          onChange={e => this.handleActorUpdate('relationship', e.target.value)}
        />

        <TextField
          label="Description"
          multiline
          fullWidth
          style={styles.textStyle}
          value={currentActor.description}
          onChange={e => this.handleActorUpdate('description', e.target.value)}
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
