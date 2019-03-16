import React, { Component, Fragment } from "react"
import { connect } from "react-redux"
import { Redirect } from "react-router-dom"
import PropTypes from "prop-types"
import {
  Toolbar,
  TextField,
  Icon,
  Button,
  Typography
} from "@material-ui/core"
import { updateScene } from "../store/actions"
import { saveState } from "../store/localStorage"
import { focusedStore } from "../app/Scene"
import NavBottom from "./NavBottom"

const styles = {
  container: {
    position: "fixed",
    top: 0,
    left: 0,
    width: "30vw",
    backgroundColor: "rgba(0, 0, 0, 0)"
  },
  textStyle: {
    marginRight: "30px",
    width: "30vw",
    fontFamily: "Roboto Mono",
    margin: "normal"
  },
  title: {
    color: "#558b2f",
    marginRight: "30px"
  }
}

class Nav extends Component {
  static propTypes = {
    updateScene: PropTypes.func.isRequired,
    scene: PropTypes.string.isRequired
  }

  state = {
    redirect: ""
  }

  handleSceneUpdate = e => {
    this.props.updateScene({ scene: e.target.value })
  }

  render() {
    const { scene } = this.props
    if (this.state.redirect) {
      return <Redirect to={this.state.redirect} />
    }
    return (
      <Fragment>
        <Toolbar style={styles.container}>

            <Button
              onClick={() => {
                saveState(focusedStore.getState())
                this.setState({ redirect: "/" })
              }}
            >
              <Icon className="material-icons">home</Icon>
            </Button>
            <Typography variant="h6" color="inherit" style={styles.title}>
              Topiary
            </Typography>
            <TextField
              name="scene"
              fullWidth
              style={styles.textStyle}
              onChange={this.handleSceneUpdate}
              value={scene}
            />
        </Toolbar>
        <NavBottom />
      </Fragment>
    )
  }
}

const mapState = ({ scene }) => ({ scene })

export default connect(mapState, { updateScene })(Nav)
