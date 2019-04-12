import React, { Component } from "react"
import {
  BottomNavigationAction,
  BottomNavigation,
  Paper,
  Icon,
  Button
} from "@material-ui/core"
import PropTypes from "prop-types"
import { connect } from "react-redux"
import ActorTab from "./partials/actor-tab"
import EditTab from "./partials/edit-tab"
import HelpTab from "./partials/help-tab"
import VariableTab from "./partials/variable-tab"
import { toggleEditor } from "store/actions"

const menuIcon = <Icon className="material-icons">menu</Icon>
const editIcon = <Icon className="material-icons">mode_edit</Icon>
const actorIcon = <Icon className="material-icons">group</Icon>
const variableIcon = <Icon className="material-icons">vpn_key</Icon>
const helpIcon = <Icon className="material-icons">help</Icon>
const settingsIcon = <Icon className="material-icons">settings</Icon>

const styles = {
  paper: {
    position: "fixed",
    top: 0,
    right: 0,
    width: "30vw",
    height: "100vh",
    overflow: "hidden",
    transition: "transform 400ms cubic-bezier(0.445, 0.05, 0.55, 0.95) 0ms"
  },
  container: {
    position: "fixed",
    top: 0,
    right: 0,
    bottom: 70,
    width: "30vw",
    height: "100vh - 70px",
    overflowY: "auto"
  },
  textStyle: {
    fontFamily: "Roboto Mono"
  },
  tabContent: {
    margin: "20px"
  },
  tabs: {
    position: "fixed",
    width: "30vw",
    bottom: "0px",
    right: "0px",
    zIndex: 4
  },
  menuButton: {
    left: "-25px",
    padding: 0
  }
}

class Editor extends Component {
  static propTypes = {
    editor: PropTypes.bool.isRequired,
    toggleEditor: PropTypes.func.isRequired
  }
  state = {
    editorTab: 0
  }

  select = index => this.setState({ editorTab: index })

  render() {
    const { editor, toggleEditor } = this.props
    const hideEditor = {
      transform: !editor ? "translateX(28vw)" : "translateX(0)"
    }
    const tabs = [
      <EditTab key={0} />,
      <ActorTab key={1} />,
      <VariableTab key={2} />,
      <HelpTab key={3} />,
      <div key={4} style={styles.tabContent}>
        <h1>Settings...</h1>
        <p>eventually</p>
      </div>
    ]

    return (
      <Paper style={{ ...styles.paper, ...hideEditor }}>
        <div style={styles.container}>{tabs[this.state.editorTab]}</div>
        <div style={styles.tabs}>
          <BottomNavigation>
            <BottomNavigationAction
              icon={editIcon}
              onClick={() => this.select(0)}
              data-tip={"Edit"}
            />
            <BottomNavigationAction
              icon={actorIcon}
              onClick={() => this.select(1)}
              data-tip={"Actors"}
            />
            <BottomNavigationAction
              icon={variableIcon}
              onClick={() => this.select(2)}
              data-tip={"Variables"}
            />
            <BottomNavigationAction
              icon={helpIcon}
              onClick={() => this.select(3)}
              data-tip={"Help"}
            />
            <BottomNavigationAction
              icon={settingsIcon}
              onClick={() => this.select(4)}
              data-tip={"Settings"}
            />
          </BottomNavigation>
        </div>
        <Button onClick={() => toggleEditor({ editor: !editor })}>
        <Icon
          style={styles.menuButton}
        >
          {menuIcon}
        </Icon>
        </Button>
      </Paper>
    )
  }
}

const mapState = ({ editor }) => ({ editor })

export default connect(mapState, { toggleEditor })(Editor)
