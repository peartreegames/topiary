import React, { Component } from "react"
import { connect } from "react-redux"
import PropTypes from "prop-types"
import {
  Fab,
  IconButton,
  TextField,
  Tooltip
} from "@material-ui/core"
import {
  SpeedDial,
  SpeedDialAction,
  SpeedDialIcon
} from '@material-ui/lab';
import { rnd } from "utils/math"
import { gridSize } from "utils/view"
import {
  toggleEditor,
  setFocusedNode,
  newNode,
  updateSearch
} from "store/actions"
import { Chat, QuestionAnswer, RadioButtonChecked, Search } from "@material-ui/icons"

const styles = {
  icon: {
    width: 30,
    height: 30,
    stroke: "green",
    color: "green"
  },
  buttonContainer: {
    position: "fixed",
    left: "calc(70vw - 70px)",
    top: "calc(100vh - 200px)",
    transition: "transform 400ms cubic-bezier(0.445, 0.05, 0.55, 0.95) 0ms",
    display: "flex",
    flexDirection: "column",
    marginLeft: "auto",
  },
  button: {
    margin: "5px",
    boxShadow: "none",
    marginLeft: "auto"
  },
  searchContainer: {
    display: "inline"
  },
  searchField: {
    position: "fixed",
    left: "-10vw",
    width: "10vw"
  },
  textStyle: {
    textAlign: "right"
  }
}

class NavControls extends Component {
  static propTypes = {
    toggleEditor: PropTypes.func.isRequired,
    setFocusedNode: PropTypes.func.isRequired,
    updateSearch: PropTypes.func.isRequired,
    newNode: PropTypes.func.isRequired,
    editor: PropTypes.bool.isRequired,
    scale: PropTypes.number.isRequired,
    search: PropTypes.object.isRequired
  }

  state = {
    redirect: "",
    speedDialOpen: false,
    searchVisible: false,
    searchText: ""
  }

  actions = [
    { icon: <RadioButtonChecked />, name: 'root', onClick: () => this.handleNewNode('root') },
    { icon: <QuestionAnswer />, name: 'choice', onClick: () => this.handleNewNode('choice')  },
  ];

  handleNewNode = type => {
    const { newNode, setFocusedNode, scale, defaultActor } = this.props
    const newId = rnd()
    const diffs =
      type === "dialogue"
        ? { actor: defaultActor, body: "new dialogue" }
        : { body: "new choice" }
    const newPos = [
      Math.round(
        (window.pageXOffset + window.innerWidth / 2 - 100) / gridSize
      ) *
        gridSize *
        scale,
      Math.round(
        (window.pageYOffset + window.innerHeight / 2 - 50) / gridSize
      ) *
        gridSize *
        scale
    ]
    newNode({
      id: newId,
      payload: {
        id: newId,
        type,
        title: "untitled",
        tags: [],
        pos: newPos,
        linkable: true,
        collapsed: false,
        ...diffs
      }
    })
    setFocusedNode({ id: newId })
  }

  handleSearchButton = () => {
    const { search, updateSearch } = this.props
    updateSearch({ search: { status: !search.status } })
  }

  handleSearchText = e => {
    this.props.updateSearch({ search: { text: e.target.value } })
  }

  handleClose = () => {
    this.setState({ speedDialOpen: false })
  }

  handleOpen = () => {
    this.setState({ speedDialOpen: true })
  }

  render() {
    const { search } = this.props
    const { speedDialOpen } = this.state
    const hideEditor = {
      transform: !this.props.editor ? "translateX(28vw)" : "translateX(0)"
    }
    return (
      <div style={{ ...styles.buttonContainer, ...hideEditor }}>
        <SpeedDial
          ariaLabel="new"
          icon={<SpeedDialIcon color="secondary" openIcon={<Chat />} />}
          onBlur={this.handleClose}
          onClick={() => this.handleNewNode("dialogue")}
          onClose={this.handleClose}
          onFocus={this.handleOpen}
          onMouseEnter={this.handleOpen}
          onMouseLeave={this.handleClose}
          open={speedDialOpen}
          style={styles.button}
        >
          {this.actions.map(action => (
            <SpeedDialAction
              key={action.name}
              icon={action.icon}
              tooltipTitle={action.name}
              onClick={action.onClick}
            />
          ))}
        </SpeedDial>

        <div style={styles.searchContainer}>
          <TextField
            name="search"
            fullWidth
            style={{
              ...styles.searchField,
              display: search.status ? "block" : "none",
              ...styles.textStyle
            }}
            onChange={this.handleSearchText}
            value={search.text}
          />
          <Tooltip title='search' placement='left'>
          <IconButton
            onClick={this.handleSearchButton}
            >
            <Search />
          </IconButton>
          </Tooltip>
        </div>
      </div>
    )
  }
}

const mapState = ({ scale, editor, search, defaultActor }) => ({
  scale,
  editor,
  search,
  defaultActor
})

export default connect(mapState, {
  toggleEditor,
  newNode,
  setFocusedNode,
  updateSearch
})(NavControls)
