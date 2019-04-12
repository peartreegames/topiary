import React, { Component } from "react"
import { connect } from "react-redux"
import PropTypes from "prop-types"
import {
  Fab,
  IconButton,
  TextField
} from "@material-ui/core"
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
    searchVisible: false,
    searchText: ""
  }

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

  render() {
    const { search } = this.props
    const hideEditor = {
      transform: !this.props.editor ? "translateX(28vw)" : "translateX(0)"
    }
    return (
      <div style={{ ...styles.buttonContainer, ...hideEditor }}>
        <Fab
          size="small"
          style={styles.button}
          onClick={() => this.handleNewNode("root")}
        >
          <RadioButtonChecked />
        </Fab>

        <Fab
          size="small"
          style={styles.button}
          onClick={() => this.handleNewNode("dialogue")}
        >
          <Chat />
        </Fab>

        <Fab
          size="small"
          onClick={() => this.handleNewNode("choice")}
          style={styles.button}
        >
          <QuestionAnswer />
        </Fab>
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
          <IconButton
            onClick={this.handleSearchButton}
            // Fix styling
            data-tip={"Search"}
            data-tippos={"left"}
          >
            <Search />
          </IconButton>
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



// WEBPACK FOOTER //
// ./src/nav/NavBottom.js
