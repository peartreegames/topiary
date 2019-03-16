import React, { Component } from "react"
import { connect } from "react-redux"
import PropTypes from "prop-types"
import {
  Table,
  TableBody,
  TableHead,
  TableRow,
  TableCell,
  Button,
  Popover,
  TextField,
  Typography
} from "@material-ui/core"
import { Delete } from '@material-ui/icons'

import { newVariable, deleteVariable } from "../../store/actions"

const styles = {
  textStyle: {
    fontFamily: "Roboto Mono",
    fontSize: 12
  },
  tabContent: {
    margin: "20px",
    paddingTop: "20px"
  },
  popover: {
    margin: "20px",
    width: "25vw"
  },
  button: {
    width: 14,
    height: 14,
    padding: 0,
    margin: 0
  },
  icon: {
    fontSize: 14
  }
}

class VariableTab extends Component {
  static propTypes = {
    newVariable: PropTypes.func.isRequired,
    deleteVariable: PropTypes.func.isRequired,
    variables: PropTypes.object
  }

  static defaultProps = {
    variables: {}
  }
  state = {
    open: false,
    newVariable: "",
    newvalue: ""
  }

  handleClick = event => {
    event.preventDefault()
    this.setState({
      open: true,
      anchorEl: event.currentTarget
    })
  }

  handleNewVariable = event => {
    this.props.newVariable({
      [this.state.newVariable]: this.state.newValue
    })
    this.setState({ open: false, newVariable: "", newValue: "" })
  }

  handleKeyUpdate = event => {
    this.setState({ newVariable: event.target.value })
  }
  handleValueUpdate = event => {
    this.setState({ newValue: event.target.value })
  }

  handleRequestClose = () => {
    this.setState({
      open: false,
      newVariable: "",
      newDefaut: ""
    })
  }

  render() {
    const { newVariable, newValue, open, anchorEl } = this.state
    const { variables } = this.props
    return (
      <div style={styles.tabContent}>
        <Button color="primary" onClick={this.handleClick}>new</Button>

        <Popover
          open={open}
          anchorEl={anchorEl}
          anchorOrigin={{ horizontal: "left", vertical: "bottom" }}
          targetOrigin={{ horizontal: "left", vertical: "top" }}
          onRequestClose={this.handleRequestClose}
        >
          <div style={styles.popover}>
            <TextField
              label="key"
              fullWidth
              textareastyle={styles.textStyle}
              value={newVariable}
              onChange={e => this.handleKeyUpdate(e)}
            />
            <TextField
              label="value"
              fullWidth
              textareastyle={styles.textStyle}
              value={newValue}
              helperText={<Typography>the default value assigned</Typography>}
              onChange={e => this.handleValueUpdate(e)}
            />
            <Button
              color="primary"
              onClick={this.handleNewVariable}
            >Submit</Button>
            <Button
              color="primary"
              onClick={() => this.setState({ open: false })}
            >Cancel</Button>
          </div>
        </Popover>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>Variable</TableCell>
              <TableCell>Value</TableCell>
              <TableCell>Options</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {Object.entries(variables).map(([key, value], i) => (
              <TableRow key={`${key}-${i}`}>
                <TableCell>{key}</TableCell>
                <TableCell>{value}</TableCell>
                <TableCell>
                  <Button
                    style={styles.button}
                    onClick={() => this.props.deleteVariable({key})}
                  >
                    <Delete style={styles.icon}/>
                  </Button>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </div>
    )
  }
}

const mapStateToProps = ({variables}) => {
  return {
    variables
  }
}

export default connect(mapStateToProps, { newVariable, deleteVariable })(VariableTab)
