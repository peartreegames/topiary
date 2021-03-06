import React, { Component } from "react"
import { connect } from "react-redux"
import PropTypes from "prop-types"
import MUIDatatable from 'mui-datatables'
import {
  Button,
  Popover,
  TextField,
  Typography,
  IconButton,
  Tooltip
} from "@material-ui/core"
import { Add } from '@material-ui/icons'

import { newVariable, deleteVariables } from "store/actions"

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

  handleNewVariable = () => {
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

  customAddButton = () => (
    <Tooltip title={"new"}>
      <IconButton onClick={this.handleClick}>
        <Add/>
      </IconButton>
    </Tooltip>)

  render() {
    const { newVariable, newValue, open, anchorEl } = this.state
    const { variables } = this.props

    const columns = [{
      name: "name",
      label: 'name',
      options: {
        filter: true,
        sort: true
      }
    },{
      name: 'value',
      label: 'default value',
      options: {
        filter: false,
        sort: false
      }
    }]
    return (
      <div style={styles.tabContent}>
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

        <MUIDatatable 
          title={"Variables"}
          data={Object.entries(variables)}
          columns={columns}
          options={{
            print: false,
            download: false,
            viewColumns: false,
            filter: false,
            pagination: false,
            responsive: 'scoll',
            elevation: 0,
            onRowsDelete: ({data}) => {
              const variableArray = Object.keys(variables)
              const variablesDeleted = data.map(({ dataIndex }) => variableArray[dataIndex])
              this.props.deleteVariables({keys: variablesDeleted})
            },
            customToolbar: this.customAddButton
          }}
        />
      </div>
    )
  }
}

const mapStateToProps = ({variables}) => {
  return {
    variables
  }
}

export default connect(mapStateToProps, { newVariable, deleteVariables })(VariableTab)
