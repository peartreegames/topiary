import React, { Fragment } from 'react'
import PropTypes from 'prop-types'
import { Modal, Button, Paper } from '@material-ui/core'
import ConditionalGroup from './ConditionalGroup'

const styles = {
  modal: {
    top: '25vh',
    bottom: '25vh',
    margin: '50px auto',
    minWidth: '25vw',
    maxWidth: '50vw',
    minHeight: '20vh',
    overflow: 'auto',
    padding: 15
  }
}

class Condition extends React.Component {
  state = {
    open: false,
    conditions: undefined
  }

  baseConditional = {
    variable: '',
    operator: '',
    value: '',
    combinator: ''
  }

  baseGroup = () => JSON.parse(JSON.stringify({
    conditionals: [{ ...this.baseConditional }],
    combinator: null
  }))

  componentWillMount() {
    this.setState({
      conditions: this.props.conditions || [this.baseGroup()]
    })
  }

  componentDidUpdate(prevProps) {
    if (prevProps.conditions !== this.props.conditions) {
      this.setState({
        conditions: this.props.conditions || [this.baseGroup()]
      })
    }
  }

  toggleModal = () => {
    const { open } = this.state
    this.setState({ open: !open })
  }

  validateConditions = (conditions) => {
    const resultGroup = []
    conditions.forEach((conditionalGroup) => {
      const { conditionals } = conditionalGroup
      const resultConditionals = []
      conditionals.forEach((condition) => {
        const { variable, operator, value } = condition
        if (variable && operator && value) {
          resultConditionals.push(condition)
        }
      })
      if (resultConditionals.length > 0) {
        resultGroup.push({ conditionals: resultConditionals, combinator: conditionalGroup.combinator })
      }
    })
    if (resultGroup.length === 0) {
      return
    }
    return resultGroup
  }

  save = () => {
    const { conditions } = this.state
    this.props.onSave("conditions", this.validateConditions(conditions))
    this.toggleModal()
  }

  addGroup = () => {
    const newCondition = [...this.state.conditions]
    const groupLength = newCondition.length
    newCondition[groupLength - 1].combinator = 'OR'
    newCondition.push(this.baseGroup())
    this.setState({
      condition: newCondition
    })
  }

  updateGroup = (groupIndex, combinator) => {
    const { conditions } = this.state
    const newCondition = [...conditions]
    newCondition[groupIndex].combinator = combinator
    this.setState({
      condition: newCondition
    })
  }

  removeGroup = groupIndex => {
    const newCondition = [...this.state.conditions].filter((_, i) => i !== groupIndex)
    if (newCondition.length === 0) {
      return this.setState({ condition: [this.baseGroup()] })
    }
    const groupLength = newCondition.length
    newCondition[groupLength - 1].combinator = ''
    this.setState({
      condition: newCondition
    })
  }

  addConditional = groupIndex => {
    const { conditions } = this.state
    const newCondition = [...conditions]
    const conditionalLength = newCondition[groupIndex].conditionals.length
    newCondition[groupIndex].conditionals[conditionalLength - 1].combinator = 'AND'
    newCondition[groupIndex].conditionals.push({ ...this.baseConditional })
    this.setState({
      condition: newCondition
    })
  }

  updateConditional = (groupIndex, conditionalIndex, variable, value) => {
    const { conditions } = this.state
    const newCondition = [...conditions]
    newCondition[groupIndex].conditionals[conditionalIndex][variable] = value
    this.setState({
      condition: newCondition
    })
  }

  removeConditional = (groupIndex, conditionalIndex) => {
    const { conditions } = this.state
    const newCondition = [...conditions]
    if (newCondition[groupIndex].conditionals.length === 1) {
      return this.removeGroup(groupIndex)
    }
    newCondition[groupIndex].conditionals = newCondition[
      groupIndex
    ].conditionals.filter((_, i) => i !== conditionalIndex)
    const conditionalLength = newCondition[groupIndex].conditionals.length
    newCondition[groupIndex].conditionals[conditionalLength - 1].combinator = ''
    this.setState({
      condition: newCondition
    })
  }

  renderGroups = () => {
    const { conditions = [this.baseGroup()] } = this.state
    return conditions.map((cond, i) => (
      <ConditionalGroup
        conditionals={cond.conditionals}
        combinator={cond.combinator}
        groupIndex={i}
        onAddGroup={this.addGroup}
        onUpdateGroup={this.updateGroup}
        onRemoveGroup={this.removeGroup}
        onAddConditional={this.addConditional}
        onUpdateConditional={this.updateConditional}
        onRemoveConditional={this.removeConditional}
        key={`conditional-group-${i}`}
      />
    ))
  }

  render() {
    const buttonStyle = {
      color: this.props.conditions ? '#558b2f' : '#000000',
      fontWeight: this.props.conditions ? 'bold' : 'normal'
    }
    return (
      <Fragment>
        <Button onClick={this.toggleModal} style={buttonStyle}>Conditions</Button>
        <Modal open={this.state.open} onClose={this.toggleModal}>
          <Paper style={styles.modal}>
            {this.renderGroups()}
            <div style={{ float: 'right' }}>
              <Button onClick={this.save}>Save</Button>
              <Button onClick={this.toggleModal}>Cancel</Button>
            </div>
          </Paper>
        </Modal>
      </Fragment>
    )
  }
}

Condition.propTypes = {
  conditions: PropTypes.array,
  onSave: PropTypes.func
}

export default Condition
