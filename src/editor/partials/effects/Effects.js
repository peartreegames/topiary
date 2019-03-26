import React, { Fragment } from 'react'
import PropTypes from 'prop-types'
import { Modal, Button, Paper } from "@material-ui/core"
import Effect from './Effect';

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

class Effects extends React.Component {

  state = {
    effects: [],
    open: false
  }

  baseEffect = () => JSON.parse(JSON.stringify({
    variable: '',
    operator: '',
    value: ''
  }))

  componentWillMount() {
    this.setState({
      effects: this.props.effects || [this.baseEffect()]
    })
  }

  componentDidUpdate(prevProps) {
    if (prevProps.effects !== this.props.effects) {
      this.setState({
        effects: this.props.effects || [this.baseEffect()]
      })
    }
  }

  toggleModal = () => {
    const { open } = this.state
    this.setState({ open: !open })
  }

  validateEffects = (effects) => {
    const results = effects.filter(({variable, operator, value}) => variable && operator && value)
    if (results.length === 0) return
    return results
  }

  save = () => {
    const validatedEffects = this.validateEffects(this.state.effects)
    console.log('EFFECTS', validatedEffects)
    this.setState({effects: validatedEffects})
    this.props.onSave("effects", validatedEffects)
    this.toggleModal()
  }

  addEffect = () => {
    this.setState({ effects: [...this.state.effects, this.baseEffect()] })
  }

  removeEffect = (index) => {
    if (this.state.effects.length === 1) {
      return this.setState({effects: [this.baseEffect()]})
    }

    this.setState({ effects: this.state.effects.filter((_, i) => i !== index) })
  }

  updateEffect = (index, field, value) => {
    const newEffects = [...this.state.effects]
    newEffects[index][field] = value
    this.setState({
      effects: newEffects
    })
  }

  renderEffects = () => (this.state.effects || [this.baseEffect()]).map(({ variable, operator, value }, i) =>
    <Effect
      key={`effects-${i}`}
      variable={variable}
      operator={operator}
      value={value}
      index={i}
      effectsLength={this.state.effects.length - 1}
      onAddEffect={this.addEffect}
      onRemoveEffect={this.removeEffect}
      onUpdateEffect={this.updateEffect} />)

  render() {
    const buttonStyle = {
      color: this.props.effects ? '#558b2f' : '#000000',
      fontWeight: this.props.effects ? 'bold' : 'normal'
    }

    return (
      <Fragment>
        <Button onClick={this.toggleModal} style={buttonStyle}>Effects</Button>
        <Modal open={this.state.open} onClose={this.toggleModal}>
          <Paper style={styles.modal}>
            {this.renderEffects()}
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

Effects.propTypes = {
  effects: PropTypes.array,
  onSave: PropTypes.func.isRequired
}

export default Effects
