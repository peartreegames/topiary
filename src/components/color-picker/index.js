import React, { Fragment } from 'react'
import PropTypes from 'prop-types'
import { Popover, Typography } from "@material-ui/core"
import { Lens } from '@material-ui/icons'
import gradstop from 'gradstop'

const colors = [['#FFFFFF', '#000000'], ['#C0E5C8', '#25322B'], ['#BEE1E3', '#3B5E62'], ['#B3AEB5', '#302135'], ['#A98078', '#3F0F06'], ['#EFEABC', '#534F31']]

const gradients = colors.map(c => gradstop({
  stops: 7,
  colorArray: c
}))

class ColorPicker extends React.Component {
  state = {
    color: '#FFF',
    open: false,
    anchorEl: null
  }

  componentWillMount() {
    this.setState({
      color: this.props.color || '#FFF'
    })
  }

  componentDidUpdate(prevProps) {
    if (prevProps.color !== this.props.color) {
      this.setState({
        color: this.props.color || '#FFF'
      })
    }
  }

  togglePopover = (e) => {
    this.setState({ anchorEl: e ? e.currentTarget : null })
  }

  save = () => {
    this.props.onSave("color", this.state.color)
    this.togglePopover()
  }

  renderSwatches = () => gradients.map((colors, i) =>
    <div key={`gradient-${i}`}>{colors.map(c => {
      const color = { color: c, margin: 3 }
      return (<Lens
        key={c}
        onClick={() => {
          this.props.onSave("color", c)
          this.togglePopover()
        }}
        style={color} />)
    })
    }</div>
  )

  render() {
    const { color, anchorEl } = this.state
    const open = Boolean(anchorEl)

    return (
      <Fragment>
        <Typography style={{ marginRight: 15 }}>Color</Typography><Lens onClick={this.togglePopover} fontSize='small' style={{ color, backgroundColor: '#EEE', borderRadius: 100 }} />
        <Popover anchorEl={anchorEl}
          anchorOrigin={{
            vertical: 'bottom',
            horizontal: 'center',
          }}
          transformOrigin={{
            vertical: 'top',
            horizontal: 'center',
          }}
          open={open}
          onClose={() => this.togglePopover(null)}
          PaperProps={{ style: { display: 'flex', width: 248, padding: 10 } }}>
          <div style={{ display: 'flex' }}>
            {this.renderSwatches()}
          </div>
        </Popover>
      </Fragment>
    )
  }
}

ColorPicker.propTypes = {
  color: PropTypes.string,
  onSave: PropTypes.func.isRequired
}

export default ColorPicker
