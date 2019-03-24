import React from 'react'
import { connect } from 'react-redux'
import PropTypes from 'prop-types'
import { Select, MenuItem, FormControl, InputLabel } from '@material-ui/core'

const styles = {
  textStyle: {
    fontFamily: 'Roboto Mono',
    fontSize: 12,
    marginTop: '20px'
  }
}

const ActorSelect = ({ actorId, actors, onChange }) => {
  const menuItems = Object.values(actors).map(actor => (
    <MenuItem key={actor.id} value={actor.id}>
      {actor.name}
    </MenuItem>
  ))

  return (
    <FormControl fullWidth style={styles.textStyle}>
      <InputLabel shrink htmlFor="actor-select">
        Actor
      </InputLabel>
      <Select
        value={actorId}
        onChange={onChange}
        inputProps={{
          name: 'Actor',
          id: 'actor-select'
        }}
      >
        {menuItems}
      </Select>
    </FormControl>
  )
}

ActorSelect.propTypes = {
  actors: PropTypes.object,
  actorId: PropTypes.string,
  onChange: PropTypes.func
}

const mapState = ({ actors }) => ({
  actors
})

export default connect(mapState)(ActorSelect)
