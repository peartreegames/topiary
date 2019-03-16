import React, { Fragment } from 'react'
import { connect } from "react-redux"
import PropTypes from 'prop-types'
import { Select, MenuItem, InputLabel, TextField } from "@material-ui/core"

const styles = {
  container: {
    display: "flex",
    flexWrap: "none",
  },
  field: {
    margin: 10
  }
}

const Sets = ({ variables, actors, sets: { variable = '', op = '', value = '' }, onChange }) => {

  const menuItems = () => {
    const variableSuggestions = Object.keys(variables).map(variable => ({ value: variable, label: variable }))
    const actorSuggestions = Object.values(actors).map(({ id, name }) => ({ value: `Actors.${id}.relationship`, label: `${name}.relationship` }))
    return [{value: '', label: 'none'}, ...actorSuggestions, ...variableSuggestions].map((v) => (
      <MenuItem key={v.value} value={v.value}>{v.label}</MenuItem>
    ))
  }

  const opItems = () => ['','=', '+=', '-='].map(o => <MenuItem key={o} value={o}>{o}</MenuItem>)
    return (
      <Fragment>
        <InputLabel shrink htmlFor="sets">
          Sets
        </InputLabel>
        <div style={styles.container}>
        <Select
          style={styles.field}
          value={variable}
          onChange={(e) => onChange(e, "variable")}
          inputProps={{
            name: 'SetsVar',
            id: 'sets-var',
          }}
        >
          {menuItems()}
        </Select>
        <Select
          style={styles.field}
          value={op}
          onChange={(e) => onChange(e, "op")}
          inputProps={{
            name: 'SetsOp',
            id: 'sets-op',
          }}
        >
          {opItems()}
        </Select>
        <TextField
            fullWidth
            style={styles.field}
            label={"value"}
            value={value}
            onChange={e => onChange(e, "value")}
            inputProps={{
              name: 'SetsValue',
              id: 'sets-value',
            }}
          />
          </div>
      </Fragment>
    )
}

Sets.propTypes = {
  actors: PropTypes.object,
  variables: PropTypes.object,
  sets: PropTypes.object,
  onChange: PropTypes.func
}

Sets.defaultProps = {
  sets: {
    variable: '',
    op: '',
    value: ''
  }
}

const mapState = ({ actors, variables }) => ({
  actors,
  variables,
})

export default connect(mapState)(Sets)