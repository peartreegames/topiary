import React from 'react'
import PropTypes from 'prop-types'
import { connect } from "react-redux"
import { FormControl, InputLabel, Select, TextField, OutlinedInput, MenuItem, Button } from '@material-ui/core'
import { AddCircleOutline, Delete } from '@material-ui/icons'

const styles = {
  select: {
    margin: 10,
    height: '4em'
  },
  container: {
    display: 'flex',
    margin: '10px auto'
  }
}

const Effect = ({ variable, operator, value, index, effectsLength, variables, actors, onUpdateEffect, onAddEffect, onRemoveEffect }) => {

  const renderVariables = () => {
    const variableFields = Object.keys(variables).map(variable => ({
      value: variable,
      label: variable
    }))
    const actorFields = Object.values(actors).map(({ id, name }) => ({
      value: `Actors.${id}.relationship`,
      label: `${name}.relationship`
    }))
    return [...actorFields, ...variableFields].map(({ value, label }) => (
      <MenuItem key={value} value={value}>
        {label}
      </MenuItem>
    ))
  }

  const opItems = () => ['=', '+=', '-='].map(o => <MenuItem key={o} value={o}>{o}</MenuItem>)

  return (<div style={styles.container}>
    <FormControl
      variant="outlined"
      style={{
        ...styles.select,
        flexGrow: 3
      }}
    >
      <InputLabel htmlFor="variable-select">Variable</InputLabel>
      <Select
        value={variable}
        name="variable"
        onChange={e =>
          onUpdateEffect(
            index,
            'variable',
            e.target.value
          )
        }
        input={
          <OutlinedInput
            name="Variable"
            id="variable-select"
            labelWidth={50}
          />
        }
      >
        {renderVariables()}
      </Select>
    </FormControl>
    <FormControl
      variant="outlined"
      style={{
        ...styles.select,
        flexGrow: 1
      }}
    >
      <InputLabel htmlFor="effects-operator-select">Op</InputLabel>
      <Select
        value={operator}
        name="operator"
        autoWidth
        onChange={e =>
          onUpdateEffect(
            index,
            'operator',
            e.target.value
          )
        }
        input={
          <OutlinedInput
            name="operator"
            id="effects-operator-select"
            labelWidth={30}
          />
        }
      >
        {opItems()}
      </Select>
    </FormControl>
    <TextField
      label="Value"
      style={{
        ...styles.select,
        flexGrow: 1
      }}
      value={value}
      name="value"
      variant="outlined"
      onChange={e =>
        onUpdateEffect(
          index,
          'value',
          e.target.value
        )
      }
    />
    {effectsLength === index && (
      <Button>
        <AddCircleOutline onClick={onAddEffect} />
      </Button>)}
    <Button>
      <Delete
        onClick={() => onRemoveEffect(index)}
      />
    </Button>
  </div>)
}

Effect.propTypes = {
  variable: PropTypes.string,
  operator: PropTypes.string,
  value: PropTypes.string,
  index: PropTypes.number,
  effectsLength: PropTypes.number,
  onAddEffect: PropTypes.func.isRequired,
  onRemoveEffect: PropTypes.func.isRequired,
  onUpdateEffect: PropTypes.func.isRequired
}

const mapState = ({ actors, variables }) => ({
  actors,
  variables,
})

export default connect(mapState)(Effect)