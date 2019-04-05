import React from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'
import {
  Button,
  Select,
  MenuItem,
  TextField,
  OutlinedInput,
  InputLabel,
  FormControl
} from '@material-ui/core'
import { Delete, AddCircleOutline } from '@material-ui/icons'

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

const Conditional = ({
  actors = {},
  variables = {},
  variable = '',
  operator = '',
  value = '',
  combinator = '',
  conditionalIndex,
  groupIndex,
  onAddConditional,
  onUpdateConditional,
  onRemoveConditional
}) => {
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

  const operators = [
    { value: 'null', label: 'Is Null' },
    { value: 'notNull', label: 'Is Not Null' },
    { value: '==', label: 'Equals' },
    { value: '!=', label: 'Not Equals' },
    { value: '<', label: 'Less Than' },
    { value: '>', label: 'Greater Than' },
    { value: '<=', label: 'Less Than or Equal to' },
    { value: '>=', label: 'Greater than or Equal to' }
  ].map(({ value, label }) => (
    <MenuItem key={value} value={value}>
      {label}
    </MenuItem>
  ))

  const combinators = [
    { value: 'AND', label: 'AND' },
    { value: 'OR', label: 'OR' }
  ].map(({ value, label }) => (
    <MenuItem key={value} value={value}>
      {label}
    </MenuItem>
  ))

  return (
    <div style={styles.container}>
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
            onUpdateConditional(
              groupIndex,
              conditionalIndex,
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
        <InputLabel htmlFor="operator-select">Op</InputLabel>
        <Select
          value={operator}
          name="op"
          autoWidth
          onChange={e =>
            onUpdateConditional(
              groupIndex,
              conditionalIndex,
              'operator',
              e.target.value
            )
          }
          input={
            <OutlinedInput
              name="Op"
              id="operator-select"
              labelWidth={30}
            />
          }
        >
          {operators}
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
          onUpdateConditional(
            groupIndex,
            conditionalIndex,
            'value',
            e.target.value
          )
        }
      />
      {combinator ? (
      <FormControl
      variant="outlined"
      style={{
        ...styles.select,
        flexGrow: 1
      }}
    >
      <InputLabel htmlFor="combinator-select">and/or</InputLabel>
        <Select
          value={combinator}
          onChange={e =>
            onUpdateConditional(
              groupIndex,
              conditionalIndex,
              'combinator',
              e.target.value
            )
          }
          input={
            <OutlinedInput
              name="Combinator"
              id="combinator-select"
              labelWidth={45}
            />
          }
        >
          {combinators}
        </Select>
        </FormControl>
      ) : (
        <Button>
          <AddCircleOutline onClick={() => onAddConditional(groupIndex)} />
        </Button>
      )}
      <Button>
        <Delete
          onClick={() => onRemoveConditional(groupIndex, conditionalIndex)}
        />
      </Button>
    </div>
  )
}

Conditional.propTypes = {
  actors: PropTypes.object,
  variables: PropTypes.object,
  variable: PropTypes.string,
  operator: PropTypes.string,
  value: PropTypes.string,
  combinator: PropTypes.string,
  conditionalIndex: PropTypes.number,
  groupIndex: PropTypes.number,
  onAddConditional: PropTypes.func.isRequired,
  onUpdateConditional: PropTypes.func.isRequired,
  onRemoveConditional: PropTypes.func.isRequired
}

const mapState = ({ actors, variables }) => ({
  actors,
  variables
})

export default connect(mapState)(Conditional)
