import React from 'react'
import PropTypes from 'prop-types'
import Conditional from './Conditional'
import {
  Select,
  MenuItem,
  Button,
  FormControl,
  InputLabel,
  OutlinedInput
} from '@material-ui/core'
import { AddCircleOutline } from '@material-ui/icons'

const styles = {
  select: {
    height: '4em',
    fontSize: 11,
    margin: 'auto'
  },
  dividerContainer: {
    display: 'flex',
    margin: '10px auto',
    width: '100%',
    backgroundColor: 'rgb(240,240,240)',
    borderRadius: 10,
    alignItems: 'center'
  }
}

const ConditionalGroup = ({
  conditionals,
  combinator,
  groupIndex,
  onAddGroup,
  onUpdateGroup,
  onRemoveGroup,
  onAddConditional,
  onUpdateConditional,
  onRemoveConditional
}) => {
  const combinators = [
    { value: 'AND', label: 'AND' },
    { value: 'OR', label: 'OR' }
  ].map(({ value, label }) => (
    <MenuItem key={value} value={value}>
      {label}
    </MenuItem>
  ))

  return (
    <div>
      {conditionals.map((conditional, i) => (
        <Conditional
          variable={conditional.variable}
          operator={conditional.operator}
          value={conditional.value}
          combinator={conditional.combinator}
          conditionalIndex={i}
          groupIndex={groupIndex}
          key={`conditional-${i}`}
          onRemoveGroup={() => onRemoveGroup(groupIndex)}
          onAddConditional={onAddConditional}
          onUpdateConditional={onUpdateConditional}
          onRemoveConditional={onRemoveConditional}
        />
      ))}
      <div style={styles.dividerContainer}>
        {combinator ? (
          <FormControl
            variant="outlined"
            style={styles.select}
          >
            <InputLabel htmlFor="combinator-select">and/or</InputLabel>
            <Select
              value={combinator}
              onChange={e => onUpdateGroup(groupIndex, e.target.value)}
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
          <Button style={styles.select}>
            <AddCircleOutline onClick={onAddGroup} />
          </Button>
        )}
      </div>
    </div>
  )
}

ConditionalGroup.propTypes = {
  conditionals: PropTypes.array,
  combinator: PropTypes.string,
  groupIndex: PropTypes.number,
  onAddGroup: PropTypes.func.isRequired,
  onUpdateGroup: PropTypes.func.isRequired,
  onRemoveGroup: PropTypes.func.isRequired,
  onAddConditional: PropTypes.func.isRequired,
  onUpdateConditional: PropTypes.func.isRequired,
  onRemoveConditional: PropTypes.func.isRequired
}

export default ConditionalGroup
