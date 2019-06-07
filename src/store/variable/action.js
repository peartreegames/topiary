export const actionTypes = {
  variable: {
    NEW: 'NEW',
    DELETE: 'DELETE'
  }
}

export const newVariable = (variable) => ({
  type: actionTypes.variable.NEW,
  payload: variable
})

export const deleteVariables = ({
  keys
}) => ({
  type: actionTypes.variable.DELETE,
  payload: keys
})
