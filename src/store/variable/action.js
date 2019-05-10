export const newVariable = (variable) => ({
  type: "NEW_VARIABLE",
  payload: variable
})

export const deleteVariables = ({
  keys
}) => ({
  type: "DELETE_VARIABLES",
  payload: keys
})
