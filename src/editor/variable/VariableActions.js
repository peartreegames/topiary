export const newVariable = (variable) => ({
  type: "NEW_VARIABLE",
  variable
})

export const deleteVariable = ({ key }) => ({
  type: "DELETE_VARIABLE",
  key
})
