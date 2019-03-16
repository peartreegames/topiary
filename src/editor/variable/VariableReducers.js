const variables = (state = {}, { type, variable, key }) => {
  switch (type) {
    case "NEW_VARIABLE":
      return {...state, ...variable}
    case "DELETE_VARIABLE":
      return (({[key]: _, ...newObj}, key) => newObj)(state, key)
    default:
      return state
  }
}

export default variables
