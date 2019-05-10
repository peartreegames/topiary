const variables = (state = {}, {
  type,
  payload
}) => {
  switch (type) {
    case 'NEW_VARIABLE':
      return {
        ...state,
        ...payload
      }
    case 'DELETE_VARIABLES': 
      return Object.entries(state).reduce((acc, [key, value]) => {
        if (!payload.includes(key)) acc[key] = value
        return acc
      }, {})
    default:
      return state
  }
}

export default variables
