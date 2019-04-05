export const actors = (state = {}, {
  type,
  id,
  actor
}) => {
  switch (type) {
    case "NEW_ACTOR":
      return {
        ...state,
        [id]: actor
      }
    case "UPDATE_ACTOR":
      return {
        ...state,
        [id]: {
          ...state[id],
          ...actor
        }
      }
    case "DELETE_ACTOR":
      // eslint-disable-next-line
      return (({
        [id]: _,
        ...newObj
      }, id) => newObj)(state, id)
    default:
      return state
  }
}

export const colors = (state = [], {
  type,
  color,
  id
}) => {
  switch (type) {
    case "NEW_COLOR":
      return [...state, color]
    case "DELETE_KEY":
      return state.filter((_, i) => i !== id)
    default:
      return state
  }
}
