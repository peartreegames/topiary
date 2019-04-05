export const focusedNode = (state = "", { type, id }) => {
    if (type === "SET_CURRENT_NODE") {
      return id
    }
    return state
}

export const nodes = (state = {}, { type, id, payload }) => {
    switch (type) {
        case "NEW_NODE":
        return {
            ...state,
            [id]: payload
        }
        case "UPDATE_NODE":
        return {
            ...state,
            [id]: {
            ...state[id],
            ...payload
            }
        }
        case "UPDATES_NODES":
        return {
            ...state,
            ...payload
        }
        case "DELETE_NODE":
        return Object.keys(state).reduce((acc, key) => {
            if (key !== id) {
            return { ...acc, [key]: state[key] }
            }
            return acc
        }, {})
        default:
        return state
    }
}
