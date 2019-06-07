import { actionTypes } from './action'

export const focusedNode = (state = "", { type, id }) => {
    if (type === actionTypes.node.SET_CURRENT) {
      return id
    }
    return state
}

export const nodes = (state = {}, { type, id, payload }) => {
    switch (type) {
        case actionTypes.node.NEW:
        return {
            ...state,
            [id]: payload
        }
        case actionTypes.node.UPDATE:
        return {
            ...state,
            [id]: {
            ...state[id],
            ...payload
            }
        }
        case actionTypes.node.UPDATE_MANY:
        return {
            ...state,
            ...payload
        }
        case actionTypes.node.DELETE:
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

export const collapsedNodes = (state = [], { type, nodeId }) => {
    switch (type) {
      case actionTypes.node.ADD_COLLAPSED:
        return [...state, nodeId]
      case actionTypes.node.REMOVE_COLLAPSED:
        return state.filter(id => id !== nodeId)
      default:
        return state
    }
  }
