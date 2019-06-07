import { actionTypes } from './action'

export const variables = (state = {}, {
  type,
  payload
}) => {
  switch (type) {
    case actionTypes.variable.NEW:
      return {
        ...state,
        ...payload
      }
    case actionTypes.variable.DELETE:
      return Object.entries(state).reduce((acc, [key, value]) => {
        if (!payload.includes(key)) acc[key] = value
        return acc
      }, {})
    default:
      return state
  }
}
