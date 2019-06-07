import { actionTypes } from './action'

export const focusedLink = (state = {}, {
  type,
  status,
  from,
  to
}) => {
  if (type === actionTypes.link.SET_CURRENT) {
    return {
      ...state,
      status,
      from,
      to
    }
  }
  return state
}

export const links = (state = {}, {
  type,
  from,
  to,
  id
}) => {
  switch (type) {
    case actionTypes.link.NEW:
      if (Array.isArray(state[from])) {
        return {
          ...state,
          [from]: [...state[from], to]
        }
      }
      return {
        ...state,
        [from]: [to]
      }
    case actionTypes.link.DELETE:
      if (state[from].length > 1) {
        return {
          ...state,
          [from]: state[from].filter(toId => toId !== to)
        }
      }
      // eslint-disable-next-line
      return (({
        [from]: _,
        ...newObj
      }, from) => newObj)(state, from)
    case actionTypes.link.DELETE_ALL:
      return Object.entries(state).reduce((acc, [from, tos]) => {
        if (from === id) {
          return acc
        }
        const linksLeft = tos.filter(to => to !== id)
        if (linksLeft.length === 0) {
          return acc
        }
        acc[from] = linksLeft
        return acc
      }, {})
    default:
      return state
  }
}
