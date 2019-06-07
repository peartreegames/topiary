import { actionTypes } from './action'

export const actors = (state = {}, {
  type,
  id,
  actor
}) => {
  switch (type) {
    case actionTypes.actor.NEW:
      return {
        ...state,
        [id]: actor
      }
    case actionTypes.actor.UPDATE:
      return {
        ...state,
        [id]: {
          ...state[id],
          ...actor
        }
      }
    case actionTypes.actor.DELETE:
      // eslint-disable-next-line
      return (({
        [id]: _,
        ...newObj
      }, id) => newObj)(state, id)
    default:
      return state
  }
}

export const defaultActor = (state = "000000", { type, actorId}) => {
  if (type === actionTypes.actor.SET_DEFAULT) return actorId
  return state
}
