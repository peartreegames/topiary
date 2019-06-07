import { actionTypes } from './action'

export const globalActors = (state = {}, {
  type,
  id,
  actor
}) => {
  switch (type) {
    case actionTypes.actor.NEW: {
      if (!actor.isGlobal) {
        return state
      }

      return {
        ...state,
        [id]: actor
      }
    }

    case actionTypes.actor.UPDATE: {
      if (!actor.isGlobal) {
        return state
      }

      return {
        ...state,
        [id]: {
          ...state[id],
          ...actor
        }
      }
    }

    case actionTypes.actor.DELETE: {
      if (!actor.isGlobal) {
        return state
      }

      const newState = {...state}
      delete newState[id]
      return newState
    }

    default:
      return state
  }
}

export const sceneActors = (state = {}, {type, id, actor}) => {
  switch (type) {
    case actionTypes.actor.NEW: {
      if (actor.isGlobal) {
        return state
      }

      return {
        ...state,
        [id]: actor
      }
    }

    case actionTypes.actor.UPDATE: {
      if (actor.isGlobal) {
        return state
      }

      return {
        ...state,
        [id]: {
          ...state[id],
          ...actor
        }
      }
    }

    case actionTypes.actor.DELETE: {
      if (actor.isGlobal) {
        return state
      }

      const newState = {...state}
      delete newState[id]
      return newState
    }

    default:
      return state
  }
}

export const defaultActor = (state = "000000", { type, actorId}) => {
  if (type === actionTypes.actor.SET_DEFAULT) return actorId
  return state
}
