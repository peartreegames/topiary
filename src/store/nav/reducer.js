import { actionTypes } from './action'

export const scene = (state = "", {
  type,
  scene
}) => {
  if (type === actionTypes.nav.UPDATE) {
    return scene
  }
  return state
}

export const search = (state = {}, {
  type,
  search
}) => {
  if (type === actionTypes.nav.SEARCH) {
    return {
      ...state,
      ...search
    }
  }
  return state
}
