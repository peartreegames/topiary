export const scene = (state = "", { type, scene }) => {
  if (type === "UPDATE_SCENE") {
    return scene
  }
  return state
}

export const search = (state = {}, { type, search }) => {
  if (type === "UPDATE_SEARCH") {
    return { ...state, ...search }
  }
  return state
}
