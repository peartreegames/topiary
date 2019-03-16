export const newActor = ({ actor }) => ({
  type: "NEW_ACTOR",
  actor,
  id: actor.id
})

export const updateActor = ({ id, actor }) => ({
  type: "UPDATE_ACTOR",
  id,
  actor
})

export const deleteActor = ({ id }) => ({
  type: "DELETE_ACTOR",
  id
})

export const newColor = ({ color }) => ({
  type: "NEW_KEY",
  color
})

export const deleteColor = ({ index }) => ({
  type: "DELETE_COLOR",
  index
})
