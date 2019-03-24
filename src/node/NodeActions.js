export const updateNode = ({ id, payload }) => ({
  type: "UPDATE_NODE",
  id,
  payload
})

export const newNode = ({ id, payload }) => ({
  type: "NEW_NODE",
  id,
  payload
})

export const deleteNode = ({ id }) => {
  return {
    type: "DELETE_NODE",
    id
  }
}

export const setFocusedNode = ({ id }) => ({
  type: "SET_CURRENT_NODE",
  id
})
