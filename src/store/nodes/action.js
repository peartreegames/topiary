export const actionTypes = {
  node: {
    NEW: 'NEW',
    UPDATE: 'UPDATE',
    UPDATE_MANY: 'UPDATE_MANY',
    DELETE: 'DELETE',
    SET_CURRENT: 'SET_CURRENT',
    ADD_COLLAPSED: 'ADD_COLLAPSED',
    REMOVE_COLLAPSED: 'REMOVE_COLLAPSED'
  }
}

export const updateNode = ({ id, payload }) => ({
  type: actionTypes.node.UPDATE,
  id,
  payload
})

export const updateNodes = ({ payload: nodes }) => ({
  type: actionTypes.node.UPDATE_MANY,
  nodes
})

export const newNode = ({ id, payload }) => ({
  type: actionTypes.node.NEW,
  id,
  payload
})

export const deleteNode = ({ id }) => {
  return {
    type: actionTypes.node.DELETE,
    id
  }
}

export const setFocusedNode = ({ id }) => ({
  type: actionTypes.node.SET_CURRENT,
  id
})

export const addCollapsedNode = ( nodeId ) => {
  return { type: actionTypes.node.ADD_COLLAPSED, nodeId }
}

export const removeCollapsedNode = (nodeId) => ({ type: actionTypes.node.REMOVE_COLLAPSED, nodeId })
