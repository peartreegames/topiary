export const actionTypes = {
  link: {
    NEW: 'NEW',
    DELETE: 'DELETE',
    DELETE_ALL: 'DELETE_ALL',
    SET_CURRENT: 'SET_CURRENT'
  }
}

export const newLink = ({
  from,
  to
}) => ({
  type: actionTypes.link.NEW,
  from,
  to
})

export const deleteLink = ({
  from,
  to
}) => ({
  type: actionTypes.link.DELETE,
  from,
  to
})

export const deleteAllLinks = ({
  id
}) => ({
  type: actionTypes.link.DELETE_ALL,
  id
})

export const setFocusedLink = ({
  status,
  from,
  to
}) => ({
  type: actionTypes.link.SET_CURRENT,
  status,
  from,
  to
})
