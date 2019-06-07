export const actionTypes = {
  nav: {
    UPDATE: 'UPDATE',
    SEARCH: 'SEARCH'
  }
}

export const updateScene = ({
  scene
}) => ({
  type: actionTypes.nav.UPDATE,
  scene
})

export const updateSearch = ({
  search
}) => ({
  type: actionTypes.nav.SEARCH,
  search
})
