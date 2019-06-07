export const actionTypes = {
  editor: {
    TOGGLE: 'TOGGLE'
  }
}

export const toggleEditor = ({ editor }) => ({
    type: actionTypes.editor.TOGGLE,
    editor
  })
