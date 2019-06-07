import { actionTypes } from './action'

export const editor = (state = true, { type, editor }) => {
    switch (type) {
      case actionTypes.editor.TOGGLE:
        return editor
      default:
        return state
    }
}
