export default (state = true, { type, editor }) => {
    switch (type) {
      case "TOGGLE_EDITOR":
        return editor
      default:
        return state
    }
}
