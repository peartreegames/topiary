import React from "react"
import { connect } from "react-redux"
import PropTypes from "prop-types"
import { Snackbar } from "@material-ui/core"
import { setWarning } from "../store/actions"

function Warning({ warning, setWarning }) {
  return (
    <Snackbar
      message={warning.message}
      open={warning.status}
      action={"ok"}
      onRequestClose={() => setWarning({ warning: false, warningMessage: "" })}
      onActionClick={() => setWarning({ warning: false, warningMessage: "" })}
    />
  )
}

Warning.propTypes = {
  warning: PropTypes.object.isRequired,
  setWarning: PropTypes.func.isRequired
}

const mapState = ({ warning }) => ({ warning })

export default connect(mapState, { setWarning })(Warning)
