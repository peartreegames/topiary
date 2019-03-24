import React from "react"
import PropTypes from "prop-types"
import { CardHeader, Typography } from "@material-ui/core"
import { brightness } from "../../lib/view"
import { ExpandLess, ExpandMore, HelpOutline, ErrorOutline } from '@material-ui/icons'

const styles = {
  rootTitle: {
    fontSize: 14
  },
  title: {
    paddingRight: 20,
    fontSize: 11
  },
  subtitle: {
    fontSize: 10
  },
  expand: {
    position: "absolute",
    right: "10px",
    top: "10px"
  }
}

export default function NodeHeader({
  type,
  title,
  actor,
  color,
  expanded,
  expand,
  conditions,
  effects
}) {
  const fontColor = brightness(color) > 165 ? "#000" : "#EEE"
  return (
    <CardHeader
      title={<Typography style={type !== 'root' ? styles.title : styles.rootTitle}>{title} {effects && <ErrorOutline style={{fontSize: 11}}/>}{conditions && <HelpOutline style={{fontSize: 11}}/>} </Typography>}
      action={expanded ? <ExpandMore style={styles.expand} onClick={() => expand(!expanded)} /> : <ExpandLess style={styles.expand} onClick={() => expand(!expanded)} />}
      subheader={<Typography style={styles.subtitle}>{actor}</Typography>}
      style={{
        fontWeight: "bold",
        backgroundColor: `#${color}`,
        color: fontColor
      }}
      className={"draggable"}
    />
  )
}

NodeHeader.propTypes = {
  type: PropTypes.string.isRequired,
  color: PropTypes.string.isRequired,
  actor: PropTypes.string.isRequired,
  expanded: PropTypes.bool.isRequired,
  expand: PropTypes.func.isRequired,
  condition: PropTypes.bool,
  sets: PropTypes.bool
}