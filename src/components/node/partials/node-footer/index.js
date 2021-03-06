import React, { Fragment } from 'react'
import PropTypes from 'prop-types'
import { CardActions, Button, Icon, Tooltip } from '@material-ui/core'

const styles = {
  corner: {
    position: 'absolute',
    bottom: 0,
    right: 0,
    margin: 0,
    padding: 0,
    transition: `transform 450ms cubic-bezier(0.23, 1, 0.32, 1) 0ms`
  },
  footer: {
    height: '16px',
    textAlign: 'right',
    padding: '1em',
    marginTop: '2em'
  },
  button: {
    width: 14,
    height: 14,
    padding: 0,
    margin: 0,
    fontSize: 14,
    transition: 'none'
  },
  icon: {
    fontSize: 14
  }
}

export default function NodeFooter({
  id,
  expanded,
  current,
  collapse,
  setFocusedLink,
  deleteAllLinks,
  deleteNode,
  isCollapsed,
  collapsedNodes,
  color
}) {
  return (
    <Fragment>
      <CardActions
        style={{
          ...styles.footer,
          background: current
            ? `linear-gradient(0deg, ${color} 10%, #FFFFFF 10%)`
            : '#FFFFFF'
        }}
      >
        <Tooltip title="delete">
          <Button
            style={{
              ...styles.button,
              opacity: expanded ? '100' : '0',
              pointerEvents: expanded ? 'auto' : 'none'
            }}
            onClick={() => {
              deleteAllLinks({ id })
              deleteNode({ id })
            }}
          >
            <Icon style={styles.icon} className="material-icons">
              delete
            </Icon>
          </Button>
        </Tooltip>
        <Tooltip title="link">
          <Button
            style={styles.button}
            onClick={() =>
              setFocusedLink({
                status: true,
                from: id
              })
            }
          >
            <Icon style={styles.icon} className="material-icons">
              arrow_forward
            </Icon>
          </Button>
        </Tooltip>
        <Tooltip title="collapse">
          <Button
            style={styles.button}
            color={isCollapsed ? 'secondary' : 'default'}
            onClick={collapse}
          >
            <Icon style={styles.icon} className="material-icons">
              layers
            </Icon>
          </Button>
        </Tooltip>
      </CardActions>
    </Fragment>
  )
}

NodeFooter.propTypes = {
  id: PropTypes.string.isRequired,
  expanded: PropTypes.bool.isRequired,
  current: PropTypes.bool.isRequired,
  collapse: PropTypes.func.isRequired,
  color: PropTypes.string,
  setFocusedLink: PropTypes.func.isRequired,
  deleteAllLinks: PropTypes.func.isRequired,
  deleteNode: PropTypes.func.isRequired,
  isCollapsed: PropTypes.bool.isRequired,
  collapsedNodes: PropTypes.array
}
