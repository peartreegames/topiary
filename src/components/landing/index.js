import React, { Component, Fragment } from 'react'
import { Redirect } from 'react-router-dom'
import {
  List,
  ListItem,
  ListItemText,
  ListItemIcon,
  Button,
} from '@material-ui/core'
import { defaultScene, globals } from 'store/initialScene'
import { saveFile, loadFile, saveGlobals } from 'store/localStorage'
import { rnd } from 'utils/math'

const styles = {
  name: {
    fontSize: 62,
    margin: '5vh auto 5vh auto',
    textAlign: 'center',
    color: '#558b2f'
  },
  space: {
    textAlign: 'center'
  },
  option: {
    textAlign: 'center',
    padding: '16px',
    textDecoration: 'none'
  },
  icons: {
    position: 'absolute',
    right: '5vw'
  },
  icon: {
    margin: '10px',
    float: 'right'
  }
}

export default class Landing extends Component {
  state = {
    scenes: [],
    usedSpace: 0,
    remainigSpace: 0,
    redirect: ''
  }
  newScene = () => {
    const id = rnd()
    localStorage.setItem(id, JSON.stringify(defaultScene(id)))
    const currentGlobals = localStorage.getItem('globals')
    if (!currentGlobals) {
      localStorage.setItem('globals', JSON.stringify(globals))
    }
    this.setState({ redirect: id })
  }

  deleteScene = i => {
    localStorage.removeItem(i)
    this.setState({ scenes: this.renderScenes() })
  }

  renderScenes = () => {
    let existingScenes = []
    for (let i = 0; i < localStorage.length; i++) {
      const storageKey = localStorage.key(i)

      let scene = {}
      try {
        scene = JSON.parse(localStorage.getItem(storageKey))
      } catch (error) {
        // do nothing
      }
      const sceneName = scene.scene
      if (sceneName !== undefined) {
        existingScenes.push(
          <ListItem
            key={sceneName || `untitled${i}`}
            button
            onClick={() => this.setState({ redirect: storageKey })}
          >
            <ListItemText style={styles.option}>
              {sceneName || 'untitled scene'}
            </ListItemText>
            <ListItemIcon style={styles.icons}>
              <Fragment>
                  <Button
                    style={styles.icon}
                    size="small"
                    onClick={e => {
                      e.stopPropagation()
                      saveFile(sceneName, storageKey)
                    }}
                  >
                    save
                  </Button>
                  <Button
                    style={styles.icon}
                    size="small"
                    onClick={e => {
                      e.stopPropagation()
                      saveFile(sceneName, storageKey, true)
                    }}
                  >
                    export
                  </Button>
                  <Button
                    style={styles.icon}
                    size="small"
                    onClick={e => {
                      e.stopPropagation()
                      this.deleteScene(storageKey)
                    }}
                  >
                    delete
                  </Button>
              </Fragment>
            </ListItemIcon>
          </ListItem>
        )
      }
    }
    existingScenes.push(
      <ListItem key={'globals'}>
        <ListItemText style={styles.option}>Global Variables</ListItemText>
        <ListItemIcon style={styles.icons}>
          <Fragment>
            <Button style={styles.icon} size="small" onClick={() => saveGlobals()}>
              save
            </Button>
            <Button
              style={styles.icon}
              size="small"
              onClick={() => saveGlobals(true)}
            >
              export
            </Button>
            <Button
              style={styles.icon}
              size="small"
              onClick={() => this.deleteScene('globals')}
            >
              delete
            </Button>
          </Fragment>
        </ListItemIcon>
      </ListItem>
    )
    return existingScenes.sort((a, b) => a.key > b.key)
  }
  componentWillMount() {
    // TODO: Get this working properly
    window.navigator.webkitTemporaryStorage.queryUsageAndQuota(
      (used, remaining) => {
        this.setState({
          usedSpace: (used / 1024 ** 3).toFixed(2) + 'mb',
          remainingSpace: (remaining / 1024 ** 3).toFixed(2) + 'mb'
        })
      }
    )
    return this.setState({ scenes: this.renderScenes() })
  }

  render() {
    if (this.state.redirect) {
      return <Redirect to={`/scene/${this.state.redirect}`} />
    }
    return (
      <div>
        <div style={styles.name}> topiary </div>
        <div style={styles.space}>
          {`Used: ${this.state.usedSpace} | Remaining: ${
            this.state.remainingSpace
          }`}
        </div>
        <List>
          <ListItem button onClick={this.newScene}>
            <ListItemText style={styles.option}>new</ListItemText>
          </ListItem>
          <ListItem button variant="contained" component="label">
            <ListItemText style={styles.option}>{'load'}</ListItemText>
            <input
              type="file"
              onChange={e => {
                loadFile(e, () => {
                  this.setState({ scenes: this.renderScenes() })
                })
              }}
              style={{ display: 'none' }}
            />
          </ListItem>
          {this.state.scenes}
        </List>
      </div>
    )
  }
}
