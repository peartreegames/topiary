import { saveAs } from "file-saver"

export const throttle = (callback, limit) => {
  let wait = false
  return () => {
    if (!wait) {
      callback.call()
      wait = true
      setTimeout(() => {
        wait = false
      }, limit)
    }
  }
}

export const loadState = id => {
  try {
    const scene = localStorage.getItem(id)
    const globals = localStorage.getItem('globals')
    if (!scene) return null
    return {...JSON.parse(scene), ...JSON.parse(globals)}
  } catch (err) {
    return null
  }
}

export const saveState = state => {
  try {
    const { variables, actors, colors, ...scene } = state
    const globals = JSON.stringify({ variables, actors, colors })
    const serial = JSON.stringify(scene)
    localStorage.setItem('globals', globals)
    localStorage.setItem(state.id, serial)
  } catch (err) {
    return null
  }
}

const formatExport = (data) => {
  const { id, scene, nodes, links } = data

  const removeNodeFields = (node) => {
    delete node.pos
    delete node.linkable
    delete node.collapsed
  }

  Object.values(nodes).forEach(node => {
    removeNodeFields(node)
    node.links = links[node.id] || null
    if (node.conditions) {
      node.conditions.forEach(({conditionals}) => conditionals.forEach((conditional) => conditional.value = parseInt(conditional.value, 10)))
    }
    if (node.effects) {
      node.effects.forEach(effect => effect.value = parseInt(effect.value, 10))
    }
  })

  return JSON.stringify({ id, scene, nodes })
}

const formatGlobalsExport = (data) => {
  const { actors, variables } = data
  Object.values(actors).forEach(actor => {
    delete actor.color
  })

  const parsedVars = Object.entries(variables).reduce((acc, [key, value]) => ({ ...acc, [key]: parseInt(value, 10)}), {})

  return JSON.stringify({ actors, variables: parsedVars })
}

export const saveGlobals = (formatted = false) => {
  try {
    const data = JSON.parse(localStorage.getItem('globals'))
    const blob = new Blob([formatted ? formatGlobalsExport(data) : JSON.stringify(data)], {
      type: "text/plain;charset=utf-8"
    })
    const name = `globals${formatted ? '' : '-raw'}.topi.json`
    saveAs(blob, name)
  } catch (err) {
    console.log(err)
    return null
  }
}

export const saveFile = (scene, id, formatted = false) => {
  try {
    const data = JSON.parse(localStorage.getItem(id))
    const blob = new Blob([formatted ? formatExport(data) : JSON.stringify(data)], {
      type: "text/plain;charset=utf-8"
    })
    const name = `${scene.trim() || 'untitled'}${formatted ? '' : '-raw'}.topi.json`
    saveAs(blob, name)
  } catch (err) {
    console.log(err)
    return null
  }
}

export const loadFile = (e, cb) => {
  try {
    const r = new FileReader()
    r.onload = e => {
      const obj = JSON.parse(e.target.result)
      localStorage.setItem(obj.id, e.target.result)
      cb()
    }
    r.readAsText(e.target.files[0])
  } catch (err) {
    return null
  }
}
