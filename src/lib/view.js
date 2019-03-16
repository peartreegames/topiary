const winWidth = window.innerWidth
const winHeight = window.innerHeight

export const gridSize = 30

export const layers = Object.freeze({
  GRID: 0,
  ARROWS: 3,
  TREE: 4,
  CURRENTTREEARROWS: 6,
  CURRENTTREE: 7,
  OVERLAY: 8,
  CURRENTARROW: 9,
  focusedNode: 10
})

export function dimensions(nodes, scale) {
  let width = winWidth
  let height = winHeight
  let passagesWidth = 0
  let passagesHeight = 0
  nodes.forEach(n => {
    const right = n.pos[0] + winWidth // + width
    const bottom = n.pos[1] + winHeight // + height

    if (right > passagesWidth) {
      passagesWidth = right
    }
    if (bottom > passagesHeight) {
      passagesHeight = bottom
    }
  })

  width = Math.max(passagesWidth / scale, winWidth)
  height = Math.max(passagesHeight / scale, winHeight)
  width += winWidth
  height += winHeight
  return {
    width: width + "px",
    height: height + "px"
  }
}

export const brightness = h => {
  const [r, g, b] = hexToRGB(h)
  return Math.sqrt(r * r * 0.299 + g * g * 0.587 + b * b * 0.114)
}

export const hexToRGB = hex => {
  if (hex[0] === "#") hex = hex.substr(1)
  const r = parseInt(hex.slice(0, 2), 16)
  const g = parseInt(hex.slice(2, 4), 16)
  const b = parseInt(hex.slice(4, 6), 16)
  return [r, g, b]
}
