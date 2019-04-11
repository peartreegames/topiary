import { createSelector } from "reselect"

const getNodeById = ({ nodes }, { id }) => nodes[id]
const getFocusedNode = ({ focusedNode }) => focusedNode
const getNodes = ({ nodes }) => nodes
const getActors = ({ actors }) => actors
const getLinks = ({links}) => links
const getLinksByNodeId = ({ links }, { id }) => links[id] || []
const getCollapsedNodes = ({ collapsedNodes }) => collapsedNodes

export const makeGetFlattenedLinks = () => 
  createSelector([getLinks], (links) => {
    const nestedLinks = Object.entries(links).map(([from, tos]) => (tos.map(to => [from, to])))
    return nestedLinks.flatMap(_ => _)
  })

export const makeGetNode = () =>
  createSelector(
    [getNodeById, getActors, getFocusedNode],
    (node, actors, focusedNode) => ({
      ...node,
      actor: node.actor !== undefined ? actors[node.actor].name : "",
      color: node.actor !== undefined ? actors[node.actor].color : "#FFFFFF",
      current: focusedNode === node.id
    })
  )

export const makeGetNodeByCurrent = () =>
  createSelector([getNodes, getFocusedNode], (nodes, focusedNode) => ({
    node: nodes[focusedNode]
  }))

export const makeConnectedNodes = () =>
  createSelector([getLinksByNodeId], (links) => ({ next: links }))

export const makeGetNodeKeys = () =>
  createSelector([getNodes], nodes => Object.keys(nodes))

export const getAllChildNodes = (nodeId, links) => {
  const getChildLinks = (node, arr) => {
    const children = links[node]
    if (!children) {
      return arr
    }
    let childLinks = []
    for (const child of children) {
      if (arr.includes(child) || childLinks.includes(child) || (childLinks.length > 0 && nodeId === node)) {
        continue
      }
      childLinks.push(child)
    }
    if (childLinks.length === 0) {
      return arr
    }
    arr = Array.from(new Set([...arr, ...children, ...childLinks]))
    return getChildLinks(childLinks, arr);
  }

  const children = getChildLinks(nodeId, [])
  return children
}

export const makeGetNonCollapsedNodes = () => 
  createSelector([getNodes, getCollapsedNodes, getLinks], (nodes, collapsedNodes, links) => {
    const result = {...nodes}
    let nodesToDelete = []
    for (const nodeId of collapsedNodes) {
      nodesToDelete = [...nodesToDelete, ...getAllChildNodes(nodeId, links)]
    }
    nodesToDelete.forEach(nodeId => {
      delete result[nodeId]
    })

    return result
  })

export const makeGetNonCollapsedLinks = () => 
  createSelector([getLinks, getCollapsedNodes], (links, collapsedNodes) => {
    const result = { ...links }
    let nodesToDelete = []
    for (const nodeId of collapsedNodes) {
      nodesToDelete = [...nodesToDelete, ...getAllChildNodes(nodeId, links)]
    }
    nodesToDelete.forEach(nodeId => {
      delete result[nodeId]
    })

    return result
  })
