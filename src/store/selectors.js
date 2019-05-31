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

export const makeGetNodeColors = () =>
  createSelector([getNodes, getActors], (nodes = {}, actors = {}) => 
    Object.values(nodes).reduce((acc, {id, actor}) => ({ ...acc, [id]: (actors && actors[actor] && actors[actor].color) || 'black' }), {}))

export const getAllChildNodes = (nodeId, links) => {
  const getChildLinks = (node, set) => {
    const children = links[node]
    if (!children) {
      return set
    }

    for (const child of children) {
      if (child === node) {
        continue;
      }
      if (!set.has(child)) {
        set.add(child)
        getChildLinks(child, set);
      }
    }

    return set;
  }

  const children = Array.from(getChildLinks(nodeId, new Set()))
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
