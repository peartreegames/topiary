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

export const makeGetNonCollapsedNodes = () => 
  createSelector([getNodes, getCollapsedNodes, getLinks], (nodes, collapsedNodes, links) => {
    const result = {...nodes}
    const getChildLinks = (nodeId, arr) => {
      const children = links[nodeId]
      if (!children) {
        return arr
      }
      let childLinks = []
      for (const child of children) {
        childLinks = [...childLinks, ...getChildLinks(child, arr)]
      }
      arr = [...arr, ...children, ...childLinks]
      return arr
    }

    for (const collapsedNode of collapsedNodes) {
      const children = getChildLinks(collapsedNode, [])
      for (const child of children) {
        delete result[child]
      }
    }

    return result
  })

export const makeGetNonCollapsedLinks = () => 
  createSelector([getLinks, getCollapsedNodes], (links, collapsedNodes) => {
    const result = { ...links }
    const getChildLinks = (nodeId, arr) => {
      const children = links[nodeId]
      if (!children) {
        return arr
      }
      let childLinks = []
      for (const child of children) {
        childLinks = [...childLinks, ...getChildLinks(child, arr)]
      }
      arr = [...arr, ...children, ...childLinks]
      return arr
    }

    for (const collapsedNode of collapsedNodes) {
      const children = getChildLinks(collapsedNode, [])
      for (const child of children) {
        delete result[child]
      }
      delete result[collapsedNode]
    }
    return links
  })
