import { rnd } from "../lib/math"

const nodeId = rnd()
export default function(id) {
  return {
    id,
    scene: "",
    search: { status: false, text: "" },
    focusedNode: nodeId,
    focusedLink: { status: false, from: "", to: "" },
    collapsedNodes: [],
    nodes: {
      [nodeId]: {
        id: nodeId,
        type: "dialogue",
        title: "Start",
        tags: ["Intro", "test"],
        body: "And so our adventure begins...",
        pos: [710, 90],
        linkable: true,
        collapsedPos: [],
        actor: "000000",
        condition: {},
        replay: false
      }
    },
    links: {},
    editor: true,
    scale: 1,
    warning: { status: false, warningMessage: "" }
  }
}

export const globals = {
    actors: {"000000": { id: "000000", name: "Narrator", playable: false, color: "FFFFFF", relationship: "0" }},
    colors: ["FFFFFF", "94E495", "85B7A1", "486B8D", "554A6E", "501D47"],
    variables: {},
  }
