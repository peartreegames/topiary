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
    variables: {},
  }
