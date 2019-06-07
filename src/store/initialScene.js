import { rnd } from '../utils/math'

const nodeId = rnd()
export const defaultScene = (id = rnd()) => {
  return {
    id,
    scene: '',
    search: { status: false, text: '' },
    focusedNode: nodeId,
    focusedLink: { status: false, from: '', to: '' },
    collapsedNodes: [],
    nodes: {
      [nodeId]: {
        id: nodeId,
        type: 'dialogue',
        title: 'Start',
        tags: ['Intro', 'test'],
        body: 'And so our adventure begins...',
        pos: [710, 90],
        linkable: true,
        actor: '000000',
        replay: false
      }
    },
    links: {},
    editor: true,
    scale: 1,
    warning: { status: false, warningMessage: '' },
    actors: {}
  }
}

export const globals = {
  actors: {
    '000000': {
      id: '000000',
      name: 'Narrator',
      color: 'FFFFFF',
      isGlobal: true,
      variables: [{ AAAAAA: 0 }],
      factions: []
    }
  },
  actorVariables: {
    AAAAAA: {
      name: 'relationship',
      default: 0
    }
  },
  variables: {}
}
