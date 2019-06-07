export const actionTypes = {
    actor: {
      NEW: 'NEW',
      UPDATE: 'UPDATE',
        DELETE: 'DELETE',
      SET_DEFAULT: 'SET_DEFAULT'
    }
  }

export const newActor = ({
    actor
}) => ({
    type: actionTypes.actor.NEW,
    actor,
    id: actor.id
})

export const updateActor = ({
    id,
    actor
}) => ({
    type: actionTypes.actor.UPDATE,
    id,
    actor
})

export const deleteActor = ({
    id
}) => ({
    type: actionTypes.actor.DELETE,
    id
})

export const updateDefaultActor = (actorId) => ({ type: actionTypes.actor.SET_DEFAULT, actorId })
