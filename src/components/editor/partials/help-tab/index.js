import React from "react"
import { ExpansionPanel, ExpansionPanelSummary, ExpansionPanelDetails, Typography } from "@material-ui/core"
import { ExpandMore } from "@material-ui/icons";

const styles = {
  container: {
    marginTop: 30
  },
  text: {
    margin: "10px"
  }
}

export default function HelpTab() {
  return (
    <div style={styles.container}>
      <ExpansionPanel>
        <ExpansionPanelSummary expandIcon={<ExpandMore />}>
          <Typography variant="h6">General</Typography>
        </ExpansionPanelSummary>
        <ExpansionPanelDetails>
          <Typography>
          <strong>What am I looking at?</strong>
          <p style={styles.text}>
            Topiary is a dialogue tree editor inspired by{" "}
            <a href="http://twinery.org">Twinery</a> to create non-linear
            dialogue. There are only two main areas. The Tree area where you can
            move nodes and links around, and the Editor area where you can
            update the Details of nodes and other assets you may have.
          </p>
          <strong>Who is this for and why did you make it?</strong>
          <p style={styles.text}>
            I created Topiary as a fun side project, if any game developers
            would like to use they are free to do so. The source code is kept on{" "}
            <a href="http://github.com/peartreegames/topiary"> github.</a> if anyone
            would like to contribute or fork their own version.
          </p>
          <strong>What is exported?</strong>
          <p style={styles.text}>
            A JSON file is a exported as a scrubbed version of the saved state. It includes all of the data minus the positions, colours, etc.
          </p>
          </Typography>
        </ExpansionPanelDetails>
      </ExpansionPanel>
      <ExpansionPanel>
      <ExpansionPanelSummary expandIcon={<ExpandMore />}>
          <Typography variant="h6">Tree</Typography>
        </ExpansionPanelSummary>
        <ExpansionPanelDetails>
          <Typography>
          <strong>New nodes</strong>
          <p style={styles.text}>
            There are three node types, roots, dialogues and choices. All conversations should start with a root. 
            But you can choose to implement it however you see fit.
            <ul>
              <li>Roots only have titles.</li>
              <li>Dialogues have conditions and actors.</li>
              <li>Choices have conditions and effects.</li>
            </ul>
          </p>
          <strong>Move nodes</strong>
          <p style={styles.text}>
            Click and drag on the header of any node to move it to a new
            location.

            Hold SHIFT and drag on the header to move all child nodes.
          </p>
          <strong>Delete nodes</strong>
          <p style={styles.text}>
            Click on the trash can icon at the bottom of any node. This will
            also delete all links connected to it.
          </p>
          <strong>Collapse nodes</strong>
          <p style={styles.text}>
            Collapse all child nodes so you can get them out of the way.
          </p>
          <strong>New links</strong>
          <p style={styles.text}>
            Click on the arrow icon at the bottom of any node, then click on the
            node you want to link it to. If you click off of a node the link
            will be deleted.
          </p>
          <strong>Move links</strong>
          <p style={styles.text}>
            Click on the link you want to change, then click a different node.
          </p>
          <strong>Delete links</strong>
          <p style={styles.text}>
            Click on the link you want to delete, then click off a node anywhere
            on the grid.
          </p>
          </Typography>
        </ExpansionPanelDetails>
      </ExpansionPanel>
      <ExpansionPanel>
      <ExpansionPanelSummary expandIcon={<ExpandMore />}>
          <Typography variant="h6">Editor</Typography>
        </ExpansionPanelSummary>
        <ExpansionPanelDetails>
          This is where the editor view help will go.
        </ExpansionPanelDetails>
      </ExpansionPanel>
      <ExpansionPanel>
      <ExpansionPanelSummary expandIcon={<ExpandMore />}>
          <Typography variant="h6">Actors</Typography>
        </ExpansionPanelSummary>
        <ExpansionPanelDetails>
          This is where the actors view help will go.
        </ExpansionPanelDetails>
      </ExpansionPanel>
      <ExpansionPanel>
      <ExpansionPanelSummary expandIcon={<ExpandMore />}>
          <Typography variant="h6">Variables</Typography>
        </ExpansionPanelSummary>
        <ExpansionPanelDetails>
          This is where the variables help will go.
        </ExpansionPanelDetails>
      </ExpansionPanel>
    </div>
  )
}
