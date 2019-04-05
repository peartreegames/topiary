import React from "react"
import { Card, CardHeader, CardContent } from "@material-ui/core"

const styles = {
  container: {
    marginTop: 30
  },
  card: {
    width: "100%",
    boxShadow:
      "rgba(0, 0, 0, 0.02) 0px 1px 1px, rgba(0, 0, 0, 0.02) 0px 1px 1px",
    marginBottom: "1px"
  },
  text: {
    margin: "20px"
  }
}

export default function HelpTab() {
  return (
    <div style={styles.container}>
      <Card style={styles.card}>
        <CardHeader title={"General"} expandable actAsExpander />
        <CardContent expandable>
          <strong>What am I looking at?</strong>
          <p style={styles.text}>
            Topiary is a dialogue tree editor inspired by{" "}
            <a href="http://twinery.org">Twinery</a> to create non-linear
            dialogue. There are only two main areas. The Tree area where you can
            move nodes and links around, and the Editor area where you can
            update the content of nodes and other assets you may have.
          </p>
          <strong>Who is this for and why did you make it?</strong>
          <p style={styles.text}>
            I created Topiary as a fun side project, if any game developers
            would like to use they are free to do so. The source code is kept on{" "}
            <a href="http://github.com/bgk-/topiary"> github.</a> if anyone
            would like to contribute or fork their own version.
          </p>
          <strong>What is exported?</strong>
          <p style={styles.text}>
            A JSON file will be exported soon. As of now, nothing.
          </p>
        </CardContent>
      </Card>
      <Card style={styles.card}>
        <CardHeader title={"Tree"} expandable actAsExpander />
        <CardContent expandable>
          <strong>New nodes</strong>
          <p style={styles.text}>
            There are two node types, dialogues and choices. The only difference
            is choices do not have actors or titles. The two circular buttons
            will create them.
          </p>
          <strong>Move nodes</strong>
          <p style={styles.text}>
            Click and drag on the header of any node to move it to a new
            location.
          </p>
          <strong>Delete nodes</strong>
          <p style={styles.text}>
            Click on the trash can icon at the bottom of any node. This will
            also delete all links connected to it.
          </p>
          <strong>Collapse nodes</strong>
          <p style={styles.text}>
            Not implemented yet, but a collapsed node will move all children to
            its position underneath.
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
        </CardContent>
      </Card>
      <Card style={styles.card}>
        <CardHeader title={"Editor"} expandable actAsExpander />
        <CardContent expandable>
          This is where the editor view help will go.
        </CardContent>
      </Card>
    </div>
  )
}
