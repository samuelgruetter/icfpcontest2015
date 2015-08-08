
import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Frame, Font, BorderLayout}
import java.io.{PrintStream, ByteArrayOutputStream}
import javax.swing._

import Movers._

class Win1 extends JFrame {
  val solutions = getSolutions

  def getSolutions: Seq[VisualizableSolution] = {
    (0 to 23).flatMap(problemId => {
      val problem = JsonRead.problemFromFile(s"../probs/problem_$problemId.json")
      Main2.playOneProblem(problem, randomlyDown).map(sol => new VisualizableSolution(problem, sol))
    })
  }

  def sol2StrId(s: VisualizableSolution): String = s"${s.solution.problemId}/${s.solution.seed}"

  def strId2Sol(s: String): VisualizableSolution = {
    val sp = s.split("/")
    val problemId = sp(0).toInt
    val seed = sp(1).toInt
    solutions.find(sol => sol.solution.problemId == problemId && sol.solution.seed == seed).get
  }

  def run: Unit = {
    initLayout

    println(JsonWrite.solutionsToJsonString(solutions.map(_.solution)))

    for (sol <- solutions) cb.addItem(sol2StrId(sol))

    showStep(solutions.head, 0)
  }

  def showStep(sol: VisualizableSolution, stepId: Int): Unit = {
    val grid = new HexGrid(sol.problem.width, sol.problem.height)
    for (moveChar <- sol.solution.solution.take(stepId)) {
      val step = Move.fromChar(moveChar).asInstanceOf[Step]
      grid.move(step.direction)
    }
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)

    grid.printTo(ps)

    val content = baos.toString("ISO-8859-1")
    textArea.setText(content)
  }

  var cb: JComboBox[String] = null
  var textArea: JTextArea = null

  def initLayout: Unit = {
    setSize(800, 600)

    cb = new JComboBox[String]()
    val sl = new JSlider()
    val nav = new JPanel()
    nav.setLayout(new BorderLayout())

    nav.add(cb, BorderLayout.WEST)
    nav.add(sl, BorderLayout.CENTER)

    val font = new Font("monospaced", Font.PLAIN, 36)
    cb.setFont(font)
    cb.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        Win1.this.setTitle(Win1.this.getTitle + "hi")
      }
    })

    textArea = new JTextArea()
    textArea.setFont(font)
    textArea.setEditable(false)

    setLayout(new BorderLayout)
    add(nav, BorderLayout.NORTH)
    add(textArea, BorderLayout.CENTER)

    setExtendedState(getExtendedState() | Frame.MAXIMIZED_BOTH)

    setTitle("Visualization")
    setVisible(true)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  }
}

object MainVisualization {
  def main(args: Array[String]): Unit = {
    // Schedule a job for the event-dispatching thread: creating and showing this application's GUI.
    javax.swing.SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        new Win1().run
      }
    })
  }
}


