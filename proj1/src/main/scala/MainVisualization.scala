
import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Frame, Font, BorderLayout}
import java.io.{PrintStream, ByteArrayOutputStream}
import javax.swing._
import javax.swing.event.{CaretEvent, CaretListener}

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

  def currentSol: VisualizableSolution = strId2Sol(cb.getSelectedItem.asInstanceOf[String])

  def run: Unit = {
    initLayout

    println(JsonWrite.solutionsToJsonString(solutions.map(_.solution)))

    for (sol <- solutions) cb.addItem(sol2StrId(sol))

    commandsField.setText(currentSol.solution.solution)
    showStep(solutions.head, "")
  }

  /** commands must have newlines between units */
  def showStep(sol: VisualizableSolution, commands: String): Unit = {
    val grid = HexGrid(sol.problem)
    val cmds = commands.split("\n")
    val units = RandomStream(sol.solution.seed).take(sol.problem.sourceLength)
      .map(rand => sol.problem.units(rand % sol.problem.units.length))

    for ((cmd, unit) <- cmds zip units) {
      grid.spawnUnit(unit)
      for (moveChar <- cmd.drop(1)) { // drop lock move, newline already dropped by split
        val step = Move.fromChar(moveChar).asInstanceOf[Step]
        grid.move(step.direction)
      }
      grid.lockUnit()
    }
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)

    grid.printTo(ps)

    val content = baos.toString("ISO-8859-1")
    textArea.setText(content)
  }

  var cb: JComboBox[String] = null
  var textArea: JTextArea = null
  var commandsField: JTextArea = null
  var posLabel: JLabel = null

  def initLayout: Unit = {
    setSize(800, 600)
    val font = new Font("monospaced", Font.PLAIN, 36)

    cb = new JComboBox[String]()
    //val sl = new JSlider()
    commandsField = new JTextArea()
    //commandsField.setEditable(false)
    commandsField.setRows(1)
    commandsField.setAutoscrolls(true)
    commandsField.setText("line1\nline2\nline number three")
    commandsField.setFont(font)
    commandsField.addCaretListener(new CaretListener {
      override def caretUpdate(caretEvent: CaretEvent): Unit = {
        val cmds = commandsField.getText(0, caretEvent.getDot)//.replace("\n", "")
        posLabel.setText(s"Step ${cmds.length - cmds.count(_ == '\n')}")
        showStep(currentSol, cmds)
        //println(cmds)
      }
    })

    posLabel = new JLabel()
    posLabel.setFont(font)
    posLabel.setText("Step 0")

    val nav = new JPanel()
    nav.setLayout(new BorderLayout())

    nav.add(cb, BorderLayout.WEST)
    nav.add(commandsField, BorderLayout.CENTER)

    val commandsBorder = new JPanel()
    commandsBorder.setLayout(new BorderLayout())
    commandsBorder.add(new JScrollPane(commandsField), BorderLayout.PAGE_START)
    nav.add(commandsBorder, BorderLayout.CENTER)
    nav.add(posLabel, BorderLayout.EAST)

    cb.setFont(font)
    cb.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        //Win1.this.setTitle(Win1.this.getTitle + "hi")
        commandsField.setText(currentSol.solution.solution)
        commandsField.setCaretPosition(0)
        commandsField.requestFocus()
      }
    })

    textArea = new JTextArea()
    textArea.setFont(font)
    textArea.setEditable(false)

    setLayout(new BorderLayout)
    add(nav, BorderLayout.PAGE_START)
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


