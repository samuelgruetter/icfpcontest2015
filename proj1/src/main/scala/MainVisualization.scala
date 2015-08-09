
import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{GridLayout, Frame, Font, BorderLayout}
import java.io.{PrintStream, ByteArrayOutputStream}
import javax.swing._
import javax.swing.event.{CaretEvent, CaretListener}

import Movers._

class Win1 extends JFrame {
  var solutions: Seq[VisualizableSolution] = null
  var calcTime: Long = -1

  def getSolutions: Seq[VisualizableSolution] = {
    val t0 = System.currentTimeMillis
    val res = getSolutionsRaw
    calcTime = System.currentTimeMillis - t0
    res
  }

  def getSolutionsRaw: Seq[VisualizableSolution] = {
    (0 to 23).flatMap(problemId => {
      val problem = JsonRead.problemFromFile(s"../probs/problem_$problemId.json")
      Main2.playOneProblem(problem, theMoverOfChoice).map(sol => new VisualizableSolution(problem, sol))
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

    solutions = getSolutions
    println(JsonWrite.solutionsToJsonString(solutions.map(_.solution)))
    //println(HexGrid.clearedLinesStats)
    println(s"Computing the solutions took ${calcTime}ms")

    for (sol <- solutions) cb.addItem(sol2StrId(sol))

    cb.setSelectedIndex(0)

    //commandsField.setText(currentSol.solution.solution)
    //showStep(solutions.head, "")
  }

  /** commands must have newlines between units,
    *  and Lock commands must be used to lock, not normal step commands */
  def showStep(sol: VisualizableSolution, commands: String): Unit = {
    val grid = HexGrid(sol.problem)
    val cmds = commands.split("\n")
    val units = RandomStream(sol.solution.seed).take(sol.problem.sourceLength)
      .map(rand => sol.problem.units(rand % sol.problem.units.length)).toList

    grid.spawnUnit(units.head)

    for (i <- 0 until cmds.length) {
      val cmd = cmds(i)

      for (moveChar <- cmd) {
        val step = Move.fromChar(moveChar).asInstanceOf[Step]
        if (step.locks) {
          grid.lockUnit()
          if (i+1 < units.length) grid.spawnUnit(units(i+1))
        } else {
          grid.step(step.direction)
        }
      }
    }

    textArea.setText(Util.printToString(ps => grid.printTo(ps)))
  }

  var cb: JComboBox[String] = null
  var textArea: JTextArea = null
  var problemSummary: JTextArea = null
  var commandsField: JTextArea = null
  var posLabel: JLabel = null

  def initLayout: Unit = {
    setSize(800, 600)
    val font = new Font("monospaced", Font.PLAIN, 24)

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

    problemSummary = new JTextArea()
    problemSummary.setFont(font)
    problemSummary.setText("description of problem")

    cb.setFont(font)
    cb.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        problemSummary.setText(Util.printToString(ps => Main.printOneProblem(currentSol.problem, ps)))
        problemSummary.setCaretPosition(0)

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
    val borderFor2Areas = new JPanel()
    borderFor2Areas.setLayout(new GridLayout(1, 2))
    borderFor2Areas.add(new JScrollPane(textArea))
    borderFor2Areas.add(new JScrollPane(problemSummary))
    add(borderFor2Areas, BorderLayout.CENTER)

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
