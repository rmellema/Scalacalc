import java.awt.BorderLayout
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{JTextField, JScrollPane, JTextArea, JFrame}

object CalcFrame extends JFrame with ActionListener {
  private var textArea:  JTextArea  = null
  private var textField: JTextField = null
  private var entered:   String     = ""

  private def write(s: String): Unit = {
    textArea.append(s + "\n")
  }

  override def actionPerformed(actionEvent: ActionEvent): Unit = {
    entered = textField.getText
    write(entered)
    textField.setText("")
    println("Read text: " + entered)
  }

  private def read (s: String): Unit = {

  }

  def initialize() = {
    setTitle("Scalacalc")
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    textArea = new JTextArea("Welcome to Scalacalc!\n", 25, 80)
    textArea.setEditable(false)
    textArea.setLineWrap(true)
    textArea.setWrapStyleWord(true)
    val scroll = new JScrollPane(textArea)
    add(scroll, BorderLayout.CENTER)
    textField = new JTextField(80)
    textField.addActionListener(this)
    add(textField, BorderLayout.SOUTH)
    setVisible(true)
  }

  def main(args: Array[String]): Unit = {
    println("Started running")
    initialize()
    println("Finished GUI")
  }
}
