package edu.rmellema.Scalacalc
import java.awt.BorderLayout
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{JTextField, JScrollPane, JTextArea, JFrame}

object CalcFrame extends JFrame with ActionListener {
  private val textArea:  JTextArea  = new JTextArea("Welcome to Scalacalc!\n", 25, 80)
  private val textField: JTextField = new JTextField(80)
  private val calc                  = new Calculator()

  private def write(s: String): Unit = textArea.append(s + "\n")

  override def actionPerformed(actionEvent: ActionEvent): Unit = {
    val ln = textField.getText
    write("> " + ln)
    textField.setText("")
    try {
      write(calc.evaluate(ln).toString)
    } catch {
      case e: Exception => write(e.getMessage)
    }
  }

  def initialize() = {
    setTitle("Scalacalc")
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    textArea.setEditable(false)
    textArea.setLineWrap(true)
    textArea.setWrapStyleWord(true)
    val scroll = new JScrollPane(textArea)
    add(scroll, BorderLayout.CENTER)
    textField.addActionListener(this)
    add(textField, BorderLayout.SOUTH)
    pack()
    setVisible(true)
  }

  def main(args: Array[String]): Unit = {
    println("Started running")
    initialize()
    println("Finished GUI")
  }
}
