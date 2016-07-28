package edu.rmellema.Scalacalc

object CalcCLI {

  def printVersion = {}

  def printHelp = {
    println("This is Scalacalc, a simple calculator written in Scala.")
    println("Type in an arithmetic expression to have it evaluated. You can assign a value to a variable with `=`")
    println("\tfoo = 42")
    println("You can define functions using a similar syntax:")
    println("\tsquare(x) = x * x")
    println("The CLI understands commands starting with `:`. Currently implemented are:")
    println("\thelp | h \t Print this help message")
    println("\tquit | q \t Quit")
  }
  def read(str: String) = {
    io.StdIn.readLine(str) match {
      case null =>
        println(":quit")
        ":quit"
      case s => s
    }
  }

  def run(calc: Calculator): Unit = {
    val ln = read("> ")
    if (ln == ":q" || ln == ":quit") return
    if (ln.take(1) == ":") {
      ln.drop(1) match {
        case "quit" | "q" => return
        case "help" | "h" => printHelp
        case "version" | "v" => printVersion
      }
    } else {

      try {
        println(calc.evaluate(ln))
      } catch {
        case e: Exception => println("Error: " + e.getMessage)
      }
    }
    run(calc)
  }

  def run(): Unit = run(new Calculator())

  def main(args: Array[String]): Unit = {
    println("Welcome to Scalacalc!")
    run()
  }
}
