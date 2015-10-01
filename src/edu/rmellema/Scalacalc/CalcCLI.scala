package edu.rmellema.Scalacalc

object CalcCLI {
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
    if (ln.take(2) == ":q") return

    try {
      println(calc.evaluate(ln))
    } catch {
      case e: Exception => println("Error: " + e.getMessage)
    }
    run(calc)
  }

  def run(): Unit = run(new Calculator())

  def main(args: Array[String]): Unit = {
    println("Welcome to Scalacalc!")
    run()
  }
}
