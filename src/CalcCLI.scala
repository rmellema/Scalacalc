object CalcCLI {
  def main(args: Array[String]): Unit = {
    println("Welcome to Scalacalc!")
    new CalcRepl((str) => io.StdIn.readLine(str) match {
      case null =>
        println(":quit")
        ":quit"
      case s    => s
    }, println).run()
  }
}
