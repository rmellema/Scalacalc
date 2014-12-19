object CalcCLI {
  def main(args: Array[String]): Unit = {
    println("Welcome to Scalacalc!")
    (new CalcRepl((str) => io.StdIn.readLine(str), println)).run()
  }
}
