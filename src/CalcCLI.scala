object CalcCLI {
  def main(args: Array[String]): Unit = {
    println("Welcome to the calculator!")
    var ln = scala.io.StdIn.readLine("> ")
    if (ln == null) {
      ln = ":quit"
      println(ln)
    }
    var e: Expr = Val(0.0)
    var v = Map[String, Double]()
    var res: Double = 0.0
    while (ln.take(2) != ":q") {
      try {
        e = Parser.parse(ln)
        res = e match {
          case Ass(n, c) =>
            val r = c.eval(v)
            v = v + ((n, r))
            r
          case _ => e.eval(v)
        }
        println(res)
      } catch {
        case e: Exception => println(e.getMessage)
      }
      ln = scala.io.StdIn.readLine("> ")
      if (ln == null) {
        ln = ":quit"
        println(ln)
      }
    }
  }
}
