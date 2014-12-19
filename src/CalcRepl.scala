import scala.annotation.tailrec

class CalcRepl(read: (String => String), write: (String => Unit)) {
  @tailrec
  final def run(v: Map[String, Double]): Unit = {
    var newV = v
    var ln = read("> ")
    if (ln.take(2) == ":q") {
      return
    }

    try {
      val res = Parser parse ln match {
        case Ass(n, e) =>
          val r = e.eval(v)
          newV = v + ((n, r))
          r
        case e         => e.eval(v)
      }
      write(res.toString)
    } catch {
      case e: Exception => println(e.getMessage)
    }

    run(newV)
  }

  def run(): Unit = {
    run(Map("e" -> math.E, "pi" -> math.Pi))
  }

}
