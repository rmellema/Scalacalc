class Calculator {
  private var v = Map("e" -> math.E, "pi" -> math.Pi)

  def evaluate(ln: String): Double = {
    val res = Parser parse ln match {
      case Ass(n, e) =>
        val r = e(v)
        v = v + ((n, r))
        r
      case e => e(v)
    }
    res
  }

}
