object Parser {
  private def subExpression(d: Int, s: List[String]): List[String] = {
    if (s.head == ")") {
      if (d < 1) {
        List()
      } else {
        s.head +: subExpression(d - 1, s.tail)
      }
    } else if (s.head == "(") {
      s.head +: subExpression(d + 1, s.tail)
    } else {
      s.head +: subExpression(d, s.tail)
    }
  }

  def subExpression(s: List[String]): List[String]
                = subExpression(0, s)

  def parseT(e: Expr, s: List[String]): Expr = {
    if (s.isEmpty) {
      e
    } else {
      val r = parseF(s.tail)
      s.head match {
        case "*" => parseT(Mul(e, r._1), r._2)
        case "/" => parseT(Div(e, r._1), r._2)
        case "%" => parseT(Mod(e, r._1), r._2)
        case "+" => Sum(e, parseT(r._1, r._2))
        case "-" => Sub(e, parseT(r._1, r._2))
        case "=" => e match {
          case Var(n) => Ass(n, parseT(r._1, r._2))
          case _      => sys.error("Trying to assign to something that is not a variable")
        }
      }
    }
  }

  def parseF(s: List[String]): (Expr, List[String]) = {
    val h = s.head
    val t = s.tail
    if (h.head.isDigit) {
      (Val(h.toDouble), t)
    } else if (h.head.isLetter) {
      (Var(h), t)
    } else if (h == "(") {
      val sub: List[String] = subExpression(t)
      (parse(sub), t.drop(sub.length + 1))
    } else {
      (Val(0.0), List())
    }
  }

  def parse(s: List[String]): Expr = {
    if (s.isEmpty) {
      Val(0.0)
    } else {
      val l = parseF(s)
      parseT(l._1, l._2)
    }
  }

  def parse(s: String): Expr = parse(Tokenizer.tokenize(s))

}
