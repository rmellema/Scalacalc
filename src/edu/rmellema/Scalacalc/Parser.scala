package edu.rmellema.Scalacalc
import scala.annotation.tailrec

object Parser {
  @tailrec
  private def subExpression(d: Int, s: List[String], acc: List[String]): List[String] = {
    if (s.head == ")") {
      if (d < 1) acc
      else subExpression(d - 1, s.tail, s.head +: acc)
    }
    else if (s.head == "(") subExpression(d + 1, s.tail, s.head +: acc)
    else subExpression(d, s.tail, s.head +: acc)
  }

  def subExpression(s: List[String]): List[String]
                = subExpression(0, s, List.empty[String]).reverse

  @tailrec
  private def split(s: String, w: List[String], acc: List[List[String]]): List[List[String]] = {
    if (w.isEmpty) acc
    else {
      val end = w.dropWhile(_ != s)
      split(s, if (end.isEmpty) end else end.tail, w.takeWhile(_ != s) :: acc)
    }
  }

  def split(s: String, x: List[String]): List[List[String]] = {
    split(s, x, List.empty[List[String]]).reverse
  }

  def parseT(e: Expr, s: List[String]): Expr = {
    if (s.isEmpty) e
    else {
      val r = parseF(s.tail)
      s.head match {
        case "^" => parseT(Pow(e, r._1), r._2)
        case "*" => parseT(Mul(e, r._1), r._2)
        case "/" => parseT(Div(e, r._1), r._2)
        case "%" => parseT(Mod(e, r._1), r._2)
        case "+" => Sum(e, parseT(r._1, r._2))
        case "-" => Sub(e, parseT(r._1, r._2))
        case "=" => e match {
          case Var(n)     => Ass(Call(n, List.empty[Expr]), parseT(r._1, r._2))
          case Call(n, a) => Ass(Call(n, a), parseT(r._1, r._2))
          case _      => sys.error("Trying to assign to something that is not a variable")
        }
        case t   => sys.error("Unexpected token in string: " + t)
      }
    }
  }

  def parseF(s: List[String]): (Expr, List[String]) = {
    val h = s.head
    val t = s.tail
    if      (h.head.isDigit || h.head == '.')
      (Val(if (h.indexOf('.') >= 0) Real(h.toDouble) else Integer(h.toInt)), t)
    else if (h.head.isLetter) {
      if (!t.isEmpty && t.head == "(") {
        val sub = subExpression(t.tail)
        (Call(h, split(",", sub).map(parse)), t.tail.drop(sub.length + 1))
      } else {
        (Var(h), t)
      }
    }
    else if (h == "(") {
      val sub: List[String] = subExpression(t)
      (parse(sub), t.drop(sub.length + 1))
    } else {
      if (h == "-" & t.head.head.isDigit) (Val(-Real(t.head.toDouble)), t.tail)
      else sys.error("Unexpected token: '" + h + "'")
    }
  }

  def parse(s: List[String]): Expr = {
    if (s.isEmpty) Val(Nil)
    else {
      val l = parseF(s)
      parseT(l._1, l._2)
    }
  }

  def parse(s: String): Expr = parse(Tokenizer.tokenize(s))

}
