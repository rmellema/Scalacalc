import scala.annotation.tailrec

object Tokenizer {
  @tailrec
  private def tokenizeImpl(s: String, acc: List[String]): List[String] = {
    if (s.isEmpty) {
      acc
    } else {
      val c: Char = s.head
      val t = s.tail
      if (c.isLetter)
        tokenizeImpl(t.dropWhile(_.isLetter), c +: t.takeWhile(_.isLetter) :: acc)
      else if (c.isDigit || c == '.')
        tokenizeImpl(t.dropWhile((c) => c.isDigit || c == '.'), c +: t.takeWhile((c) => c.isDigit || c == '.') :: acc)
      else if (c == '+' || c == '-' || c == '*' || c == '/' || c == '(' || c == ')' || c == '=' || c == '^')
          tokenizeImpl(t, c.toString :: acc)
      else if (c.isSpaceChar)
        tokenizeImpl(t.dropWhile(_.isSpaceChar), acc)
      else
        sys.error("Illegal character '" + c + "' in input")
    }
  }

  def tokenize(s: String): List[String] = {
    tokenizeImpl(s, List.empty[String]).reverse
  }
}
