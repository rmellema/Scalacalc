object Tokenizer {
  def tokenize(s: String): List[String] = {
    if (s.isEmpty) {
      List()
    } else {
      val c: Char = s.head
      val t = s.tail
      if (c.isLetter) {
        c +: t.takeWhile(_.isLetter) :: tokenize(t.dropWhile(_.isLetter))
      } else if (c.isDigit) {
        c +: t.takeWhile(_.isDigit) :: tokenize(t.dropWhile(_.isDigit))
      } else if (c == '+' || c == '-' || c == '*' || c == '/' || c == '(' || c == ')' || c == '=') {
        c.toString :: tokenize(t)
      } else if (c.isSpaceChar) {
        tokenize(t.dropWhile(_.isSpaceChar))
      } else {
        sys.error("Illegal character '" + c + "' in input")
      }
    }
  }
}
