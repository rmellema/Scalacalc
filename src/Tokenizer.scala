object Tokenizer {
  def tokenize(s: String): List[String] = {
    if (s.isEmpty) {
      List.empty[String]
    } else {
      val c: Char = s.head
      val t = s.tail
      if (c.isLetter) {
        c +: t.takeWhile(_.isLetter) :: tokenize(t.dropWhile(_.isLetter))
      } else if (c.isDigit || c == '.') {
        c +: t.takeWhile((c) => c.isDigit || c == '.') :: tokenize(t.dropWhile((c) => c.isDigit || c == '.'))
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
