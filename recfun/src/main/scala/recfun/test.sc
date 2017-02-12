def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

pascal(2, 4)



def balance(chars: List[Char]): Boolean = {
  def balance(chars1: List[Char], numOfLeftBracket: Int): Boolean = {
    if (numOfLeftBracket < 0) false
    else if (chars1.isEmpty && numOfLeftBracket == 0) true
    else if (chars1.head == '(') balance(chars1.tail, numOfLeftBracket + 1)
    else if (chars1.head == ')') balance(chars1.tail, numOfLeftBracket - 1)
    else balance(chars1.tail, numOfLeftBracket)
  }
  if (chars == Nil) false
  else if (chars.isEmpty) true
  else balance(chars, 0);
}

balance("(".toList)