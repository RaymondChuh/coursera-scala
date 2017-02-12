package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars1: List[Char], numOfLeftBracket: Int): Boolean = {
      if (chars1.isEmpty && numOfLeftBracket > 0 || numOfLeftBracket < 0) false
      else if (chars1.isEmpty && numOfLeftBracket == 0) true
      else if (chars1.head == '(') balance(chars1.tail, numOfLeftBracket + 1)
      else if (chars1.head == ')') balance(chars1.tail, numOfLeftBracket - 1)
      else balance(chars1.tail, numOfLeftBracket)
    }
    balance(chars, 0);
  }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = if (money == 0 || coins.isEmpty) 0 else 1
  }
