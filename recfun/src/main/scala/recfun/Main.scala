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
    def balanceRecursive(open: Int, closed: Int, remaining: List[Char]): Boolean = {
      if (open < closed) return false
      if (remaining.isEmpty) return open == closed
      if (remaining.head == '(') return balanceRecursive(open + 1, closed, remaining.tail)
      if (remaining.head == ')') return balanceRecursive(open, closed + 1, remaining.tail)
      balanceRecursive(open, closed, remaining.tail)
    }
    balanceRecursive(0, 0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def recursive(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else recursive(money - coins.head, coins) + recursive(money, coins.tail)
    }
    recursive(money, coins)
  }
}
