package recfun
import common._

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
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || c == 0 || r == c) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceNum(chars: List[Char], n: Int): Int = {
      if (chars.isEmpty) n
      else {
        if (chars.head == '(') balanceNum(chars.tail, n + 1)
        else {
          if (chars.head == ')') {
            if (n == 0) -1
            else balanceNum(chars.tail, n - 1)}
          else balanceNum(chars.tail, n)
        }
      }
    }

    0 == balanceNum(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else {
      if (money == 0) 1
      else {
        if (money >= coins.head) countChange(money - coins.head, coins) + countChange(money, coins.tail)
        else countChange(money, coins.tail)
      }
    }
  }
}
