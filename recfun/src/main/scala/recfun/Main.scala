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
    require(r >= 0)
    require(c >= 0 && c <= r)
    if (c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIterate(leftParentheses: Int, index: Int): Boolean = {
      if (leftParentheses < 0 || (index >= chars.length && leftParentheses != 0)) {
        false
      } else if (leftParentheses == 0 && index >= chars.length) {
        true
      } else if (chars(index) == '(') {
        balanceIterate(leftParentheses + 1, index + 1)
      } else if (chars(index) == ')') {
        balanceIterate(leftParentheses - 1, index + 1)
      } else {
        balanceIterate(leftParentheses, index + 1)
      }
    }
    if(chars.isEmpty)
      true
    else
      balanceIterate(0, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeIterate(money: Int, coins: List[Int]): Int = {
      if (money == 0) {
        1
      } else if ((coins.isEmpty && money > 0) || (!coins.isEmpty && money < coins(0))) {
        0
      } else {
        val count = money / coins(0)
        (0 to count).foldLeft(0){ (z, x) =>
          z + countChangeIterate(money - x * coins(0), coins.tail)
        }
      }
    }
    if (money <= 0)
      0
    else if (coins.isEmpty)
      0
    else {
      val ascList = coins.sortWith(_ < _)
      countChangeIterate(money, ascList)
    }
  }
}
