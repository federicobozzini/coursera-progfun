package recfun

import scala.annotation.tailrec

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
    def pascal(c: Int, r: Int): Int = 
      if (r == 0) 1
      else {
        val left = if (c==0) 0 else pascal(c-1, r-1)
        val right = if (c==r) 0 else pascal(c, r-1)
        left + right
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
    @tailrec
      def balanceHelper(chars: List[Char], openBrackets: Int):Boolean = chars match {
        case Nil => openBrackets == 0
        case '('::tail => balanceHelper(chars.tail, openBrackets+1)
        case ')'::tail => if (openBrackets == 0) false else balanceHelper(chars.tail, openBrackets-1)
        case _::tail => balanceHelper(chars.tail, openBrackets)
      }
            
      balanceHelper(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChangeHelper(leftMoney: Int, sortedCoins: List[Int]): Int =
        if (leftMoney <= 0 || sortedCoins.isEmpty) 0 else
        (if (leftMoney - sortedCoins.head == 0) 1 else 0) + 
        (if (leftMoney <= 0) 0 else countChangeHelper(leftMoney, sortedCoins.tail)) + 
        (if (sortedCoins.isEmpty) 0 else countChangeHelper(leftMoney-sortedCoins.head, sortedCoins))
          
      countChangeHelper(money, coins.sorted)
    }
  }
