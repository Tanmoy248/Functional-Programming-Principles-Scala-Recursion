package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    val paran = "I am ( no programmer )".toList
    println("Is paran balnaced ? " + balance(paran))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    (c,r) match {
      case (0, 0) => 1
      // column 0 , row 1 or column 1, row 1
      case (column, row) if (column.equals(row) || column.equals(0)) => 1
      case  (column, row) if column > 0 && row > 0 => pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val paranthesisStack = chars.filter(element => (element.equals('(') || element.equals(')')))
    //println(paranthesisStack)
    // write the base cases
    if (paranthesisStack.isEmpty) true
    else checkOpen(paranthesisStack, 0)

    }

    private def checkOpen(paranthesisStack: List[Char], open: Int) : Boolean = {
      //println("TRACE1 " + paranthesisStack)
      //println("TRACE head " + paranthesisStack.head)
      if (paranthesisStack.isEmpty && open == 0) true
      else
      if (paranthesisStack.head.equals('(')) checkOpen(paranthesisStack.tail, open + 1)
      else if(paranthesisStack.head.equals(')') && open > 0) checkOpen(paranthesisStack.tail, open - 1)
      else false

    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
