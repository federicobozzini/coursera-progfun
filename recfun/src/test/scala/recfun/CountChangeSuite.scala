package recfun

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange
  
  def t(money: Int, coins: List[Int], res: Int) =
    test("test " + money + " " + coins.toString()) {
    assert(countChange(money, coins) === res)
  }
  
  
  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }
  
  t(0, List(), 0)
  
  t(4, List(), 0)
  
  t(4, List(2, 1), 3)
  
  t(4, List(2, 1, 5), 3)

}
