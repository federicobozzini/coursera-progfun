package recfun

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
    
    def t(col: Int, row: Int, res: Int) = 
      test("pascal: col="++col.toString()++", row="++row.toString()) {
        assert(pascal(col, row) === res)
      }
    
    test("pascal: col=0,row=2") {
      assert(pascal(0,2) === 1)
  }

    test("pascal: col=1,row=2") {
      assert(pascal(1,2) === 2)
  }

    test("pascal: col=1,row=3") {
      assert(pascal(1,3) === 3)
  }
    
    t(0, 0, 1)
    
    t(0, 1, 1)
    
    t(1, 1, 1)
    
    t(0, 2, 1)
    
    t(1, 2, 2)
    
    t(2, 2, 1)
    
    t(0, 3, 1)
    
    t(1, 3, 3)
    
    t(2, 3, 3)
    
    t(3, 3, 1)
    
    t(0, 4, 1)
    
    t(1, 4, 4)
    
    t(2, 4, 6)

}
