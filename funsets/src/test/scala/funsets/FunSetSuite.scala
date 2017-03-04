package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    def g:Set = x => x<5
    def h: Set = x=> x%2 == 0
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")

      val l = union(g, h)
      assert(contains(l, 1), "Union 1")
      assert(contains(l, 2), "Union 2")
      assert(contains(l, 8), "Union 8")
      assert(!contains(l, 7), "Union 7")
    }
  }

  test("intersect") {
    new TestSets {
      val s = union(s1, s2)
      val t = union(s1, s3)
      val v = intersect(s, t)
      assert(contains(v, 1), "Intersect 1")
      assert(!contains(v, 2), "Intersect 2")
      assert(!contains(v, 3), "Intersect 3")

      val l = intersect(g, h)
      assert(!contains(l, 1), "Intersect 1")
      assert(contains(l, 2), "Intersect 2")
      assert(!contains(l, 8), "Intersect 8")
      assert(!contains(l, 7), "Intersect 7")
    }
  }

    test("diff") {
      new TestSets {
        val s = union(s1, s2)
        val t = union(s1, s3)
        val v = diff(s, t)
        assert(!contains(v, 1), "Diff 1")
        assert(contains(v, 2), "Diff 2")
        assert(!contains(v, 3), "Diff 3")

        val l = diff(g, h)
        assert(contains(l, 1), "Diff 1")
        assert(!contains(l, 2), "Diff 2")
        assert(!contains(l, 8), "Diff 8")
        assert(!contains(l, 7), "Diff 7")
      }
  }

  test("filter") {
    new TestSets {

      val l = filter(g, x=> x >1)
      assert(!contains(l, 1), "filter 1")
      assert(contains(l, 2), "filter 2")
      assert(!contains(l, 8), "filter 8")
      assert(!contains(l, 7), "filter 7")
    }
  }

  test("forall") {
    new TestSets {

      val l = filter(g, x=> x >1)
      //println(FunSets.toString(l))
      assert(forall(l, x=> x>1), "filter 1")
      assert(forall(l, x=> x>=2), "filter 2")
      assert(!forall(l, x=> x<2), "filter 8")
      assert(!forall(l, x=> x%2==0), "filter 7")
    }
  }

  test("map") {
    new TestSets {

      val l = filter(g, x=> x >1)
      val l2 = map(l, x=> x*2)
      var x = singletonSet(1000)
      val x2 = map(x, x=> x)
      //println(FunSets.toString(l))
      assert(contains(l2, 8), "filter 1")
      assert(contains(l2, 4), "filter 1")
      assert(!contains(l2, 3), "filter 1")
      assert(contains(l2, 6), "filter 1")
      assert(contains(l2, 6), "filter 1")
      assert(contains(x2, 1000), "filter 1")
    }
  }


}
