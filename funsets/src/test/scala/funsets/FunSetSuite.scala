package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 * - run the "test" command in the SBT console
 * - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   * - test
   * - ignore
   * - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => x % 2 == 0, 100))
    assert(contains(x => x % 2 == 1, 101))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   * val s1 = singletonSet(1)
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

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("union contains union elements") {
    val s1 = (x: Int) => if (x % 2 == 0) true else false
    val s2 = (x: Int) => if (x % 5 == 0) true else false
    val s = union(s1, s2)
    for (x <- -bound to bound) {
      if (x % 5 == 0 || x % 2 == 0)
        assert(s(x))
      else
        assert(!s(x))
    }
  }

  test("intersect contains no elements") {
    new TestSets {
      val s = intersect(s1, s2)
      for (x <- -bound to bound) {
        assert(!s(x))
      }
    }
  }

  test("intersect contains elements") {
    val s1 = (x: Int) => if (x % 2 == 0) true else false
    val s2 = (x: Int) => if (x % 4 == 0) true else false
    val s = intersect(s1, s2)
    for (x <- -bound to bound) {
      if (x % 4 == 0)
        assert(s(x))
      else
        assert(!s(x))
    }
  }

  test("diff contains no elements") {
    val s1 = (x: Int) => if (x > 1000) true else false
    val s2 = (x: Int) => if (x % 2 == 1) true else false
    val s = diff(s1, s2)
    for (x <- -bound to bound) {
      assert(!s(x))
    }
  }

  test("diff contains diff elements") {
    val s1 = (x: Int) => if (x % 2 == 0) true else false
    val s2 = (x: Int) => if (x % 4 == 0) true else false
    val s = diff(s1, s2)
    for (x <- -bound to bound) {
      if (x % 2 == 0 && x % 4 != 0) {
        assert(s(x))
      }
      else
        assert(!s(x))
    }
  }

  test("filter contains no elements") {
    val s1 = (x: Int) => if (x % 2 == 0) true else false
    val s2 = (x: Int) => if (x % 2 == 1) true else false
    val s = filter(s1, s2)
    for (x <- -bound to bound) {
      assert(!s(x))
    }
  }

  test("filter contains elements") {
    val s1 = (x: Int) => if (x % 2 == 0) true else false
    val s2 = (x: Int) => if (x % 6 == 0) true else false
    val s = filter(s1, s2)
    for (x <- -bound to bound) {
      if (x % 6 == 0)
        assert(s(x))
      else
        assert(!s(x))
    }
  }

  test("forall test") {
    val s1 = (x: Int) => if (x % 6 == 0) true else false
    val s2 = (x: Int) => if (x % 2 == 0) true else false
    assert(forall(s1, s2))
    assert(!forall(s2, s1))
  }

  test("exists test") {
    val s1 = (x: Int) => if (x % 6 == 0) true else false
    val s2 = (x: Int) => if (x % 2 == 0) true else false
    val s3 = (x: Int) => if (x == 18) true else false
    val s4 = (x: Int) => if (x == 5) true else false
    assert(exists(s1, s2))
    assert(exists(s1, s3))
    assert(!exists(s1, s4))
  }

  test("map contains elements") {
    val s1 = (x: Int) => if (x % 2 == 0) true else false
    val p = (x: Int) => x + 1
    val s = map(s1, p)
    for (x <- -bound to bound) {
      if (x % 2 == 1 || x % 2 == -1)
        assert(s(x))
      else
        assert(!s(x))
    }
  }

  test("map contains no elements") {
    val s1 = (x: Int) => if (x % 2 == 0) true else false
    val p = (x: Int) => if(x < 0) x-10 else x+10
    val s = map(s1, p)
    for (x <- -10 to 10) {
      if (x == 10)
        assert(s(x))
      else
        assert(!s(x))
    }
  }

}
