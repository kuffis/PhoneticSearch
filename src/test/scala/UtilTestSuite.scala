import org.scalatest.FunSuite

class UtilTestSuite extends FunSuite {

  test("it joins am array of 0 strings") {
    assert(Util.join(Array[String]()) == "")
  }
  test("it joins am array of 1 strings") {
    assert(Util.join(Array[String]("the one ring"), ",") == "the one ring")
  }
  test("it joins am array of many strings") {
    assert(Util.join(Array[String]("kissa","koira"), " ja ") == "kissa ja koira")
  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    assertThrows[NoSuchElementException] {
      Set.empty.head
    }
  }
}
