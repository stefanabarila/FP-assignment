package intro

import org.scalatest.FunSuite
import intro.PatternMatching2._

class PatternMatching2Test extends FunSuite{


  test(" twice") {
    assertResult(List("abc", "abc")) {
      twice(List("abc"))
    }
  }

  test("drunkWords") {
    assertResult(List("uoy","yeh")) {
      drunkWords(List("hey", "you"))
    }
  }

  test("myForAll"){
    val startsWithD = (d: String) => d.startsWith("d")

    assert(myForAll(Nil, startsWithD))
    assert(!myForAll(List("ab", "a", "add", "c"), startsWithD))
    assert(!myForAll(List("AA", "dd", "D", "c"), startsWithD))
    assert(myForAll(List("dab", "da", "dadd", "dc"), startsWithD))

  }

  test("lastElem") {
    assertResult(Some("yes")) {
      lastElem(List("no", "yes", "no", "no", "yes"))
    }
  }

  test("append") {
    assertResult(List(1,3,5,2,4)) {
      append(List(1,3,5), List(2,4))
    }
  }
}
