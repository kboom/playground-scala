package com.kbhit.playground.scala

import org.scalatest.{FlatSpec, _}

class CustomStreamTest extends FlatSpec with Matchers {

  "stream of 1,2,3" should "be converted to list [1,2,3]" in {
    CustomStreamImpl(1, 2, 3).toList should be (List(1, 2, 3))
  }

  "take 2 of stream of 1,2,3" should "be converted [1,2]" in {
    CustomStreamImpl(1, 2, 3).take(2) should be (List(1, 2))
  }

  "exists 2 of stream of 1,2,3" should "be true" in {
    CustomStreamImpl(1, 2, 3).exists((x) => x == 2) should be (true)
  }

  "forAll x % 2 == 0 of stream of 2,4,9,12" should "be false" in {
    CustomStreamImpl(2,4,9,12).forAll((x) => x % 2 == 0) should be (false)
  }

}
