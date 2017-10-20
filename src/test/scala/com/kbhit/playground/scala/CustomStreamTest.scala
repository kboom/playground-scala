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

  "constant stream of 1 when taking 4 items" should "be [1,1,1,1]" in {
    CustomStreamImpl.constant(2).take(4) should be (List(2,2,2,2))
  }

  "infinite stream of n = 2" should "be [2,3,4,5,6]" in {
    CustomStreamImpl.from(2).take(5) should be (List(2,3,4,5,6))
  }

  "fibs stream when taking 7 items" should "be [0,1,1,2,3,5,8]" in {
    CustomStreamImpl.fibs().take(7) should be (List(0,1,1,2,3,5,8))
  }

}
