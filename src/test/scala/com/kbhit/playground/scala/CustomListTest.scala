package com.kbhit.playground.scala

import org.scalatest.{FlatSpec, _}

class CustomListTest extends FlatSpec with Matchers {

  "A sum of [1,2]" should "be 3" in {
    CustomList.sum(CustomList[Int](1,2)) should be (3)
  }

  "tail of [1,2]" should "be [2]" in {
    CustomList.tail(CustomList[Int](1,2)) should be (CustomList[Int](2))
  }

  "tail 2 of [1,2,3,4]" should "be [3,4]" in {
    CustomList.tail(CustomList[Int](1,2,3,4), 2) should be (CustomList[Int](3,4))
  }

}
