package com.kbhit.playground.scala

import org.scalatest.{FlatSpec, _}

class CustomListTest extends FlatSpec with Matchers {

  "A sum of [1,2]" should "be 3" in {
    CustomList.sum(CustomList[Int](1,2)) should be (3)
  }

}
