package com.kbhit.playground.scala

import org.scalatest.{FlatSpec, Matchers}

class ListTest extends FlatSpec with Matchers {

  "A List" should "could be appended with element" in {
    1::List(2, 3) should be (List(1,2,3))
  }

  "A List" should "could be concatenated with another" in {
    List(1, 2) ::: List(3,4) should be (List(1,2,3,4))
  }

  "A List" should "could be reversed" in {
    List(2, 3).reverse should be (List(3,2))
  }

}

