package com.kbhit.playground.scala

import org.scalatest.{FlatSpec, _}

class CustomOptionTest extends FlatSpec with Matchers {

  "map" should "work" in {
    CustomSome(3).map(t => t * 2) should be (CustomSome(6))
  }

  "flatMap" should "work" in {
    CustomSome(3).flatMap(t => CustomSome(t * 2)) should be (CustomSome(6))
  }

  "orElse" should "work" in {
    CustomSome(3).orElse(CustomSome(1)) should be (CustomSome(3))
    CustomNone.orElse(CustomSome(1)) should be (CustomSome(1))
  }

  "getOrElse" should "work" in {
    CustomSome(3).getOrElse(1) should be (3)
    CustomNone.getOrElse(1) should be (1)
  }

  "filter" should "work" in {
    CustomSome(3).filter(t => t < 2) should be (CustomNone)
  }

  "map2" should "work" in {
    CustomOption.map2(CustomSome(2), CustomSome(3))((x: Int, y: Int) => x * y) should be (CustomSome(6))
    CustomOption.map2(CustomSome(2), CustomNone)((x: Int, y: Int) => x * y) should be (CustomNone)
    CustomOption.map2(CustomNone, CustomSome(2))((x: Int, y: Int) => x * y) should be (CustomNone)
  }

  "sequence" should "work" in {
    CustomOption.sequence(List(Option(1), Option(3))) should be (scala.Some(List(1, 3)))
    CustomOption.sequence(List(Option(1), scala.None)) should be (scala.None)
  }

}
