package com.kbhit.playground.scala

import org.scalatest.{FlatSpec, _}

class CustomOptionTest extends FlatSpec with Matchers {

  "map" should "work" in {
    Some(3).map(t => t * 2) should be (Some(6))
  }

  "flatMap" should "work" in {
    Some(3).flatMap(t => Some(t * 2)) should be (Some(6))
  }

  "orElse" should "work" in {
    Some(3).orElse(Some(1)) should be (Some(3))
    None.orElse(Some(1)) should be (Some(1))
  }

  "getOrElse" should "work" in {
    Some(3).getOrElse(1) should be (3)
    None.getOrElse(1) should be (1)
  }

  "filter" should "work" in {
    Some(3).filter(t => t < 2) should be (None)
  }

}
