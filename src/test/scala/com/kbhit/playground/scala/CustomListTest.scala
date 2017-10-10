package com.kbhit.playground.scala

import org.scalatest.{FlatSpec, _}

class CustomListTest extends FlatSpec with Matchers {

  "setHead 1 to [3,4]" should "be [1,3,4]" in {
    CustomList.setHead(CustomList[Int](3,4), 1) should be (CustomList[Int](1,3,4))
  }

  "append [1,2] to [3,4]" should "be [1,2,3,4]" in {
    CustomList.append(CustomList[Int](1,2), CustomList[Int](3,4)) should be (CustomList[Int](1,2,3,4))
  }

  "length of [1,2]" should "be 2" in {
    CustomList.length(CustomList[Int](1,2)) should be (2)
  }

  "A sum of [1,2]" should "be 3" in {
    CustomList.sum2(CustomList[Int](1,2)) should be (3)
  }

  "A product of [1,2,3]" should "be 6" in {
    CustomList.sum2(CustomList[Int](1,2,3)) should be (6)
  }

  "tail of [1,2]" should "be [2]" in {
    CustomList.tail(CustomList[Int](1,2)) should be (CustomList[Int](2))
  }

  "tail 2 of [1,2,3,4]" should "be [3,4]" in {
    CustomList.tail(CustomList[Int](1,2,3,4), 2) should be (CustomList[Int](3,4))
  }

  "dropWhile f != 2 called on [1,2,3,4]" should "be [3,4]" in {
    CustomList.dropWhile(CustomList[Int](1,2,3,4))(t => t <= 2) should be (CustomList[Int](3,4))
  }

  "reverse of [1,2,3]" should "be [3,2,1]" in {
    CustomList.reverse(CustomList[Int](1,2,3)) should be (CustomList[Int](3,2,1))
  }

  "map x -> x * 2 of [1,2]" should "be [2,4]" in {
    CustomList.map(CustomList[Int](1,2))(t => t * 2) should be (CustomList[Int](2,4))
  }

  "flatMap x -> List(x,x) of [1,2]" should "be [1,1,2,2]" in {
    CustomList.flatMap(CustomList[Int](1,2))(t => CustomList(t, t)) should be (CustomList[Int](1,1,2,2))
  }

  "filter x % 2 == 0 of [1,2,3,4]" should "be [2,4]" in {
    CustomList.filter(CustomList[Int](1,2,3,4))(t => t % 2 == 0) should be (CustomList[Int](2,4))
  }

  "sum of elements [1,2] and [3,4]" should "be [4,6]" in {
    CustomList.sumElements(CustomList(1,2), CustomList(3,4)) should be (CustomList[Int](4,6))
  }

}
