package com.neosavvy.ninetynineproblems

import com.neosavvy.ninetynineproblems._
import org.specs2.mutable._

/**
 * Created by aparrish on 7/12/14.
 */
class ProblemsSpec extends Specification {

  "The function last" should {
    "return the final element in a list" in {
      val p = new Problems()
      p.last(List(1, 1, 2, 3, 5, 8)) must be equalTo 8
    }
  }

  "The function penultimate" should {
    "return the next to last element in a list" in {
      val p = new Problems()
      p.penultimate(List(1, 1, 2, 3, 5, 8)) must be equalTo 5
    }
  }

  "The function nth" should {
    "return the n-th element in a list" in {
      val p = new Problems();
      p.nth(0, List(1, 1, 2, 3, 5, 8)) must be equalTo 1
      p.nth(1, List(1, 1, 2, 3, 5, 8)) must be equalTo 1
      p.nth(2, List(1, 1, 2, 3, 5, 8)) must be equalTo 2

    }
  }

  "The function length" should {
    "return the length of the input list" in {
      val p = new Problems()
      p.length(List(1,1,2,3)) must be equalTo 4
      p.length(List(1,1,2,3,5)) must be equalTo 5
      p.length(List(1,1,2,3,5,8)) must be equalTo 6
      p.length(List()) must be equalTo 0
    }
  }

  "The function reverse" should {
    "return the reversal of a list's contents" in {
      val p = new Problems()
      p.reverse(List()) must be equalTo List()
      p.reverse(List(1)) must be equalTo List(1)
      p.reverse(List(1, 2)) must be equalTo List(2, 1)
      p.reverse(List(1, 1, 2)) must be equalTo List(2, 1, 1)
      p.reverse(List(1, 1, 2, 3)) must be equalTo List(3, 2, 1, 1)
    }
  }


}
