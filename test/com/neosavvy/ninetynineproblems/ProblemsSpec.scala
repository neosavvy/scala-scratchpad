package com.neosavvy.ninetynineproblems

import com.neosavvy.ninetynineproblems.Problems._
import org.specs2.mutable._

/**
 * Created by aparrish on 7/12/14.
 */
class ProblemsSpec extends Specification {

  "The function last" should {
    "return the final element in a list" in {
      last(List(1, 1, 2, 3, 5, 8)) must be equalTo 8
    }
  }

  "The function penultimate" should {
    "return the next to last element in a list" in {
      penultimate(List(1, 1, 2, 3, 5, 8)) must be equalTo 5
    }
  }

  "The function nth" should {
    "return the n-th element in a list" in {
      nth(0, List(1, 1, 2, 3, 5, 8)) must be equalTo 1
      nth(1, List(1, 1, 2, 3, 5, 8)) must be equalTo 1
      nth(2, List(1, 1, 2, 3, 5, 8)) must be equalTo 2

    }
  }

  "The function length" should {
    "return the length of the input list" in {
      Problems.length(List(1,1,2,3)) must be equalTo 4
      Problems.length(List(1,1,2,3,5)) must be equalTo 5
      Problems.length(List(1,1,2,3,5,8)) must be equalTo 6
      Problems.length(List()) must be equalTo 0
    }
  }

  "The function reverse" should {
    "return the reversal of a list's contents" in {
      reverse(List()) must be equalTo List()
      reverse(List(1)) must be equalTo List(1)
      reverse(List(1, 2)) must be equalTo List(2, 1)
      reverse(List(1, 1, 2)) must be equalTo List(2, 1, 1)
      reverse(List(1, 1, 2, 3)) must be equalTo List(3, 2, 1, 1)
    }
  }

  "The function isPalindrome" should {
    "return the true or false if the input array is a palindrome" in {
      isPalindrome(List()) must beTrue
      isPalindrome(List(1)) must beTrue
      isPalindrome(List(1, 2, 1)) must beTrue
      isPalindrome(List(1, 2, 3)) must beFalse
      isPalindrome(List(1, 2, 5, 2, 1)) must beTrue
      isPalindrome(List(1, 2, 5, 7, 5, 2, 1)) must beTrue
    }
  }


  "The function flatten" should {
    "return a flattened version of the input list" in {
      flatten(List(List(1, 1), 2, List(3, List(5, 8)))) must be equalTo List(1, 1, 2, 3, 5, 8)
    }
  }

  "The function compress" should {
    "return a list with consecutive duplicates eliminated" in {
      compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must be equalTo List('a, 'b, 'c, 'a, 'd, 'e)
    }
  }

  "The function pack" should {
    "return a list of lists that contains all repeated elements in the source list" in {
      pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must be equalTo List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    }

  }
}
