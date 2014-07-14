package com.neosavvy.ninetynineproblems

/**
 * My attempt at solving 99 scala problems: http://aperiodic.net/phil/scala/s-99/
 *
 * Created by aparrish on 7/12/14.
 */
object Problems {

  /**
   * Find the last element of a list.
   */
  def last( list : List[Int] ):Int = list(list.length - 1)

  /**
   * Find the last but one element of a list.
   */
  def penultimate( list : List[Int] ) : Int = list(list.length - 2)

  /**
   * Find the Nth element of a list.
   */
  def nth( nth : Int , list : List[Int] ) : Int = list(nth)

  /**
   * Find the number of elements of a list.
   */
  def length[A]( list : List[A] ) : Int = {
    def lengthWithAcc[A]( list : List[A], acc : Int ) : Int = list match {
      case Nil => acc
      case _ :: tail => lengthWithAcc( tail, acc + 1 )
    }
    lengthWithAcc( list, 0 )
  }

  /**
   * Reverse a list.
   */
  def reverse[A]( list : List[A] ) : List[A] = {
    def reverseWithAcc[A]( ls : List[A], reversed : List[A] ) : List[A] = ls match {
      case Nil => reversed
      case h :: tail => reverseWithAcc( tail ,  h :: reversed )
    }
    reverseWithAcc( list, List() )
  }

  /**
   * Find out if a list is a palindrome
   */
  def isPalindrome[A]( list : List[A] ) : Boolean = {
    if( list == Nil || list.length == 1) {
      return true;
    }

    if( list.head == list.last ) {
      isPalindrome( list.take( list.length - 1 ).tail )
    } else {
      false
    }

  }

  def flatten(l: List[_]): List[Any] = l match {
    case Nil => Nil
    case (head: List[_]) :: tail => flatten(head) ::: flatten(tail)
    case head :: tail => head :: flatten(tail)
  }


}
