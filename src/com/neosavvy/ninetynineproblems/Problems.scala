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

  /**
   * Eliminate consecutive duplicates of list elements.
   */
  def compress[T]( l : List[T] ) : List[T] = {
    def compressStep( remaining : List[T], prev : Option[T], result : List[T] ) : List[T] = remaining match {
      case Nil => result reverse
      case head :: tail => {
        val p = prev.getOrElse(None)
        if( p == head ) {
          compressStep(tail, Some(head), result )
        }
        else {
          compressStep(tail, Some(head), head :: result )
        }
      }
    }
    compressStep( l, None, List())
  }

  /**
   * Pack consecutive duplicates of list elements into sublists.
   */
  def eliminateDupes[T]( l : List[T] ) : Any = {
    val m = scala.collection.mutable.Map[T, List[T]]()
    def f( x : T ) = {
      if( m.contains(x) )
       m.put(x, x :: m(x))
      else
        m.put(x, x :: Nil)
    }
    l map ( f(_) )
    m.values
  }

  /**
   * Takes a list of symbols and makes sublists out of the identical values
   */
  def pack[T]( l : List[T] ) : List[List[T]] = {
    def packStep[T]( remaining : List[T], result : List[List[T]], prev : Option[T], currentStep : List[T] ) : List[List[T]] = remaining match {
      case Nil => {
        currentStep :: result reverse
      }
      case head :: tail => {
        val p = prev.getOrElse(None)
        if( p == head ) {
          packStep(tail, result, Some(head), head :: currentStep)
        } else {
          currentStep match {
            case Nil =>packStep(tail, result, Some(head), head :: Nil )
            case _ =>packStep(tail, currentStep :: result, Some(head), head :: Nil )
          }
        }
      }
    }

    packStep( l, Nil, None, Nil)
  }
}
