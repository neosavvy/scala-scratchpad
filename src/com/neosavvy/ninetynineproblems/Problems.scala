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

  /**
   * Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
   */
  def encode[T]( l : List[T] ) : List[(Int, T)] = {
    def encodeStep[T]( remaining : List[List[T]], result : List[(Int, T)] ) : List[(Int, T)] = remaining match {
      case Nil => result reverse
      case head :: tail => {
        val tuple = ( head.length, head(0))
        val step =  tuple :: result
        encodeStep( tail, step )
      }
    }

    encodeStep( pack(l), Nil)
  }

  /**
   * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
   */
  def encodeModified[T]( l : List[T] ) : List[Any] = {
    def encodeStep[T]( remaining : List[List[T]], result : List[Any] ) : List[Any] = remaining match {
      case Nil => result reverse
      case head :: tail => {
        if( head.length > 1 )
          encodeStep( tail, ( head.length, head(0)) :: result)
        else
          encodeStep( tail, head(0) :: result)
      }
    }

    encodeStep( pack(l), Nil)
  }

  def decode[T]( l : List[(Int, T)] ) : List[Any] = {
    def decodeTuple[T]( num : Int, symbol : T, result : List[T] ) : List[Any] = {
      if( num == 0 ) result
      else decodeTuple( num - 1, symbol, symbol :: result)
    }

    def decodeStep[T]( remaining : List[(Int, T)], result : List[T] ) : List[Any] = remaining match {

      case Nil => {
        result reverse
      }
      case head :: tail => {
        decodeStep( tail, decodeTuple( head._1, head._2, Nil ) ++ result )
      }

    }

    decodeStep[T]( l, List[T]())
  }
}
