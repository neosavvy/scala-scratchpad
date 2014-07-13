package com.neosavvy.ninetynineproblems

/**
 * Created by aparrish on 7/12/14.
 */
class Problems {

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
  def length[A]( list : List[A] ) : Int = lengthWithAcc( list, 0 )

  def lengthWithAcc[A]( list : List[A], acc : Int ) : Int = list match {
    case Nil => acc
    case h :: Nil => acc + 1
    case _ :: tail => lengthWithAcc( tail, acc + 1 )
  }
}
