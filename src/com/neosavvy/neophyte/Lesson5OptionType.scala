package com.neosavvy.neophyte.five

/**
 * Created by aparrish on 7/25/14.
 */
package object lessonFive {

  /*
   * Internal Scala options
   */
  val greeting: Option[String] = Some("Hello world")
  val emptyGreeting : Option[String] = None

  /**
   * Useful when grabbing values from Java where
   * Option is not a concept
   */
  val absentGreeting : Option[String] = Option(null)
  val presentGreeting : Option[String] = Option("Here")


  /**
   * Simple Repository============================================================
   */
  case class User(
                   id: Int,
                   firstName: String,
                   lastName: String,
                   age: Int,
                   gender: Option[String])

  object UserRepository {
    private val users = Map(1 -> User(1, "John", "Doe", 32, Some("male")),
      2 -> User(2, "Johanna", "Doe", 30, None))
    def findById(id: Int): Option[User] = users.get(id)
    def findAll = users.values
  }
  /**
   * End Simple Repository============================================================
   */


}
