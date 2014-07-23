package com.neosavvy.neophyte

import com.neosavvy.neophyte.lessonTwo._
import org.specs2.mutable._

/**
 * Created by aparrish on 7/23/14.
 */
class Lesson2ExtractingSequencesSpec extends Specification {

  def greetWithFirstName(name: String) = name match {
    case GivenNames(firstName, _*) => "Good morning, " + firstName + "!"
    case _ => "Welcome! Please make sure to fill in your name!"
  }

  "The given name unapplySeq test" should {
    "return a string with the first name" in {
      greetWithFirstName("Adam") must be equalTo ("Good morning, Adam!")
    }
    "return a string with the first name when full name is provided" in {
      greetWithFirstName("Adam Parrish") must be equalTo ("Good morning, Adam!")
    }
    "return a string with a generic message" in {
      greetWithFirstName("") must be equalTo ("Welcome! Please make sure to fill in your name!")
    }
  }

  def greet(fullName: String) = {
    fullName match {
      case Names(lastName, firstName, _*) => "Good morning, " + firstName + " " + lastName + "!"
      case _ => "Welcome! Please make sure to fill in your name!"
    }
  }

  "The Names approach should" should {
    "properly extract the first and last names" in {
      greet("William Adam Parrish") must be equalTo "Good morning, William Parrish!"
    }
  }

}
