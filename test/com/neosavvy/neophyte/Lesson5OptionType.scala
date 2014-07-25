package com.neosavvy.neophyte

import com.neosavvy.neophyte.five._
import com.neosavvy.neophyte.five.lessonFive._
import org.specs2.mutable._

/**
 * Created by aparrish on 7/25/14.
 */
class Lesson5OptionType extends Specification {

  "simple greeting" should {
    "return hello world" in {
      lessonFive.greeting must be equalTo Some("Hello world")
    }
  }

  "simple empty" should {
    "return a value of None" in {
      lessonFive.emptyGreeting must be equalTo None
    }
  }

  "Java wrappers" should {
    "return none for null" in {
      lessonFive.absentGreeting must be equalTo None
    }
    "return a value for a string" in {
      lessonFive.presentGreeting must be equalTo Some("Here")
    }
  }

  "Simple Repository should return users" should {
    "return by id" in {
      val user1 = UserRepository.findById(1)
      ( user1.isDefined must be equalTo true ) and ( user1.get.firstName must be equalTo "John")
    }
  }

  "Simple pattern matching with Options" should {
    "allow you to tell if a value is None or Some value" in {
      val user = User(2, "Johanna", "Doe", 30, None)
      val result = user.gender match {
        case Some(gender) => "Gender: " + gender
        case None => "Gender: not specified"
      }

      result must be equalTo "Gender: not specified"
    }

    "return the age of an object from repository" in {
      val age = UserRepository.findById(1).map(_.age)
      age must be equalTo Some(32)
    }

    "return the option of gender using flatMap vs map" in {
      val gender1 = UserRepository.findById(1).flatMap(_.gender)
      gender1 must be equalTo Some("male")
    }
  }
}
