package com.neosavvy.neophyte

import com.neosavvy.neophyte.lessonOne._
import org.specs2.mutable._

/**
 * Created by aparrish on 7/23/14.
 */
class Lesson1Extractors extends Specification {

  "The basic extractor for Free and Premium Users" should {
    "unapply for Free Users" in {
      val user = FreeUser.unapply(new FreeUser("Adam"))
      user must be equalTo Some("Adam")
    }

    "unapply for Premium Users" in {
      val pUser = PremiumUser.unapply(new PremiumUser("Dana"))
      pUser must be equalTo Some("Dana")
    }
  }

  "Pattern matching" should {
    "work for Premium Users" in {
      val pUser = new PremiumUser("Adam")
      val matched = pUser match {
        case FreeUser(name) => "Hello" + name
        case PremiumUser(name) => "Welcome back premium user: " + name
      }

      matched must be equalTo "Welcome back premium user: Adam"
    }
  }

  "Pattern matching on complex extractors" should {
    "still support matching" in {
      val faUser = new FreeAdvancedUser("Adam", 3000, 0.9d)
      val matched = faUser match {
        case FreeAdvancedUser(name, _, p) => if( p > 0.75) name + ", what can we do for you today?" else "Hello " + name
        case PremiumAdvancedUser(name, _) => "Welcome back, " + name
      }
      matched must be equalTo "Adam, what can we do for you today?"
    }
  }

  "Boolean pattern matching can work too" should {
    "check if a premium candidate is likely from a free user" in {
      val fUser : AdvancedUser = new FreeAdvancedUser("Adam", 2500, 0.8d)
      val isMatch = fUser match {
        case freeUser @ premiumCandidate() => "initiating the spam"
        case _ => "send the trash"
      }
      isMatch must be equalTo "initiating the spam"
    }
  }

  "Extractors for streams" should {
    "support destructuring with a match" in {
      val xs = 58 #:: 43 #:: 93 #:: Stream.empty
      val result = xs match {
        case first #:: second #:: _ => first - second
        case _ => -1
      }

      result must be equalTo (58 - 43)
    }

    "likewise the same destructuring should work with infix" in {
      val xs = 58 #:: 43 #:: 93 #:: Stream.empty
      val result = xs match {
        case #::(first, #::(second, _)) => first - second
        case _ => -1
      }

      result must be equalTo (58 - 43)
    }
  }

}