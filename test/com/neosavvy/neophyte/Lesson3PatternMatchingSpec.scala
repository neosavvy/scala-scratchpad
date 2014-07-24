package com.neosavvy.neophyte

import com.neosavvy.neophyte.lessonThree._
import org.specs2.mutable.Specification

/**
 * Created by aparrish on 7/23/14.
 */
class Lesson3PatternMatchingSpec extends Specification {

  "The message" should {
    "be returned for a low score player" in {
      message(Player("Adam", 110000)) must be equalTo "Get a job, dude!"
    }

    "be returned for a non low score" in {
      message(Player("Adam", 0)) must be equalTo "Hey Adam, nice to see you again!"
    }
  }

  "The player" should {
    "be returned from current player" in {
      val player = currentPlayer()
      player.name must be equalTo "Adam"
    }

    "be returned in a destructed way" in {
      val Player(name, _) = currentPlayer()
      name must be equalTo "Adam"
    }

    "example list" in {
      def scores: List[Int] = List()
      val best :: rest = scores
      best must be equalTo 0
    }.pendingUntilFixed()

    "game result" in {
      val (name, score) = gameResult()
      (name must be equalTo "Adam") and (score must be equalTo 3500)
    }

    "game results" in {
      val r = hallOfFame
      r.foreach(println(_))
      r.length must be equalTo 2
    }

    "simple for comprehension" in {
      val lists = List(1, 2, 3) :: List.empty :: List(5, 3) :: Nil
      val s = for {
        list @ head :: _ <- lists
      } yield list.size
      s must be equalTo 3 :: 2 :: Nil
    }
  }

}
