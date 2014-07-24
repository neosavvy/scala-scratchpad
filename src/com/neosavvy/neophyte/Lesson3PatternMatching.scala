package com.neosavvy.neophyte

/**
 * Created by aparrish on 7/23/14.
 */
package object lessonThree {

  case class Player(name: String, score: Int)

  def message(player: Player) = player match {
    case Player(_, score) if score > 100000 => "Get a job, dude!"
    case Player(name, _) => "Hey " + name + ", nice to see you again!"
  }

  def printMessage(player: Player) = println( message(player) )

  def currentPlayer(): Player = Player("Adam", 3500)

  def gameResult() : (String, Int) = ("Adam", 3500)

  def gameResults(): Seq[(String, Int)] =
    ("Daniel", 3500) :: ("Melissa", 13000) :: ("John", 7000) :: Nil

  def hallOfFame = for {
    (name, score) <- gameResults()
    if (score > 5000)
  } yield name




}
