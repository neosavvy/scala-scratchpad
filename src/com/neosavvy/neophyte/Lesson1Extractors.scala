package com.neosavvy.neophyte

package object lessonOne {

  trait User {
    def name: String
  }
  class FreeUser( val name : String ) extends User
  class PremiumUser( val name : String ) extends User

  object FreeUser {
    def unapply( user : FreeUser ) : Option[String] = Some(user.name)
  }

  object PremiumUser {
    def unapply( user : PremiumUser ) : Option[String] = Some(user.name)
  }

  trait AdvancedUser {
    def name: String
    def score : Int
  }

  class FreeAdvancedUser( val name:String, val score: Int, val upgradeProbability: Double ) extends AdvancedUser
  class PremiumAdvancedUser( val name:String, val score: Int) extends AdvancedUser

  object FreeAdvancedUser {
    def unapply( user : FreeAdvancedUser ) : Option[(String, Int, Double)] = Some((user.name, user.score, user.upgradeProbability))
  }

  object PremiumAdvancedUser {
    def unapply( user : PremiumAdvancedUser ) : Option[(String, Int)] = Some((user.name, user.score))
  }

  object premiumCandidate {
    def unapply(user: FreeAdvancedUser):Boolean = user.upgradeProbability > 0.75
  }
}
