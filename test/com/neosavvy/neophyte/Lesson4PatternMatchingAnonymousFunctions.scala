package com.neosavvy.neophyte

import org.specs2.mutable._

/**
 * Created by aparrish on 7/23/14.
 */
class Lesson4PatternMatchingAnonymousFunctions extends Specification {

  val wordFrequencies = ("habitual", 6) ::
    ("and", 56) ::
    ("consuetudinary", 2) ::
    ("additionally", 27) ::
    ("homely", 5) ::
    ("society", 13) ::
    Nil


  "Simple anonymous function" should {
    "allow me to define inline" in {
      val songTitles = List("The White Hare", "Childe the Hunter", "Take no Rogues")
      val r = songTitles.map(t => {
        t.toLowerCase
      })

      r must be equalTo "the white hare" :: "childe the hunter" :: "take no rogues" :: Nil
    }

    "even shorter form" in {
      val songTitles = List("The White Hare", "Childe the Hunter", "Take no Rogues")
      val r = songTitles.map(_.toLowerCase)
      r must be equalTo "the white hare" :: "childe the hunter" :: "take no rogues" :: Nil
    }
  }

  "Simple filter" should {
    def wordsWithoutOutliers(wordFrequencies: Seq[(String, Int)]): Seq[String] =
      wordFrequencies.filter(wf => wf._2 > 3 && wf._2 < 25).map(_._1)

    "verify" in {
      wordsWithoutOutliers(wordFrequencies) must be equalTo "habitual" :: "homely" :: "society":: Nil
    }
  }

  "Improved Filter" should {
    def wordsWithoutOutliers(wordFrequencies: Seq[(String, Int)]): Seq[String] =
      wordFrequencies.filter { case (_, f) => f > 3 && f < 25 } map { case (w, _) => w }

    "verify" in {
      wordsWithoutOutliers(wordFrequencies) must be equalTo "habitual" :: "homely" :: "society":: Nil
    }
  }

  "More improvements" should {
    val predicate: ((String, Int)) => Boolean = { case (_, f) => f > 3 && f < 25 }
    val transformFn: ((String, Int)) => String = { case (w, _) => w }

    def wordsWithoutOutliers(wordFrequencies: Seq[(String, Int)]): Seq[String] =
      wordFrequencies.filter( predicate ) map ( transformFn )

    "verify" in {
      wordsWithoutOutliers(wordFrequencies) must be equalTo "habitual" :: "homely" :: "society":: Nil
    }
  }

  "Partial Fun" should {
    val pf: PartialFunction[(String, Int), String] = {
      case (word, freq) if freq > 3 && freq < 25 => word
    }

    val pf1 = new PartialFunction[(String, Int), String] {
      def apply(wordFrequency: (String, Int)) = wordFrequency match {
        case (word, freq) if freq > 3 && freq < 25 => word
      }
      def isDefinedAt(wordFrequency: (String, Int)) = wordFrequency match {
        case (word, freq) if freq > 3 && freq < 25 => true
        case _ => false
      }
    }

    "pf should return a properly filtered and mapped result with collect" in {
      val r = wordFrequencies.collect(pf)
      r.length must be equalTo(3)
    }

    "pf1 should work just like pf" in {
      val r = wordFrequencies.collect(pf1)
      r.length must be equalTo(3)
    }

    "The previously defined map / filter approach can be rewritten as a collect" in {

      def wordsWithoutOutliers(wordFrequencies: Seq[(String, Int)]): Seq[String] =
        wordFrequencies.collect { case (word, freq) if freq > 3 && freq < 25 => word }

      wordsWithoutOutliers(wordFrequencies) must be equalTo "habitual" :: "homely" :: "society":: Nil
    }
  }

}
