package br.com.igorbonadio.likely.examples.language_classifier

import org.scalatest._
import org.scalatest.matchers._

class LanguageClassifierSpec extends FlatSpec with ShouldMatchers {
  behavior of "A Language Classifier"

  trait PortugueseEnglishClassifier {
    val classifier = new LanguageClassifier
  }

  it should "classify portuguese texts" in new PortugueseEnglishClassifier {
    classifier.classify("Pequena, eu te amo!")._1 should be === "portuguese"
    classifier.classify("Hoje comi peixei.")._1 should be === "portuguese"
    classifier.classify("Nao posso perder a novela hoje!!!")._1 should be === "portuguese"
    classifier.classify("Nao existe lugar como nossa casa...")._1 should be === "portuguese"
    classifier.classify("Isso funciona! Incrivel!!!")._1 should be === "portuguese"
  }

  it should "classify english texts" in new PortugueseEnglishClassifier {
    classifier.classify("Baby, I love you!")._1 should be === "english"
    classifier.classify("Today, I ate fish.")._1 should be === "english"
    classifier.classify("I cant miss the today's episode of the soap opera!!!")._1 should be === "english"
    classifier.classify("There is no place like home")._1 should be === "english"
    classifier.classify("It works! Awesome!!!")._1 should be === "english"
  }
}
