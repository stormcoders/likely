package br.com.igorbonadio.likely.examples.language_classifier

import org.scalatest._
import org.scalatest.matchers._

class LanguageClassifierSpec extends FlatSpec with ShouldMatchers {
  behavior of "A Language Classifier"

  trait PortugueseEnglishClassifier {
    val classifier = new LanguageClassifier
  }

  it should "classify portuguese texts" in new PortugueseEnglishClassifier {
    classifier.classify("Pequena, eu te amo!", false) should be === "pt"
    classifier.classify("Hoje comi peixei.", false) should be === "pt"
    classifier.classify("Nao posso perder a novela hoje!!!", false) should be === "pt"
    classifier.classify("Nao existe lugar como nossa casa...", false) should be === "pt"
    classifier.classify("Isso funciona! Incrivel!!!", false) should be === "pt"
  }

  it should "classify english texts" in new PortugueseEnglishClassifier {
    classifier.classify("Baby, I love you!", false) should be === "en"
    classifier.classify("Today, I ate fish.", false) should be === "en"
    classifier.classify("I cant miss the today's episode of the soap opera!!!", false) should be === "en"
    classifier.classify("There is no place like home", false) should be === "en"
    classifier.classify("It works! Awesome!!!", false) should be === "en"
  }
}
