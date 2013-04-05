package br.com.igorbonadio.likely.examples

import org.scalatest._
import org.scalatest.matchers._

class LanguageSpec extends FlatSpec with ShouldMatchers {
  behavior of "A Language Classifier"

  it should "classify portuguese texts" in {
    Language.classify("Pequena, eu te amo!", false) should be === "pt"
    Language.classify("Hoje comi peixei.", false) should be === "pt"
    Language.classify("Nao posso perder a novela hoje!!!", false) should be === "pt"
    Language.classify("Nao existe lugar como nossa casa...", false) should be === "pt"
    Language.classify("Isso funciona! Incrivel!!!", false) should be === "pt"
  }
  it should "classify english texts" in {
    Language.classify("Baby, I love you!", false) should be === "en"
    Language.classify("Today, I ate fish.", false) should be === "en"
    Language.classify("I cant miss the today's episode of the soap opera!!!", false) should be === "en"
    Language.classify("There is no place like home", false) should be === "en"
    Language.classify("It works! Awesome!!!", false) should be === "en"
  }
}