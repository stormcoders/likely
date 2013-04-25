package br.com.igorbonadio.likely

import org.scalatest._
import org.scalatest.matchers._

import Fancy._

class BayesianClassifierSpec extends FlatSpec with ShouldMatchers {
  behavior of "A Bayesian Classifier"

  it should "choose between classes" in {
    val alphabet = Alphabet("Loaded", "Fair")
    
    val classifier = BayesianClassifier(
      "model1" -> DiscreteIIDModel(80.0.%%, 20.0.%%),
      "model2" -> DiscreteIIDModel(20.0.%%, 80.0.%%)
    )

    classifier.classify(alphabet.generateSequeceOfIds(Stream("Fair", "Loaded", "Fair")))._1 should be === "model2"
    classifier.classify(alphabet.generateSequeceOfIds(Stream("Fair", "Loaded", "Loaded")))._1 should be === "model1"
  }

}