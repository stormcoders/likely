package br.com.igorbonadio.likely

import org.scalatest._
import org.scalatest.matchers._

class BayesianClassifierSpec extends FlatSpec with ShouldMatchers {
  behavior of "A Bayesian Classifier"

  it should "choose between classes" in {
    val alphabet = Alphabet("Loaded", "Fair")
    
    val classifier = BayesianClassifier(
      "model1" -> new DiscreteIIDModel(
        new DiscreteDistribution(List(Probability(0.8), Probability(0.2)))
      ),
      "model2" -> new DiscreteIIDModel(
        new DiscreteDistribution(List(Probability(0.2), Probability(0.8)))
      )
    )

    classifier.classify(alphabet.generateSequeceOfIds(Stream("Fair", "Loaded", "Fair")))._1 should be === "model2"
    classifier.classify(alphabet.generateSequeceOfIds(Stream("Fair", "Loaded", "Loaded")))._1 should be === "model1"
  }

}