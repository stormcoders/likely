package br.com.igorbonadio.likely

import org.scalatest._
import org.scalatest.matchers._

class BayesClassifierSpec extends FlatSpec with ShouldMatchers {
  behavior of "A Bayes Classifier"

  it should "choose between classes" in {
    val alphabet = Alphabet("Loaded", "Fair")
    
    val distribution1 = new DiscreteDistribution(List(Probability(0.8), Probability(0.2)))
    val model1 = new DiscreteIIDModel(distribution1)

    val distribution2 = new DiscreteDistribution(List(Probability(0.2), Probability(0.8)))
    val model2 = new DiscreteIIDModel(distribution2)

    val classifier = new BayesClassifier(Map(
      "model1" -> model1,
      "model2" -> model2
    ))

    classifier.classify(alphabet.generateSequeceOfIds(Stream("Fair", "Loaded", "Fair")))._1 should be === "model2"
    classifier.classify(alphabet.generateSequeceOfIds(Stream("Fair", "Loaded", "Loaded")))._1 should be === "model1"
  }

}