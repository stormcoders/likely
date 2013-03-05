package br.com.igorbonadio.likely

import org.scalatest._
import org.scalatest.matchers._

class DiscreteDistributionSpec extends FlatSpec with ShouldMatchers {
  behavior of "A discrete distribution"
  
  it should "get the probability of a given symbol" in {
    val alphabet = new Alphabet(List("Loaded", "Fair"))
    val distribution = new DiscreteDistribution(List(Probability(0.8), Probability(0.2)))
    distribution.prob(alphabet.id("Loaded")).expValue should be (0.8 plusOrMinus 0.0001)
    distribution.prob(alphabet.id("Fair")).expValue should be (0.2 plusOrMinus 0.0001)
  }
}