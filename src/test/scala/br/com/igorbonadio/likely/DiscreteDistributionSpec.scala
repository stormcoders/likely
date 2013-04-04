package br.com.igorbonadio.likely

import org.scalatest._
import org.scalatest.matchers._

class DiscreteDistributionSpec extends FlatSpec with ShouldMatchers {
  behavior of "A discrete distribution"

  trait Casino {
    val alphabet = new Alphabet(List("Loaded", "Fair"))
    val distribution = new DiscreteDistribution(List(Probability(0.8), Probability(0.2)))
  }
  
  it should "get the probability of a given symbol" in new Casino {
    distribution.prob(alphabet.id("Loaded")).expValue should be (0.8 plusOrMinus 0.0001)
    distribution.prob(alphabet.id("Fair")).expValue should be (0.2 plusOrMinus 0.0001)
  }

  it should "choose a symbol" in  new Casino {
    distribution.choose should (be === 0 or be === 1)
  }

  it should "be trained by a stream of symbols" in new Casino {
    val d = DiscreteDistribution.train(
      alphabet.generateSequeceOfIds(Stream("Fair", "Fair", "Loaded", "Loaded", "Fair")))
    d.prob(alphabet.id("Loaded")).expValue should be (0.4 plusOrMinus 0.0001)
    d.prob(alphabet.id("Fair")).expValue should be (0.6 plusOrMinus 0.0001)
  }
}