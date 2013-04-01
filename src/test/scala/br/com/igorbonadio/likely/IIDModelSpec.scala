package br.com.igorbonadio.likely

import org.scalatest._
import org.scalatest.matchers._

class DiscreteIIDModelSpec extends FlatSpec with ShouldMatchers {
  behavior of "A Discrete IID Model"

  trait IIDCasino {
    val alphabet = new Alphabet(List("Loaded", "Fair"))
    val distribution = new DiscreteDistribution(List(Probability(0.8), Probability(0.2)))
    val model = new DiscreteIIDModel(distribution)
  }

  it should "get the probability of a given stream" in new IIDCasino {
    model.prob(alphabet.generateSequeceOfIds(Stream("Fair", "Loaded"))).
      expValue should be (0.16 plusOrMinus 0.0001)
  }
  
  it should "generate a random symbol" in new IIDCasino {
    model.choose.head should (be === 0 or be === 1)
  }

  it should "generate a random stream" in new IIDCasino {
    model.choose.take(5).foreach(sym => sym should (be === 0 or be === 1))
  }
}

class ContinuousIIDModelSpec extends FlatSpec with ShouldMatchers {
  behavior of "A Continuous IID Model"

  trait IIDNormal {
    val distribution = new NormalDistribution(0, 1)
    val model = new ContinuousIIDModel(distribution)
  }

  it should "get the probability of a given stream" in new IIDNormal {
    model.prob(Stream(0.1, 0.2, 0.3, 0.4)).
      expValue should be (0.0218 plusOrMinus 0.0001)
  }
  
  it should "generate a random symbol" in new IIDNormal {
    model.choose.head should not be === (model.choose.head)
  }

  it should "generate a random stream" in new IIDNormal {
    model.choose.take(5).length should be === (5)
  }
  
}