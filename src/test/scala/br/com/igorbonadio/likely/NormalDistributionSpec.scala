package br.com.igorbonadio.likely

import org.scalatest._
import org.scalatest.matchers._

class NormalDistributionSpec extends FlatSpec with ShouldMatchers {
  behavior of "A normal distribution"
  
  it should "get the probability of a given symbol" in {
    val distribution = new NormalDistribution(0, 1)
    distribution.prob(2).expValue should be (0.0539 plusOrMinus 0.0001)
    distribution.prob(1).expValue should be (0.2419 plusOrMinus 0.0001)
    distribution.prob(-0.5).expValue should be (0.3520 plusOrMinus 0.0001)
  }

  it should "choose a symbol" in {
    val distribution = new NormalDistribution(0, 1)
    
    distribution.choose should not be === (distribution.choose)
  }
}