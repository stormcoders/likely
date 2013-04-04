package br.com.igorbonadio.likely

import org.scalatest._
import org.scalatest.matchers._

class NormalDistributionSpec extends FlatSpec with ShouldMatchers {
  behavior of "A normal distribution"
  
  trait DistributionFixture {
    val distribution = new NormalDistribution(0, 1)
  }

  it should "get the probability of a given number" in new DistributionFixture {
    distribution.prob(2).expValue should be (0.0539 plusOrMinus 0.0001)
    distribution.prob(1).expValue should be (0.2419 plusOrMinus 0.0001)
    distribution.prob(-0.5).expValue should be (0.3520 plusOrMinus 0.0001)
  }

  it should "choose generate numbers according to the normal distribution" in new DistributionFixture {
    val randomNumbers = (1 to 10000).map(i => distribution.choose)
    val mean = randomNumbers.foldLeft(0.0) { (a, b) => a + b/randomNumbers.length }
    val sumOfSquares = randomNumbers.foldLeft(0.0){(a, b) => a + Math.pow(b - mean,2)}
    val sd = Math.sqrt(sumOfSquares/randomNumbers.length)
    mean should be (0.0 plusOrMinus 0.1)
    sd should be (1.0 plusOrMinus 0.1)
  }

  it should "be trained by a given sequence of numbers" in new DistributionFixture {
    val randomNumbers = (1 to 10000).map(i => distribution.choose)
    val trainedDistribution = NormalDistribution.train(randomNumbers.toStream)
    trainedDistribution.prob(2).expValue should be (0.0539 plusOrMinus 0.1)
    trainedDistribution.prob(1).expValue should be (0.2419 plusOrMinus 0.1)
    trainedDistribution.prob(-0.5).expValue should be (0.3520 plusOrMinus 0.1)
  }
}