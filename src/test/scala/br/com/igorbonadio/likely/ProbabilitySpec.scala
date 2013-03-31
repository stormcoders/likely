package br.com.igorbonadio.likely

import org.scalatest._
import org.scalatest.matchers._

class ProbabilitySpec extends FlatSpec with ShouldMatchers {
  behavior of "A probability"
  
  it should "store its log representation" in {
    (1 to 10).map {i => i/10.0}.foreach { p => 
      Probability(p).logValue should be (-math.log(p) plusOrMinus 0.0001)
    }
  }
  
  it should "show its normal [0,1] representation" in {
    (1 to 10).map {i => i/10.0}.foreach { p => 
      Probability(p).expValue should be (p plusOrMinus 0.0001)
    }
  }
  
  it should "multiply probabilities" in {
    (for (x <- 1 to 10; y <- 1 to 10) yield Pair(x/10.0, y/10.0)).foreach { p => p match {
      case Pair(x, y) => (Probability(x) * Probability(y)).logValue should be (Probability(x*y).logValue plusOrMinus 0.0001)
    }}
  }
  
  it should "divide probabilities" in {
    (for (x <- 1 to 10; y <- 1 to 10) yield Pair(x/10.0, y/10.0)).foreach { p => p match {
      case Pair(x, y) => (Probability(x) / Probability(y)).logValue should be (Probability(x/y).logValue plusOrMinus 0.0001)
    }}
  }
  
  it should "add probabilities" in {
    (for (x <- 1 to 10; y <- 1 to 10) yield Pair(x/10.0, y/10.0)).foreach { p => p match {
      case Pair(x, y) => (Probability(x) + Probability(y)).logValue should be (Probability(x+y).logValue plusOrMinus 0.0001)
    }}
  }
  
  it should "subtract probabilities" in {
    (Probability(0.5) - Probability(0.2)).logValue should be (Probability(0.3).logValue plusOrMinus 0.0001)
    (Probability(0.5) - Probability(0.5)).logValue should be (Probability(0.0).logValue plusOrMinus 0.0001)
    (Probability(0.9) - Probability(0.3)).logValue should be (Probability(0.6).logValue plusOrMinus 0.0001)
    (Probability(0.9) - Probability(0.0)).logValue should be (Probability(0.9).logValue plusOrMinus 0.0001)
    (Probability(0.1) - Probability(0.1)).logValue should be (Probability(0.0).logValue plusOrMinus 0.0001)
  }
  
  it should "have a string representation" in {
    Probability(0.5).toString should be === "0.5"
    Probability(0.2).toString should be === "0.2"
  }
}