package br.com.stormcoders.likely

import org.scalatest._
import org.scalatest.matchers._

import Fancy._

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

  it should "have a fancy way to be defined" in {
    50.0.%%.expValue should be (0.5 plusOrMinus 0.0001)
  }
  
  it should "multiply probabilities" in {
    (for (x <- 1 to 10; y <- 1 to 10 if x*y <= 100) yield Pair(x/10.0, y/10.0)).foreach { p => p match {
      case Pair(x, y) => (Probability(x) * Probability(y)).logValue should be (Probability(x*y).logValue plusOrMinus 0.0001)
    }}
  }
  
  it should "divide probabilities" in {
    (for (x <- 1 to 10; y <- 1 to 10 if x/y <= 1) yield Pair(x/10.0, y/10.0)).foreach { p => p match {
      case Pair(x, y) => (Probability(x) / Probability(y)).logValue should be (Probability(x/y).logValue plusOrMinus 0.0001)
    }}
  }
  
  it should "add probabilities" in {
    (for (x <- 1 to 10; y <- 1 to 10 if x + y <= 10) yield Pair(x/10.0, y/10.0)).foreach { p => p match {
      case Pair(x, y) => (Probability(x) + Probability(y)).logValue should be (Probability(x+y).logValue plusOrMinus 0.0001)
    }}
  }
  
  it should "subtract probabilities" in {
    (for (x <- 1 to 10; y <- 1 to 10 if x - y > 0) yield Pair(x/10.0, y/10.0)).foreach { p => p match {
      case Pair(x, y) => (Probability(x) - Probability(y)).logValue should be (Probability(x-y).logValue plusOrMinus 0.0001)
    }}
  }
  
  it should "have a string representation" in {
    (1 to 20).map {i => i/20.0}.foreach {p => 
      Probability(p).toString.toDouble should be (p plusOrMinus 0.0001)
    }
  }

  it should "check if a probability is equal to other" in {
    (50.0.%% == 50.0.%%) should be === true
    (50.0.%% == 40.0.%%) should be === false
  }

  it should "check if a probability is greater than other" in {
    (50.0.%% > 50.0.%%) should be === false
    (50.0.%% > 40.0.%%) should be === true
  }

  it should "check if a probability is greater than or equal to other" in {
    (50.0.%% >= 60.0.%%) should be === false
    (50.0.%% >= 50.0.%%) should be === true
    (50.0.%% >= 40.0.%%) should be === true
  }

  it should "check if a probability is less than other" in {
    (50.0.%% < 60.0.%%) should be === true
    (50.0.%% < 40.0.%%) should be === false
  }

  it should "check if a probability is less than or equal to other" in {
    (50.0.%% <= 60.0.%%) should be === true
    (50.0.%% <= 50.0.%%) should be === true
    (50.0.%% <= 40.0.%%) should be === false
  }
}
