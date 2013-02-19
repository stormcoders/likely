import org.scalatest._
import org.scalatest.matchers._

class ProbabilitySpec extends FlatSpec with ShouldMatchers {
  behavior of "A probability"
  
  it should "store its log representation" in {
    Probability(1.0).logValue should be (-math.log(1.0) plusOrMinus 0.0001)
    Probability(0.9).logValue should be (-math.log(0.9) plusOrMinus 0.0001)
    Probability(0.8).logValue should be (-math.log(0.8) plusOrMinus 0.0001)
    Probability(0.7).logValue should be (-math.log(0.7) plusOrMinus 0.0001)
    Probability(0.6).logValue should be (-math.log(0.6) plusOrMinus 0.0001)
    Probability(0.5).logValue should be (-math.log(0.5) plusOrMinus 0.0001)
    Probability(0.4).logValue should be (-math.log(0.4) plusOrMinus 0.0001)
    Probability(0.3).logValue should be (-math.log(0.3) plusOrMinus 0.0001)
    Probability(0.2).logValue should be (-math.log(0.2) plusOrMinus 0.0001)
    Probability(0.1).logValue should be (-math.log(0.1) plusOrMinus 0.0001)
  }
  
  it should "show its normal [0,1] representation" in {
    Probability(1.0).expValue should be (1.0 plusOrMinus 0.0001)
    Probability(0.9).expValue should be (0.9 plusOrMinus 0.0001)
    Probability(0.8).expValue should be (0.8 plusOrMinus 0.0001)
    Probability(0.7).expValue should be (0.7 plusOrMinus 0.0001)
    Probability(0.6).expValue should be (0.6 plusOrMinus 0.0001)
    Probability(0.5).expValue should be (0.5 plusOrMinus 0.0001)
    Probability(0.4).expValue should be (0.4 plusOrMinus 0.0001)
    Probability(0.3).expValue should be (0.3 plusOrMinus 0.0001)
    Probability(0.2).expValue should be (0.2 plusOrMinus 0.0001)
    Probability(0.1).expValue should be (0.1 plusOrMinus 0.0001)
  }
  
  it should "multiply probabilities" in {
    (Probability(0.5) * Probability(0.2)).logValue should be (Probability(0.10).logValue plusOrMinus 0.0001)
  }
  
  it should "divide probabilities" in {
    (Probability(0.5) / Probability(0.5)).logValue should be (Probability(1).logValue plusOrMinus 0.0001)
  }
  
  it should "add probabilities" in {
    (Probability(0.5) + Probability(0.2)).logValue should be (Probability(0.7).logValue plusOrMinus 0.0001)
  }
  
  it should "subtract probabilities" in {
    (Probability(0.5) - Probability(0.2)).logValue should be (Probability(0.3).logValue plusOrMinus 0.0001)
  }
  
  it should "have a string representation" in {
    Probability(0.5).toString should be === "0.5"
  }
}