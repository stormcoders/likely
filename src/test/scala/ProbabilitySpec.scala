import org.scalatest._
import org.scalatest.matchers._

class ProbabilitySpec extends FlatSpec with ShouldMatchers {
  behavior of "A probability"
  
  it should "store its log representation" in {
    Probability(0.5).logValue should be (-math.log(0.5) plusOrMinus 0.0001)
  }
  
  it should "show its normal [0,1] representation" in {
    Probability(0.5).value should be (0.5 plusOrMinus 0.0001)
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
}