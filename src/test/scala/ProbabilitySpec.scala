import org.scalatest._
import org.scalatest.matchers._

class ProbabilitySpec extends FlatSpec with ShouldMatchers {
  behavior of "A probability"
  
  it should "store its log representation" in {
    Prob(0.5).logValue should be (-math.log(0.5) plusOrMinus 0.0001)
  }
  
  it should "show its normal [0,1] representation" in {
    Prob(0.5).value should be (0.5 plusOrMinus 0.0001)
  }
  
  it should "multiply probabilities" in {
    (Prob(0.5) * Prob(0.2)).logValue should be (-math.log(0.10) plusOrMinus 0.0001)
  }
  
  it should "divide probabilities" in {
    (Prob(0.5) / Prob(0.5)).logValue should be (-math.log(1) plusOrMinus 0.0001)
  }
  
  it should "add probabilities" in (pending)
  it should "subtract probabilities" in (pending)
}