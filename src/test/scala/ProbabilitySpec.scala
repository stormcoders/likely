import org.scalatest._

class ProbabilitySpec extends FlatSpec {
  behavior of "A probability"
  
  it should "store its log representation" in {
    assert(Prob(0.5).logValue === math.log(0.5))
  }
  
  it should "show its normal [0,1] representation" in {
    assert(Prob(0.5).value === 0.5)
  }
  
  it should "multiply probabilities" in {
    assert((Prob(0.5) * Prob(0.2)).logValue === math.log(0.10))
  }
  
  it should "divide probabilities" in (pending)
  it should "add probabilities" in (pending)
  it should "subtract probabilities" in (pending)
}