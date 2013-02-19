import org.scalatest._

class ProbabilitySpec extends FlatSpec {
  behavior of "A probability"
  
  it should "store its log representation" in {
    val p = new Probability(0.5)
    p.logValue == math.log(0.5)
  }
  
  it should "print its normal [0,1] representation" in (pending)
  it should "multiply probabilities" in (pending)
  it should "divide probabilities" in (pending)
  it should "add probabilities" in (pending)
  it should "subtract probabilities" in (pending)
}