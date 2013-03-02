import org.scalatest._
import org.scalatest.matchers._

class DiscreteDistributionSpec extends FlatSpec with ShouldMatchers {
  behavior of "A discrete distribution"
  
  it should "be initialized by a vector of probabilities" in {
    val distribution = new DiscreteDistribution(List(0.8, 0.2))
  }
  
  it should "get the probability of a given symbol" in {
    //val alphabet = new Alphabet(List("Loaded", "Fair"))
    //val distribution = new DiscreteDistribution(alphabet, List(0.8, 0.2))
    //distribution.prob(alphabet.id("Loaded")) should be === 0.8
    //distribution.prob(alphabet.id("Fair")) should be === 0.2
  }
}