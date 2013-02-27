import org.scalatest._
import org.scalatest.matchers._

class AlphabetSpec extends FlatSpec with ShouldMatchers {
  behavior of "A alphabet"
  
  it should "store a set of symbols" in {
    val casino = new Alphabet(List("Loaded", "Fair"))
    casino.alphabetMap.size should be === 2
  }
  
  it should "return symbol's id " in {
    val casino = new Alphabet(List("Loaded", "Fair"))
    casino.id("Loaded") should be === 0
    casino.id("Fair") should be === 1
  }
  
  it should "return an invalid id if symbol doesn't exist " in {
    val casino = new Alphabet(List("Loaded", "Fair"))
    casino.id("Invalid") should be === -1
  }
  
  it should "return id's symbol" in (pending)
  it should "generate a sequence of ids" in (pending)
  it should "generate a sequence of symbols" in (pending)
}