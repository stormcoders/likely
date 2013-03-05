package br.com.igorbonadio.likely

import org.scalatest._
import org.scalatest.matchers._

class AlphabetSpec extends FlatSpec with ShouldMatchers {
  behavior of "A alphabet"
  
  it should "store a set of symbols" in {
    val casino = new Alphabet(List("Loaded", "Fair"))
    casino.alphabetMap.size should be === 2
  }
  
  it should "return the number of symbols" in {
    val casino = new Alphabet(List("Loaded", "Fair"))
    casino.size should be === 2
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
  
  it should "return id's symbol" in {
    val casino = new Alphabet(List("Loaded", "Fair"))
    casino.symbol(0) should be === "Loaded"
    casino.symbol(1) should be === "Fair"
  }
  
  it should "return an empty string if id is invalid" in {
    val casino = new Alphabet(List("Loaded", "Fair"))
    casino.symbol(2) should be === ""
  }
  
  it should "generate a sequence of ids" in {
    val casino = new Alphabet(List("Loaded", "Fair"))
    casino.generateSequeceOfSymbols(List(1,1, 0, 0, 1)) should be === List("Fair", "Fair", "Loaded", "Loaded", "Fair")
  }
  
  it should "generate a sequence of symbols" in {
    val casino = new Alphabet(List("Loaded", "Fair"))
    casino.generateSequeceOfIds(List("Fair", "Fair", "Loaded", "Loaded", "Fair")) should be === List(1,1, 0, 0, 1)
  }
}
