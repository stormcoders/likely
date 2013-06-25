package br.com.stormcoders.likely

import org.scalatest._
import org.scalatest.matchers._

class AlphabetSpec extends FlatSpec with ShouldMatchers {
  behavior of "A alphabet"

  trait Casino {
    val casino = Alphabet("Loaded", "Fair")
  }
  
  it should "store a set of symbols" in new Casino {
    casino.alphabetMap.size should be === 2
  }

  it should "have a string representation" in new Casino {
    casino.toString should be === "Alphabet(Loaded, Fair)"
  }
  
  it should "return the number of symbols" in new Casino {
    casino.size should be === 2
  }
  
  it should "return symbol's id " in new Casino {
    casino.id("Loaded") should be === 0
    casino.id("Fair") should be === 1
  }
  
  it should "return an invalid id if symbol doesn't exist " in new Casino {
    casino.id("Invalid") should be === -1
  }
  
  it should "return id's symbol" in new Casino {
    casino.symbol(0) should be === "Loaded"
    casino.symbol(1) should be === "Fair"
  }
  
  it should "return an empty string if id is invalid" in new Casino {
    casino.symbol(2) should be === ""
  }

  it should "generate a stream of symbols" in new Casino {
    casino.generateSequeceOfSymbols(Stream(1,1, 0, 0, 1)).toList should be === List("Fair", "Fair", "Loaded", "Loaded", "Fair")
  }

  it should "generate a stream of ids" in new Casino {
    casino.generateSequeceOfIds(Stream("Fair", "Fair", "Loaded", "Loaded", "Fair")).toList should be === List(1,1, 0, 0, 1)
  }

  it should "be defined by a range (int)" in {
    val alphabet = Alphabet(1 to 10)
    alphabet.size should be === 10
  }

  it should "be defined by a range (char)" in {
    val alphabet = Alphabet('A' to 'Z')
    alphabet.size should be === 26
  }
}
