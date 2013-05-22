package br.com.igorbonadio.likely

import org.scalatest._
import org.scalatest.matchers._

import Fancy._

class HiddenMarkovModelSpec extends FlatSpec with ShouldMatchers {
  behavior of "A HMM"

  it should "evaluate the joint probability of a sequence of labels and a sequence of simbols" in {
    val states = Alphabet("Loaded", "Fair")
    val symbols = Alphabet(1 to 6)
    val hmm = HiddenMarkovModel(symbols, states) { HMM =>
      HMM.transitions { Prob =>
        Prob("1" | "Loaded") is 50.0.%%
        Prob("2" | "Loaded") is 10.0.%%
        Prob("3" | "Loaded") is 10.0.%%
        Prob("4" | "Loaded") is 10.0.%%
        Prob("5" | "Loaded") is 10.0.%%
        Prob("6" | "Loaded") is 10.0.%%
        Prob("1" | "Fair") is 16.66.%%
        Prob("2" | "Fair") is 16.66.%%
        Prob("3" | "Fair") is 16.66.%%
        Prob("4" | "Fair") is 16.66.%%
        Prob("5" | "Fair") is 16.66.%%
        Prob("6" | "Fair") is 16.66.%%
      }
      HMM.emissions { Prob =>
        Prob("Loaded" | "Loaded") is 5.0.%%
        Prob("Fair"   | "Loaded") is 95.0.%%
        Prob("Loaded" | "Fair") is 10.0.%%
        Prob("Fair"   | "Fair") is 90.0.%%
      }
    }
    val x = symbols.generateSequeceOfIds(Stream("1", "2"))
    val y = states.generateSequeceOfIds(Stream("Fair", "Loaded"))
    hmm.prob(x, y).expValue should be ((0.95*(1.0/6)*0.1*0.1) plusOrMinus 0.0001)
  }
}
