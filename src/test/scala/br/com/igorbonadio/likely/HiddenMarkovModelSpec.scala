package br.com.igorbonadio.likely

import org.scalatest._
import org.scalatest.matchers._

import Fancy._

class HiddenMarkovModelSpec extends FlatSpec with ShouldMatchers {
  behavior of "A HMM"

  it should "evaluate the joint probability of a sequence of labels and a sequence of simbols" in {
    val states = Alphabet("Loaded", "Fair")
    val symbols = Alphabet(1 to 6)
    val emissions = Map(
      states.id("Loaded") -> DiscreteDistribution(symbols) { Prob =>
                              Prob("1") is 50.0.%%
                              Prob("2") is 10.0.%%
                              Prob("3") is 10.0.%%
                              Prob("4") is 10.0.%%
                              Prob("5") is 10.0.%%
                              Prob("6") is 10.0.%%
                            },
      states.id("Fair") ->   DiscreteDistribution(symbols) { Prob =>
                              Prob("1") is 16.66.%%
                              Prob("2") is 16.66.%%
                              Prob("3") is 16.66.%%
                              Prob("4") is 16.66.%%
                              Prob("5") is 16.66.%%
                              Prob("6") is 16.66.%%
                            }
    )
    val transitions = Map(
      states.id("Loaded") -> DiscreteDistribution(5.0.%%, 95.0.%%),
      states.id("Fair") -> DiscreteDistribution(10.0.%%, 90.0.%%)
    )
    val x = symbols.generateSequeceOfIds(Stream("1", "2"))
    val y = states.generateSequeceOfIds(Stream("Fair", "Loaded"))
    val hmm = new HiddenMarkovModel(emissions, transitions)
    hmm.prob(x, y).expValue should be ((0.95*(1.0/6)*0.1*0.1) plusOrMinus 0.0001)
  }
}