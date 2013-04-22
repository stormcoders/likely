package br.com.igorbonadio.likely

import org.scalatest._
import org.scalatest.matchers._

class HiddenMarkovModelSpec extends FlatSpec with ShouldMatchers {
  behavior of "A HMM"

  it should "evaluate the joint probability of a sequence of labels and a sequence of simbols" in {
    val states = Alphabet("Loaded", "Fair")
    val symbols = Alphabet(1 to 6)
    val emissions = Map(
      (symbols.id("1"), states.id("Fair")) -> Probability(1.0/6),
      (symbols.id("2"), states.id("Fair")) -> Probability(1.0/6),
      (symbols.id("3"), states.id("Fair")) -> Probability(1.0/6),
      (symbols.id("4"), states.id("Fair")) -> Probability(1.0/6),
      (symbols.id("5"), states.id("Fair")) -> Probability(1.0/6),
      (symbols.id("6"), states.id("Fair")) -> Probability(1.0/6),
      (symbols.id("1"), states.id("Loaded")) -> Probability(0.5),
      (symbols.id("2"), states.id("Loaded")) -> Probability(0.1),
      (symbols.id("3"), states.id("Loaded")) -> Probability(0.1),
      (symbols.id("4"), states.id("Loaded")) -> Probability(0.1),
      (symbols.id("5"), states.id("Loaded")) -> Probability(0.1),
      (symbols.id("6"), states.id("Loaded")) -> Probability(0.1)
    )
    val transitions = Map(
      (states.id("Fair"), states.id("Fair")) -> Probability(0.9),
      (states.id("Loaded"), states.id("Fair")) -> Probability(0.1),
      (states.id("Loaded"), states.id("Loaded")) -> Probability(0.05),
      (states.id("Fair"), states.id("Loaded")) -> Probability(0.95)
    )
    val x = symbols.generateSequeceOfIds(Stream("1", "2"))
    val y = states.generateSequeceOfIds(Stream("Fair", "Loaded"))
    val hmm = new HiddenMarkovModel(states, symbols, emissions, transitions)
    hmm.prob(x, y).expValue should be ((0.95*(1.0/6)*0.1*0.1) plusOrMinus 0.0001)
  }
}