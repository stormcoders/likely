package br.com.igorbonadio.likely

import org.scalatest._
import org.scalatest.matchers._

class HiddenMarkovModelSpec extends FlatSpec with ShouldMatchers {
  behavior of "A HMM"

  it should "evaluate the joint probability of a sequence of labels and a sequence of simbols" in {
    val states = Alphabet("Loaded", "Fair")
    val symbols = Alphabet(1 to 6)
    val emissions = Map(
      states.id("Loaded") -> DiscreteDistribution(Probability(0.5), 
                                                  Probability(0.1), 
                                                  Probability(0.1), 
                                                  Probability(0.1), 
                                                  Probability(0.1), 
                                                  Probability(0.1)),
      states.id("Fair") ->   DiscreteDistribution(Probability(1.0/6), 
                                                  Probability(1.0/6), 
                                                  Probability(1.0/6), 
                                                  Probability(1.0/6), 
                                                  Probability(1.0/6), 
                                                  Probability(1.0/6))
    )
    val transitions = Map(
      states.id("Loaded") -> DiscreteDistribution(Probability(0.05), Probability(0.95)),
      states.id("Fair") -> DiscreteDistribution(Probability(0.1), Probability(0.90))
    )
    val x = symbols.generateSequeceOfIds(Stream("1", "2"))
    val y = states.generateSequeceOfIds(Stream("Fair", "Loaded"))
    val hmm = new HiddenMarkovModel(emissions, transitions)
    hmm.prob(x, y).expValue should be ((0.95*(1.0/6)*0.1*0.1) plusOrMinus 0.0001)
  }
}