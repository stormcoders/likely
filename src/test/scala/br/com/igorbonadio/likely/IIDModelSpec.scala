package br.com.igorbonadio.likely

import org.scalatest._
import org.scalatest.matchers._

class IIDModelSpec extends FlatSpec with ShouldMatchers {
  behavior of "An IID Model"
  
  it should "get the probability of a given sequence" in {
    val alphabet = new Alphabet(List("Loaded", "Fair"))
    val distribution = new DiscreteDistribution(List(Probability(0.8), Probability(0.2)))
    val model = new IIDModel(distribution)
    val sequence = alphabet.generateSequeceOfIds(List("Fair", "Loaded"))
    
    model.prob(sequence).expValue should be (0.16 plusOrMinus 0.0001)
  }
  
  it should "generate a random symbol" in {
    val alphabet = new Alphabet(List("Loaded", "Fair"))
    val distribution = new DiscreteDistribution(List(Probability(0.8), Probability(0.2)))
    val model = new IIDModel(distribution)
    
    model.choose should (be === 0 or be === 1)
  }
}