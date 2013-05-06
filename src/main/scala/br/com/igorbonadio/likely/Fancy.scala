package br.com.igorbonadio.likely

import scala.language.implicitConversions
import scala.collection.mutable.ListBuffer

import Fancy._

object Fancy {
  implicit def DoubleToPercentage(value: Double) = new Percentage(value)

  class Percentage(value: Double) {
    def %% = Probability(value/100)
  }
}

class ProbabilityOf(alphabet: Alphabet) {
  var probs: ListBuffer[ProbabilityValueOf] = new ListBuffer()
  def distribution = {
    var dist: Array[LogProbability] = new Array[LogProbability](alphabet.size)
    probs.foreach { p =>
      dist(alphabet.id(p.name)) = p.value
    }
    new DiscreteDistribution(dist.toList)
  }
  def apply(symbol: String) = {
    probs.prepend(new ProbabilityValueOf(symbol))
    probs.head
  }
}

class ProbabilityValueOf(symbol: String) {
  var value:LogProbability = 0.0.%%
  def name = symbol
  def is(v: LogProbability) = {
    value = v
  }
}

object ConditionalProbabilities {
  def apply(alphabet1: Alphabet, alphabet2: Alphabet)(definition: (ConditionalProbabilityOf => Unit)) = {
    val probs = new ConditionalProbabilityOf(alphabet1, alphabet2)
    definition(probs)
    probs.toMap
  }
}

class ConditionalProbabilityOf(alphabet1: Alphabet, alphabet2: Alphabet) {
  var probs: ListBuffer[ConditionalProbabilityValueOf] = new ListBuffer()

  def apply(symbol1: String, symbol2: String) = {
    probs.prepend(new ConditionalProbabilityValueOf(symbol1, symbol2))
    probs.head
  }

  def toMap = {
    (0 to alphabet2.size).map {index => (index -> distribution(index))}.toMap
  }

  def distribution(index: Int) = {
    var dist: Array[LogProbability] = new Array[LogProbability](alphabet1.size)
    probs.foreach { p =>
      if (alphabet2.id(p.symbol2) == index)
        dist(alphabet1.id(p.symbol1)) = p.value
    }
    new DiscreteDistribution(dist.toList)
  }
}

class ConditionalProbabilityValueOf(s1: String, s2: String) {
  var value:LogProbability = 0.0.%%
  def symbol1 = s1
  def symbol2 = s2
  def is(v: LogProbability) = {
    value = v
  }
}
