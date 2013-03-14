package br.com.igorbonadio.likely

import util.Random

abstract class IIDModel[T](distribution: Distribution[T]) {
  def prob(sequence: List[T]):LogProbability = sequence match {
    case x::xs => distribution.prob(x) * prob(xs)
    case List() => Probability(1)
  }
  def choose: T
  def chooseSequence(size: Int): List[T]
}

class DiscreteIIDModel(distribution: DiscreteDistribution) extends IIDModel(distribution) {
  def choose = {
    def chooseWith(v: Double, prob: List[LogProbability], sym: Int): Int = prob match {
      case p::ps => if ((v - p.expValue) < 0) sym
              else chooseWith(v - p.expValue, ps, sym + 1)
      case List() => sym
    }
    chooseWith((new Random()).nextDouble, distribution.probabilities, 0)
  }
  
  def chooseSequence(size: Int) = 
    if (size > 0) choose :: chooseSequence(size-1)
    else List()
}

class ContinuousIIDModel(distribution: NormalDistribution) extends IIDModel(distribution) {
  def choose = 0.1
  def chooseSequence(size: Int) = List(1.1, 2.2)
}