package br.com.igorbonadio.likely

import util.Random

abstract class IIDModel[T](distribution: Distribution[T]) {
  def prob(sequence: Stream[T]): LogProbability = sequence match {
    case x #:: xs => distribution.prob(x) * prob(xs)
    case Stream() => Probability(1)
  }
  
  def choose: T

  def chooseStream: Stream[T] = choose #:: chooseStream
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
}

class ContinuousIIDModel(distribution: NormalDistribution) extends IIDModel(distribution) {
  def choose = {
    def boxMuller = {
      val uniform = new Random()
      val u1 = uniform.nextDouble
      val u2 = uniform.nextDouble
      val r = math.sqrt(-2*math.log(u1))
      math.sqrt(-2*math.log(u1))*math.cos(2*math.Pi*u2)
    }
    distribution.createMember(boxMuller)
  }
}