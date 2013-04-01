package br.com.igorbonadio.likely

import util.Random

abstract class IIDModel[T](distribution: Distribution[T]) {
  def prob(sequence: Stream[T]): LogProbability = sequence match {
    case x #:: xs => distribution.prob(x) * prob(xs)
    case Stream() => Probability(1)
  }

  def chooseStream: Stream[T] = distribution.choose #:: chooseStream
}

class DiscreteIIDModel(distribution: DiscreteDistribution) extends IIDModel(distribution)
class ContinuousIIDModel(distribution: NormalDistribution) extends IIDModel(distribution)