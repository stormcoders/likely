package br.com.igorbonadio.likely

trait Distribution[T] {
  def prob(x: T): LogProbability
  def choose: T
}