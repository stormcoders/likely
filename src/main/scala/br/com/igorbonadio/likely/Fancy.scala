package br.com.igorbonadio.likely

import scala.language.implicitConversions

object Fancy {
  implicit def DoubleToPercentage(value: Double) = new Percentage(value)

  class Percentage(value: Double) {
    def %% = Probability(value/100)
  }
}