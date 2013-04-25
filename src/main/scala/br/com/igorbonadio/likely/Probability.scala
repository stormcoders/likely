package br.com.igorbonadio.likely

import scala.language.implicitConversions

class LogProbability(v: Double) {
  val logValue = v
  def expValue = math.exp(-logValue)
  
  def *(p: LogProbability):LogProbability =
    new LogProbability(logValue + p.logValue)
    
  def /(p: LogProbability):LogProbability =
    new LogProbability(logValue - p.logValue)
    
  def +(p: LogProbability):LogProbability =
    new LogProbability(logValue - math.log(1 + math.exp(logValue - p.logValue)))
    
  def -(p: LogProbability):LogProbability =
    new LogProbability(logValue - math.log(1 - math.exp(logValue - p.logValue)))

  def ==(p: LogProbability): Boolean =
    logValue == p.logValue

  def >(p: LogProbability): Boolean =
    logValue < p.logValue

  def >=(p: LogProbability): Boolean =
    logValue <= p.logValue

  def <(p: LogProbability): Boolean =
    logValue > p.logValue

  def <=(p: LogProbability): Boolean =
    logValue >= p.logValue
    
  override def toString:String = expValue.toString
}

object Probability {
  def apply(v: Double) = new LogProbability(-math.log(v))
}

class Percentage(value: Double) {
  def %% = Probability(value/100)
}

object PercentageImplicits {
  implicit def DoubleToPercentage(value: Double) = new Percentage(value)
}
