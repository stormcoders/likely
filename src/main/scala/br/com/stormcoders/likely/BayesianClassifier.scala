package br.com.stormcoders.likely

class BayesianClassifier[T](prior: Map[String, LogProbability], models: Map[String, IIDModel[T]]) {
  def classify(sequence: Stream[T]) = {
    def classify2(m: List[(String, IIDModel[T])], prob: List[(String, LogProbability)]): List[(String, LogProbability)] = m match {
      case List() => prob
      case (name, model)::ms => classify2(ms, (name, model.prob(sequence) * prior(name))::prob)
    }
    classify2(models.toList, List()).reduceLeft{(a, b) => if (a._2 > b._2) a else b}
  }
}

object BayesianClassifier {
  def apply[T](prior: (String, LogProbability)*)(models: (String, IIDModel[T])*) = 
    new BayesianClassifier(prior.toMap, models.toMap)
}