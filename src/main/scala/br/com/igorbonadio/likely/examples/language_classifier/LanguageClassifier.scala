package br.com.igorbonadio.likely.examples.language_classifier

import scala.io.Source

import br.com.igorbonadio.likely._

class LanguageClassifier {
  // training set
  val pt = justLetters(Source.fromFile("src/main/scala/br/com/igorbonadio/likely/examples/language_classifier/pt.txt").mkString)
  val en = justLetters(Source.fromFile("src/main/scala/br/com/igorbonadio/likely/examples/language_classifier/en.txt").mkString)
  val alphabet = new Alphabet(('A' to 'Z').toList.map(_.toString))
  val ptModel = DiscreteIIDModel.train(alphabet.generateSequeceOfIds(pt.toStream))
  val enModel = DiscreteIIDModel.train(alphabet.generateSequeceOfIds(en.toStream))

  def justLetters(sequence: String) =
    sequence.toUpperCase.toList.filter(c => c.isLetter && c <= 'z').map(_.toString)

  def classify(text: String, debug: Boolean) = {
    val test = justLetters(text)
    val ptProb = ptModel.prob(alphabet.generateSequeceOfIds(test.toStream))
    val enProb = enModel.prob(alphabet.generateSequeceOfIds(test.toStream))
    
    if (debug) {
      print("ptProb = ")
      println(ptProb)
      print("enProb = ")
      println(enProb)
    }

    if (ptProb > enProb) "pt" 
    else "en"
  }
}

object LanguageClassifier {
  def main(args: Array[String]) {
    val classifier = new LanguageClassifier
    println("classification = " + classifier.classify(args(0), true))
  }
}