package br.com.igorbonadio.likely.examples.language_classifier

import scala.io.Source

import br.com.igorbonadio.likely._

class LanguageClassifier {
  // training set
  val pt = justLetters(fileToString("src/main/scala/br/com/igorbonadio/likely/examples/language_classifier/pt.txt"))
  val en = justLetters(fileToString("src/main/scala/br/com/igorbonadio/likely/examples/language_classifier/en.txt"))
  val alphabet = Alphabet('A' to 'Z')
  val ptModel = DiscreteIIDModel.train(alphabet.generateSequeceOfIds(pt.toStream))
  val enModel = DiscreteIIDModel.train(alphabet.generateSequeceOfIds(en.toStream))

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

  private def fileToString(filename: String) =
    Source.fromFile(filename).mkString

  private def justLetters(sequence: String) =
    sequence.toUpperCase.toList.filter(c => c.isLetter && c <= 'z').map(_.toString)
}

object LanguageClassifier {
  def main(args: Array[String]) {
    val classifier = new LanguageClassifier
    println("classification = " + classifier.classify(args(0), true))
  }
}