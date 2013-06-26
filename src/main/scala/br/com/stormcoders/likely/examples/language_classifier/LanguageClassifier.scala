package br.com.stormcoders.likely.examples.language_classifier

import scala.io.Source

import br.com.stormcoders.likely._
import Fancy._

class LanguageClassifier {
  val alphabet = Alphabet('A' to 'Z')
  val classifier = BayesianClassifier(
    "portuguese" -> DiscreteIIDModel.train(
      alphabet.generateSequeceOfIds(
        justLetters(fileToString(full_path("pt.txt"))))),
    "english" -> DiscreteIIDModel.train(
      alphabet.generateSequeceOfIds(
        justLetters(fileToString(full_path("en.txt")))))
  )

  def classify(text: String) =
    classifier.classify(alphabet.generateSequeceOfIds(justLetters(text)))

  private def full_path(filename: String) =
    "src/main/scala/br/com/stormcoders/likely/examples/language_classifier/" + filename

  private def fileToString(filename: String) =
    Source.fromFile(filename).mkString

  private def justLetters(sequence: String) =
    sequence.toUpperCase.toList.filter(c => c.isLetter && c <= 'z').map(_.toString).toStream
}

object LanguageClassifier {
  def main(args: Array[String]) {
    val languageClassifier = new LanguageClassifier
    println("classification = " + languageClassifier.classify(args(0)))
  }
}