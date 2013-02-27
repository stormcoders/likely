class Alphabet(symbols: List[String]) {
  val alphabetMap = (symbols zip (0 to (symbols.length-1))).toMap
}