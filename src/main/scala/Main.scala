package main

object Main {
  def main(args: Array[String]): Unit = {
    println(args.toList)
    println(args.length)
    val argsList = args.toList

    val picked: Option[List[Int]] = getConfigIntListFromArgs(argsList, "picked")
    val smallRandom: Option[Int] = getConfigIntFromArgs(argsList, "smallRandom")
    val largeRandom: Option[Int] = getConfigIntFromArgs(argsList, "largeRandom")
    val target : Option[Int] = getConfigIntFromArgs(argsList, "target")

    val solutions: List[Calculation] = findSolutions()
    Countdown.formPrintableSolutions(solutions).foreach(println)
  }

  private def getConfigIntFromArgs(args: List[String], argKey: String): Option[Int] = {
    None
  }

  private def getConfigIntListFromArgs(args: List[String], argKey: String): Option[List[Int]] = {
    None
  }

  private def findSolutions(picked: Option[List[Int]] = None,
                            smallRandom: Option[Int] = None,
                            largeRandom: Option[Int] = None,
                            target : Option[Int] = None): List[Calculation] = {
    val largeNumbers = 1
    val smallNumbers = 6 - largeNumbers

    val largePicker = new NumberPicker((1 to 4).map(d => 25 * d).toList)
    val smallPicker = new NumberPicker((1 to 10).toList ++ (1 to 10).toList)

    val answerPicker = new NumberPicker((101 to 999).toList)

    val pickedNumbers = smallPicker.select(smallNumbers).sorted ++ largePicker.select(largeNumbers)
    println(pickedNumbers.mkString("Picked | ", ", ", ""))

    val targetNumber = answerPicker.select(1).head
    println(s"Target | $targetNumber")

    val solutions: List[Calculation] = Countdown.solve(pickedNumbers, targetNumber).solutions
    solutions
  }
}
