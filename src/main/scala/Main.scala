package main

object Main {
  def main(args: Array[String]): Unit = {
    val argsList = args.toList

    val picked: Option[List[Int]] = getConfigIntListFromArgs(argsList, "picked")
    val smallRandom: Option[Int] = getConfigIntFromArgs(argsList, "smallRandom")
    val largeRandom: Option[Int] = getConfigIntFromArgs(argsList, "largeRandom")
    val target : Option[Int] = getConfigIntFromArgs(argsList, "target")

    val solutions: List[Calculation] = findSolutions(picked, smallRandom, largeRandom, target)
    Countdown.formPrintableSolutions(solutions).foreach(println)
  }

  private def getConfigIntFromArgs(args: List[String], argKey: String): Option[Int] = {
    args.indexOf(argKey) match {
      case -1 => None
      case argKeyIndex =>
        val argValue = args.splitAt(argKeyIndex + 1)._2.head
        if (argValue.nonEmpty) Some(argValue.toInt) else None
    }
  }

  private def getConfigIntListFromArgs(args: List[String], argKey: String): Option[List[Int]] = {
    args.indexOf(argKey) match {
      case -1 => None
      case argKeyIndex =>
        val argValue = args.splitAt(argKeyIndex + 1)._2.head
        if (argValue.nonEmpty) Some(argValue.split(",").map(_.toInt).toList) else None
    }
  }

  private def findSolutions(picked: Option[List[Int]] = None,
                            smallRandom: Option[Int] = None,
                            largeRandom: Option[Int] = None,
                            target : Option[Int] = None): List[Calculation] = {
    def getPickedNumbers: List[Int] = {
      picked match {
        case Some(intList) => intList
        case None =>
          val largeNumbers = largeRandom.getOrElse(1)
          val smallNumbers = smallRandom.getOrElse(6 - largeNumbers)

          val largePicker = new NumberPicker((1 to 4).map(d => 25 * d).toList)
          val smallPicker = new NumberPicker((1 to 10).toList ++ (1 to 10).toList)

          val pickedNumbers = smallPicker.select(smallNumbers).sorted ++ largePicker.select(largeNumbers)
          pickedNumbers
      }
    }

    def getTargetNumber: Int = {
      target match {
        case Some(value) => value
        case None =>
          val answerPicker = new NumberPicker((101 to 999).toList)
          val targetNumber = answerPicker.select(1).head
          targetNumber
      }
    }

    val pickedNumbers = getPickedNumbers
    println(pickedNumbers.mkString("Picked | ", ", ", ""))

    val targetNumber = getTargetNumber
    println(s"Target | $targetNumber")

    val solutions: List[Calculation] = Countdown.solve(pickedNumbers, targetNumber).solutions
    solutions
  }
}
